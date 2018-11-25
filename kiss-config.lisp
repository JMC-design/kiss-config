(defpackage :kiss.config
  (:use :cl :split-sequence)
  (:export #:cel
	   #:cel-meta
	   #:cel-set
	   #:object
	   #:config
	   #:process-raw-config
	   #:cel->image
	   #:palettes))
(in-package :kiss.config)

;;;; READ BIT UTILS

(defun read-n-bytes (stream pos bytes)
  (file-position stream pos)
  (loop :for x :from 1 :to bytes
     :collect (read-byte stream)))

(defun read-word (stream pos)
  (file-position stream pos)
  (let ((low-bit (read-byte stream))
	(high-bit (read-byte stream)))
    (+ low-bit (ash high-bit 8))))

;;;; Stream Readers
(defun parse-xy (stream)
  (destructuring-bind (a b)
      (split-sequence:split-sequence #\, (read-line stream nil))
    `(,(parse-integer a) . ,(parse-integer b :junk-allowed t))))

(defun parse-command (stream)
  (read-line stream nil nil))
;; The problem with old style lha is they uppercase characters. Shouldn't make this
;; machinery exploit this flaw by using read this way.

;;;; Config processing

(defstruct config
  raw
  border
  comments
  palettes
  playfield
  cels
  commands
  sets
  name
  location)

(defun process-raw-config (pathname)
  "Takes a raw FKISS CNF file and returns a CONFIG STRUCT."
  (with-open-file (stream pathname)
    (let ((name (pathname-name pathname))
	  (location (make-pathname :directory (pathname-directory pathname)))
	  (commands)
	  (comments)
	  (border)
	  (playfield)
	  (cels)
	  (palettes)
	  (sets))
      (loop :for char := (read-char stream nil nil)
	 :while char ; do we have a character to test?
	 :while (not (eql :eof (peek-char nil stream nil :eof)))
	         ;for people who put semi-colons followed by notbing
	 :do
	 (progn (case char 
		  (#\; (if (char= #\@ (peek-char nil stream nil))
			   (push (parse-command stream) commands)                        ; commands
			   (push (read-line stream nil) comments)))                      ; comments
		  (#\# (push (parse-cel stream) cels))                                   ; cels
		  (#\%  (push (remove #\return (read-line stream)) palettes))		 ; palettes  
		  (#\[ (setf border (parse-integer (read-line stream) :junk-allowed t))) ; border
		  (#\( (setf playfield (parse-xy stream)))                               ; playfield   
		  (#\$ (push (read-set stream) sets))	                                 ; sets 
		  ((t) (return))
		  (nil (return)))))
      (make-config :raw pathname :border border :comments comments :palettes palettes
		   :playfield playfield :cels cels :commands (reverse commands) :sets (reverse sets)
		   :name name :location location))))

(defstruct cel-meta
  id
  fix
  name
  pen-group
  in-sets
  alpha)

;;fixme: parse beyond the name
(defun parse-cel (stream)
  (let ((meta (make-cel-meta  )))
    (progn
      (multiple-value-bind (i f) (floor (read stream))
	(setf (cel-meta-id meta) i
	      (cel-meta-fix meta) (if (= 0 f)nil (floor (* f 10000)))))
      (setf (cel-meta-name meta) (symbol-name (read-preserving-whitespace stream)));(print (peek-char nil stream))
      (when (char= #\newline (peek-char nil stream))
	(return-from parse-cel meta))
      (let ((rest (read-line stream nil nil)))
	(when (and rest (not (string= #\newline rest)))	  ;(format t "rest:~a" rest)
	  (with-input-from-string (in rest)
	    (loop
	       (let ((next (read-char in nil :eof)))
		 (when next   ;   (format t "next:~S" next)
		   (case next 
		     (#\* (setf (cel-meta-pen-group meta) (read in))) ;palette pen group
		     (#\: (print "sidestep")) ;;  (do ((int (read in nil) (read in nil))
		     ;;    (result '()))			     
		     ;;   ((char= (peek-char nil in) #\newline) (reverse result))
		     ;; (push int result)))
					;visible set
			      
		     (#\;)		;comment or transparency

		     ((#\newline nil) (return meta))
		     (t (return))))))))
	meta))))


;;; SET processing

(defstruct cel-set
  palette-group
  locations)		      ; vector of (X . Y) co-ordinates
  
(defmethod print-object ((set cel-set)stream)
  (if *print-readably*
      (call-next-method)
      (format stream "palette:~a Locations:~a"
	      (cel-set-palette-group set) (length (cel-set-locations set)))))

(defun read-set (stream)
  (let ((group (read stream))
	(bag (collect-coords stream)))
    (let ((vec (make-array (length bag))))
      (loop :for coords :in bag
	 :for pos :from 0
	 :do  (setf (aref vec pos) coords))
      (make-cel-set :palette-group group :locations vec))))

(defun collect-coords (stream)
  (let ((result '()))
    (loop :do
       (let ((line (split-sequence:split-sequence #\space (remove #\return (read-line stream nil)) :remove-empty-subseqs t)))
	 (if (> (length (car line)) 0) ; LINE will be empty string in a list on eof
	     (dolist (coord line)	; (print coord)
	       (push (with-input-from-string (in coord)
		       (let ((input (peek-char nil in nil :eof)))
			 (case input
			   ((:eof  #\; #\$)(return-from collect-coords (reverse result)))  ;fix?
			   (#\* (read-char in nil nil))
			   (t (parse-xy in)))))
		     result))
	     (return-from nil) )))
    (reverse result)))

;;;; CELS

(defstruct cel
  type
  bpp
  width
  height
  x-offset
  y-offset 
  data
  meta
  plist)

(defmethod print-object ((cel cel) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "~a ~a ~a ~a" (cel-meta-id (cel-meta cel)) (cel-meta-name (cel-meta cel)) (cel-width cel) (cel-height cel)))  )

(defun cel-file? (stream)
  (let ((kiss-id '(#x4b #x69 #x53 #x53))
	(id (read-n-bytes stream 0 4) ))
    (equal kiss-id id)))

(defun read-cel-data (cel stream)
  "Returns pixel data from cel."
  (let* ((width (cel-width cel))
	 (height (cel-height cel))
	 (int-array (make-array `(,height ,width ) :element-type '(unsigned-byte 24))))
    (file-position stream 32)
    (loop :for i :from 0 :below height :do
       (loop :for j :from 0 :below  width :do
	  (setf (aref int-array i j ) (read-byte stream))))
    int-array))
  
(defun load-cel (filename meta)
  (let ((cel (make-cel)))
    (with-open-file (stream filename :element-type '(unsigned-byte 8))
      (when (cel-file? stream)	
	(setf (cel-type cel) (car (read-n-bytes stream 4 1))) ;#x20 palette based #x21 32bit bgra
	(setf (cel-bpp cel) (car (read-n-bytes stream 5 1)))
	(setf (cel-width cel) (read-word stream 8))
	(setf (cel-height cel) (read-word stream 10))
	(setf (cel-x-offset cel)(read-word stream 12))
	(setf (cel-y-offset cel)(read-word stream 14))
	(setf (cel-data cel) (read-cel-data cel stream))
	(setf (cel-meta cel) meta)
	cel))))

(defun process-cels (config)
  "Returns CEL STRUCTS for each cel in config."
  (let ((dir (config-location config)))
    (loop for cel in (config-cels config)
       :collect (load-cel (merge-pathnames (cel-meta-name cel) dir) cel))))


;;;; Palettes

(defstruct palettes 
  bpc
  number-colours
  count
  palettes)

; who so fucking many palettes? What is a set of palettes called?
 (defun process-palettes (palette)
   (setf (palettes-palettes palette)
	 (if (= 24 (palettes-bpc palette))
	     (loop :for palette :in (palettes-palettes palette)
		   :collect (map 'vector #'identity
				 (loop :for x :from 0 :upto (* 3  #xff) :by 3
				       :collect (+ (ash (aref palette x) 16)
						   (ash (aref palette  (+ x 1)) 8)
						   (aref palette (+ x 2)))))))))
(defun load-palette (filename)
  "loads and returns a palette from filename" ;fixme check for old ;format
  (let ((palette (make-palettes)))
    (with-open-file (stream filename :element-type '(unsigned-byte 8))
      (setf (palettes-bpc palette) (car (read-n-bytes stream 5 1)))
      (setf (palettes-number-colours palette) (read-word stream 8))
      (setf (palettes-count palette) (read-word stream 10))
      (setf (palettes-palettes palette) ; peter piper pic...
	    (let ((bytes-to-read (if (= (palettes-bpc palette) 24) 3 2)))
	      (loop :for x :from 0 :below (palettes-count palette)
		 :for index :from 32 :by (* bytes-to-read (+ 1 #xff))
		 :collect (let ((array (make-array (* bytes-to-read (palettes-number-colours palette)))))
			    (file-position stream index)
			    (read-sequence array stream)
			    array)))))
    (process-palettes palette)
    palette))



;; X11 image stuff
(defun cel->image (cel palette )
  "Takes a cel and returns an Ximage made with given palette."
  (let* ((raw-data (cel-data cel))
	 (data (make-array (array-dimensions raw-data) :element-type '(unsigned-byte 32))))
    (loop for x from 0 :below (apply #'* (array-dimensions raw-data))
       :do (let* ((pixel  (row-major-aref raw-data x))
		  (pal-colour (row-major-aref palette pixel)))
	     (setf (row-major-aref data x) pal-colour)))
    (xlib:create-image :width  (cel-width cel)
		       :height (cel-height cel)
		       :depth 24
		       :bits-per-pixel 32
		       :data data)))
