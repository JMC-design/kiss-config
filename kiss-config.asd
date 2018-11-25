(in-package :asdf-user)
(defsystem "kiss.config"
  :description "KISS CNF loader"
  :version "0.0.1"
  :author "Johannes Martinez Calzada"
					;:licence
  :depends-on ("split-sequence")
  :components ((:file "kiss.config")))
