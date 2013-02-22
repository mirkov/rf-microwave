;;;; transmission-line-transformations.asd

(asdf:defsystem #:transmission-lines
  :serial t
  :description "Formulas for design and analysis of transmission lines"
  :author "Mirko Vukovic <your.name@example.com>"
  :license "Specify license here"
  :depends-on ("lisp-unit"
               "alexandria"
	       "physics-constants")
  :components
  ((:module "setup"
	    :components
	    ((:file "transmission-line-package-def")
	     (:file "init")))
   (:module "transmission-lines"
	    :components
	    ((:file "general-equations")
	     (:file "coaxial-tl-equations")
	     (:file "micro-strip-transmission-line")))
   (:module "waveguides"
	    ((:file "rectangular-waveguides")))
   (:module "cavities")))

