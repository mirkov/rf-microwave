;;; General packages

(defpackage #:constants.rfmw
  (:nicknames #:constants)
  (:use #:cl)
  (:documentation "Constants used and exported by the RF/MW package")
  (:export :+epsilon-0+ :+mu-0+ :+Z-v+ :+c+ :+2pi+ :+i+
	   :i))

(defpackage #:material-props.rfmw
  (:nicknames :material-props)
  (:use #:cl #:lisp-unit #:constants.rfmw)
  (:documentation "Material properties")
  (:export :loss-tangent
	   :skin-depth))

;;; Transmission line packages
(defpackage #:general.transmission-lines
  (:nicknames :tl)
  (:use #:cl #:lisp-unit #:constants.rfmw)
  (:documentation "General transmission line formulas")
  (:export :Z-t-in
	   :Z-in
	   :Z-0 :Z-0/ideal :Z-0/lossy
	   :gamma
	   :z-t))

(defpackage #:coaxial.transmission-lines
  (:nicknames :cx-tl :cxtl)
  (:use #:cl #:lisp-unit #:constants.rfmw)
  (:documentation "Equations for the co-axial transmission line")
  (:export :C/h
	   :L/h
	   :R/h
	   :Z-0
	   :d-inner
	   :d-outer
	   ))

(defpackage #:micro-strip.transmission-lines
  (:nicknames :mstl :ms-tl)
  (:use #:cl #:lisp-unit #:constants.rfmw)
  (:documentation "Equations for the micro-strip transmission line")
  (:export :C/h
	   :L/h
	   :Z-0))


;;; Waveguide packages
(defpackage #:general.waveguides
  (:use #:cl #:lisp-unit #:constants.rfmw))

(defpackage #:rectangular.waveguides
  (:nicknames :rwg)
  (:use #:cl #:lisp-unit #:constants.rfmw)
  (:documentation "Equations for rectangular waveguides"))
