(defpackage #:transmission-lines
  (:nicknames :tl)
  (:use #:cl #:lisp-unit)
  (:documentation "General transmission line formulas")
  (:export :Z-t-in
	   :Z-in
	   :Z-0
	   :gamma
	   :z-t
	   :delta
	   :k))

(defpackage #:coaxial-transmission-lines
  (:nicknames :cx-tl :cxtl)
  (:use #:cl #:lisp-unit)
  (:import-from :tl
		:+epsilon-0+
		:+mu-0+
		:+Z-v+
		:delta)
  (:documentation "Equations for the co-axial transmission line")
  (:export :C/h
	   :L/h
	   :R/h
	   :Z-0
	   :d-inner
	   :d-outer
	   ))

(defpackage #:micro-strip-transmission-lines
  (:nicknames :mstl)
  (:use #:cl #:lisp-unit)
  (:import-from :tl
		:+epsilon-0+
		:+mu-0+
		:+Z-v+)
  (:documentation "Equations for the micro-strip transmission line")
  (:export :C/h
	   :L/h
	   :Z-0))

(defpackage #:rectangular-waveguides
  (:nicknames :rwg)
  (:use #:cl #:lisp-unit)
  (:import-from :tl
		:+epsilon-0+
		:+mu-0+
		:+Z-v+
		:+c+)
  (:documentation "Equations for rectangular waveguides"))
