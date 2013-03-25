(in-package :constants.rfmw)

(defconstant +epsilon-0+ physics-constants:+free-space-permittivity-sp+)
(defconstant +mu-0+ physics-constants:+free-space-permeability-sp+)
(defconstant +Z-v+ physics-constants:+impedance-of-vacuum+)
(defconstant +c+ physics-constants:+speed-of-light-sp+)
(defconstant +2pi+ (* 2.0 pi))
(defconstant +pi/2+ (/ pi 2.0))
(defconstant +i+ (complex 0.0 1.0))

(defun i (arg)
  "Convert float to a complex with real part 0"
  (complex 0.0 arg))