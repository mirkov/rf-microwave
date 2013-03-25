(in-package :material-props.rfmw)


(defun loss-angle (loss-factor capacivity)
  "Dielectric loss angle DELTA as function of epsilon' (capacivity) and epsilon''
 (loss-factor).

The dielectric constant is epsilon' - j * epsilon''

Harrington 1-74"
  (atan loss-factor capacivity))

(defun skin-depth (sigma f)
  "Skin depth as function of conductivity and frequency"
  (/ (sqrt (* pi f +mu-0+ sigma))))



