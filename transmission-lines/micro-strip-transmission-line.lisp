(in-package #:micro-strip.transmission-lines)

(defun C/h (epsilon-r w h)
"Capacitance/unit-length of a co-axial transmission line


EPSILON-R - relative dielectric constant
W - width
H - dielectric layer thickness"
  (* (* +epsilon-0+ epsilon-r) (/ w h)))


(defun L/h (w h)
"Inductance/unit-length of a co-axial transmission line

W - width
H - dielectric layer thickness"
  (* +mu-0+ (/ h w)))

(defun z-0 (epsilon-r w h)
  "Impedance of a coaxial transmission line as function of the

EPSILON-R - relative dielectric constant
W - width
H - dielectric layer thickness"
  (tl:z-0/ideal (l/h w h)
       (c/h epsilon-r w h)))