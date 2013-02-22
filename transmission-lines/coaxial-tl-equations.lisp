;;; coaxial transmission line
;;; unit test values were obtained from:
;;; http://www.cvel.clemson.edu/emc/calculators/TL_Calculator/index.html

(in-package :cx-tl)

(defun z-0 (epsilon-r d-inner d-outer)
  "Impedance of a coaxial transmission line as function of the
dielectric constant, and inner and outer diameter"
  (* (/ +z-v+
	(sqrt epsilon-r)) (log (/ d-outer d-inner))))


(define-test z-0
  (assert-equal 65.87 (z-0 1.0 1.0 3.0)))

(defun C/h (epsilon-r d-inner d-outer)
"Capacitance/unit-length of a co-axial transmission line

epsilon-r - relative dielectric constant
d-inner, d-outer - inner and outer diameter"  
  (/ (* 2 pi 8.85e-12 epsilon-r)
     (log (/ d-outer d-inner))))



(define-test c/h
  (assert-equal 5.06e-11 (c/h 1.0 1.0 3.0)))


(defun R/h (d-inner d-outer sigma f)
  (/ (* (* (* 2 pi (+ d-inner)
	      d-outer)
	   (delta sigma f))
	   sigma)
	   ))

(defun L/h (d-inner d-outer)
"Inductance/unit-length of a co-axial transmission line

d-inner, d-outer - inner and outer diameter"  
  (* (/ (* 4e-7 pi)
	(* 2 pi))
     (log (/ d-outer
	     d-inner))))

(define-test l/h
  (assert-equal 2.20e-7 (l/h 1.0 3.0)))


(defun d-outer (z-0 d-inner &optional (epsilon-r 1.0))
  (* d-inner (exp (/ z-0 (/ +z-v+
	(sqrt epsilon-r))))))

(defun d-inner (z-0 d-outer &optional (epsilon-r 1.0))
  (/ d-outer (exp (/ z-0 (/ +z-v+
	(sqrt epsilon-r))))))

