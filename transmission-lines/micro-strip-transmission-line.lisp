;;; micro-strip transmission line equations
;;; unit test values were obtained from:
;;; http://www.cvel.clemson.edu/emc/calculators/TL_Calculator/index.html


(in-package :mstl)

(defun C/h (epsilon-r w h)
"Capacitance/unit-length of a micro-strip transmission line

epsilon-r - relative dielectric constant
w - width
h - height

This formula is valid for w>>h
"
  (* (* 8.85e-12 epsilon-r) (/ w h)))

(define-test c/h
  (assert-equal 8.85e-9 (C/h 1.0 1.0 1e-3)))

(defun L/h (w h)
"Inductance/unit-length of a micro-strip transmission line

w - width
h - height

This formula is valid for w>>h
"
  (* (* 4e-7 pi) (/ h w)))

(define-test l/h
  (assert-equal 1.257e-9 (l/h 1.0 0.001)))

(defun z-0 (epsilon-r w h)
  "Impedance of a micro-strip transmission line as function of the
dielectric constant, width and height

w - width
h - height

This formula is valid for w>>h
"
  (tl:z-0 (l/h w h)
       (c/h epsilon-r w h)))

(define-test z-0
  (assert-equal 0.377 (z-0 1.0 1.0 1e-3)))