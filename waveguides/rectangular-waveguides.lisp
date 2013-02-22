(in-package :rectangular-waveguides)


(defun k-0 (f &optional (epsilon-r 1.0) (mu-r 1.0))
  "Free space propagation constant

Harrington, 1-71, 2-2"
  (sqrt (* (expt (/ (* 2 pi f)
		    +c+)
		 2)
	   epsilon-r mu-r)))

(define-test k-0
  (assert-true (/ (* 2 pi)
		  (k-0 1e9))))

(defun k-c (a m b n)
  "Waveguide cut-off propagation constant

Harrington, 4-22"
  (* pi (sqrt (+ (expt (/ m a) 2)
		 (expt (/ n b) 2)))))

(defun f-c (a m b n  &optional (epsilon-r 1.0) (mu-r 1.0))
  "Harrington, 4-24"
  (* (/ +c+ (* 2 (sqrt (* epsilon-r mu-r))))
     (sqrt (+ (expt (/ m a) 2)
	      (expt (/ n b) 2)))))


(defun lambda-g (lambda-0 f-c f)
  "Guide wavelength

Harrington Table 2-4"
  (/ lambda-0
     (sqrt (- 1.0 (expt (/ f-c f)
			2)))))