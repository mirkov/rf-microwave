(in-package #:general.transmission-lines)

;;;; Characteristic impedance

(defun z-0 (L/h C/h &optional (f 1.0) (R/h 0.0) (G/h 0.0))
  "Transmission line impedance as function of inductance/unit-length
and capacitance/unit-length"
  (let ((z-0 (sqrt (/ (complex R/h (* +2pi+ f L/h))
		      (complex G/h  (* +2pi+ f C/h))))))
    (if (zerop (imagpart z-0))
	(realpart z-0)
	z-0)))



(defun z-0/ideal (L/h C/h)
  "Impedance of a loss-less transmission line"
  (sqrt (/ L/h C/h)))

(defun z-0/lossy (L/h C/h &optional (R/h 0.0) (G/h 0.0))
  "Impedance of a lossy transmission line"
  (sqrt (/ (complex R/h L/h)
	   (complex G/h C/h))))


;;; Impedance transformations

(defun z-in-1 (z-l/rel beta-l)
  "Transformation of relative impedance z-l/rel (normalized to the line
impedance Z-0) along length 2 pi l/lambda"
  (let ((tan-beta-l (tan beta-l)))
    (/ (+ z-l/rel (i tan-beta-l))
       (+ 1.0 (i (* z-l/rel tan-beta-l))))))

(defun z-in (z-l z-0 beta l)
  "Transformation of Z-L on a transmission line of Z-0

beta = 2pi/lambda
l = length

Returns Z-in, |Z-in|, and Gamma (Z-in/Z-0) "
  (let* ((beta-l (* beta l))
	 (z-in (* z-0 (z-in-1 (/ z-l z-0) beta-l))))
    (values z-in (abs z-in) (gamma (/ z-in z-0)))))


(defun Z-in-chain (Z-l &rest segment-specifications)
  "Return the imput impedance of a chain of transmission lines terminated in Z-l load.

SEGMENT-SPECIFICATION is a three element list consisting of Z-0, beta,
l for each transmission line segment"
  (reduce (lambda (spec Z-l)
	    (destructuring-bind (Z-0 beta l) spec
	      (Z-in Z-l Z-0 beta l))) segment-specifications
	      :initial-value Z-l
	      :from-end t))

(defun Im-z-in (z-l z-0 beta l)
  "Approximate value of Im Z-in valid when beta * l is small and z-l
and z-0 are of similar order of magnitude"
  (* Z-l
     (- (/ z-0 z-l)
	(/ z-l z-0))
     beta l))

(defun Im-Z-in-chain (Z-l &rest segment-specifications)
  "Approximate imaginary component of the input impedance of a chain
of transmission line segments.

SEGMENT-SPECIFICATION is a three element list consisting of Z-0, beta,
l for each transmission line segment.

The calculation uses Im-z-in, and thus assumes that beta * l is small
for each segment and that Z-0 of each segment is of the same order of
magnitude as Z-l.
"
  (* Z-l
     (reduce #'+
	     (mapcar (lambda (spec)
		       (destructuring-bind (Z-0 beta l) spec
			 (/ (Im-z-in Z-l Z-0 beta l)
			    Z-l))) segment-specifications)
	     :initial-value 0.0
	     :from-end t)))




(defun gamma (z-rel)
  "Reflection coefficient as function of normalized impedance Z-t"
  (/ (- z-rel 1.0)
     (+ z-rel 1.0)))

(defun z-rel (gamma)
  "Normalized impedance as function of gamma"
  (/ (+ 1.0 gamma)
     (- 1.0 gamma)))


(defun k (L/h C/h &optional (f 1.0) (R/h 0.0) (G/h 0.0))
  (error "Deprecated.  Consider using z-0/lossy")
  (let ((z-0 (sqrt (* (complex R/h (* f L/h))
		      (complex G/h  (* f C/h))))))
    (if (zerop (imagpart z-0))
	(realpart z-0)
	z-0)))

