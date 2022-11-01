(in-package :game-engine)

(defun lerp (a b w &key (clamp-p t) (step :normal))
  "Linearly interpolate between a and b. Weight w should be in [0.0, 1.0]."
  (let ((res (ecase step
	       (:normal (+ a (* (- b a) w)))
	       (:smooth (+ a (* (- b a)
				(- 3.0 (* w 2.0))
				w
				w))))))
    (if clamp-p
	(clamp res a b)
	res)))

(defun trunc-u32 (i)
  (logand i #xFFFFFFFF))

(defun random-gradient (vector &optional (seed 0))
  (with-vec (x y z) vector
    (let ((octets (make-array 32 :element-type '(unsigned-byte 8)))
	  (x (floor x))
	  (y (floor y))
	  (z (floor z)))
      (loop for i from 0 below 8
	    do (progn
		 (setf (aref octets (+ (* i 3) 0)) (ldb (byte 8 (* i 8)) x))
		 (setf (aref octets (+ (* i 3) 1)) (ldb (byte 8 (* i 8)) y))
		 (setf (aref octets (+ (* i 3) 2)) (ldb (byte 8 (* i 8)) z))
		 (setf (aref octets (+ (* i 3) 3)) (ldb (byte 8 (* i 8)) seed))))
      (let ((hash (mod (city-hash:city-hash-32 octets) 36500)))
	(vec (cos (radians (/ hash 100)))
	     (sin (radians (/ hash 100))))))))

(defun dot-grid-gradient (ix iy x y)
  (let ((gradient (random-gradient ix iy))
	(dx (- x (float ix)))
	(dy (- y (float iy))))
    (+ (* dx (vx gradient))
       (* dy (vy gradient)))))

;;; FIXME: Doesn't work. Either this function or the above ones
(defun perlin (x y &key (seed 0) (octave 0) (amplitude 0))
  (let* ((x (* x (expt 2 octave)))
	 (y (* y (expt 2 octave)))
	 (x0 (floor x))
	 (x1 (1+ x0))
	 (y0 (floor y))
	 (y1 (1+ y0))

	 (sx (- x (float x0)))
	 (sy (- y (float y0)))

	 (n0 (dot-grid-gradient x0 y0 x y))
	 (n1 (dot-grid-gradient x1 y0 x y))
	 (ix0 (lerp n0 n1 sx))

	 (n0 (dot-grid-gradient x0 y1 x y))
	 (n1 (dot-grid-gradient x1 y1 x y))
	 (ix1 (lerp n0 n1 sx)))
    (* (lerp ix0 ix1 sy)
       (/ 1 (expt 2 amplitude)))))
