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

(defun random-gradient (ix iy)
  (let* ((w (* 8 4))
	 (s (/ w 2))
	 (a ix)
	 (b iy))
    (setf a (* a 3284157443))
    (setf b (logxor b (logior (ash a s)
			      (ash a (- s w)))))

    (setf b (* b 1911520717))
    (setf a (logxor a (logior (ash b s)
			      (ash b (- s w)))))

    (setf a (* a 2048419325))

    (let ((rand (* a (/ pi (lognot #x7FFFFFFF)))))
      (vec (cos rand)
	   (sin rand)))))

(defun dot-grid-gradient (ix iy x y)
  (let ((gradient (random-gradient ix iy))
	(dx (- x (float ix)))
	(dy (- y (float iy))))
    (+ (* dx (vx gradient))
       (* dy (vy gradient)))))

;;; FIXME: Doesn't work. Either this function or the above ones
(defun perlin (x y)
  (let* ((x0 (floor x))
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
    (lerp ix0 ix1 sy)))
