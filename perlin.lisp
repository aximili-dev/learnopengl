(in-package :perlin)

(defun degrees (radians)
  (/ (* radians 180) pi))

(defun radians (degrees)
  (/ (* degrees pi) 180))

(defun clamp (value floor ceiling)
  (min (max value floor) ceiling))

;; https://en.wikipedia.org/wiki/Smoothstep
(defun smoothstep (x)
  (* x
     x
     (- 3.0 (* 2.0 x))))

(defun lerp (a b w &key (step :smooth))
  "Linearly interpolate between a and b. Weight w should be in [0.0, 1.0]."
  (let ((res (ecase step
	       (:normal (+ a (* (smoothstep (clamp w 0 1))
				(- b a))))
	       (:smooth (+ a (* (- b a)
				(- 3.0 (* w 2.0))
				w
				w))))))
    res))

(defun trunc-u32 (i)
  (logand i #xFFFFFFFF))

;; https://mathworld.wolfram.com/SpherePointPicking.html
(defun random-gradient (x y z &optional (seed 0))
  (let ((octets (make-array 32 :element-type '(unsigned-byte 8)))
	(x (floor x))
	(y (floor y))
	(z (floor z)))
    (loop for i from 0 below 8
	  do (progn
	       (setf (aref octets (+ (* i 4) 0)) (ldb (byte 8 (* i 8)) x))
	       (setf (aref octets (+ (* i 4) 1)) (ldb (byte 8 (* i 8)) y))
	       (setf (aref octets (+ (* i 4) 2)) (ldb (byte 8 (* i 8)) z))
	       (setf (aref octets (+ (* i 4) 3)) (ldb (byte 8 (* i 8)) seed))))
    (let* ((hash (city-hash:city-hash-64 octets))
	   (hash-low (ldb (byte 32 0) hash))
	   (hash-high (ldb (byte 32 32) hash))
	   (u (/ hash-low  #xFFFFFFFF))
	   (v (/ hash-high #xFFFFFFFF))
	   (theta (* 2 pi u))
	   (phi (acos (- (* 2 v) 1)))
	   (u (cos phi)))
      (vunit (vec (* (sqrt (- 1 (* u u))) (cos theta))
		  (* (sqrt (- 1 (* u u))) (sin theta))
		  u)))))

(defun random-gradient-bad (vector &optional (seed 0))
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
(defun perlin (vector &key (seed 0))
  (with-vec (x y z) vector
    (let* ((xi (floor x))
	   (yi (floor y))
	   (zi (floor z))
	   (xf (- x xi))
	   (yf (- y yi))
	   (zf (- z zi))
	   (aaa-gradient (random-gradient     xi      yi      zi  seed))
	   (aab-gradient (random-gradient     xi      yi  (1+ zi) seed))
	   (aba-gradient (random-gradient     xi  (1+ yi)     zi  seed))
	   (abb-gradient (random-gradient     xi  (1+ yi) (1+ zi) seed))
	   (baa-gradient (random-gradient (1+ xi)     yi      zi  seed))
	   (bab-gradient (random-gradient (1+ xi)     yi  (1+ zi) seed))
	   (bba-gradient (random-gradient (1+ xi) (1+ yi)     zi  seed))
	   (bbb-gradient (random-gradient (1+ xi) (1+ yi) (1+ zi) seed))
	   (aaa-offset (vec     xf      yf      zf))
	   (aab-offset (vec     xf      yf  (1- zf)))
	   (aba-offset (vec     xf  (1- yf)     zf)) 
	   (abb-offset (vec     xf  (1- yf) (1- zf)))
	   (baa-offset (vec (1- xf)     yf      zf)) 
	   (bab-offset (vec (1- xf)     yf  (1- zf)))
	   (bba-offset (vec (1- xf) (1- yf)     zf)) 
	   (bbb-offset (vec (1- xf) (1- yf) (1- zf)))
	   (aaa (v. aaa-gradient aaa-offset))
	   (aab (v. aab-gradient aab-offset))
	   (aba (v. aba-gradient aba-offset))
	   (abb (v. abb-gradient abb-offset))
	   (baa (v. baa-gradient baa-offset))
	   (bab (v. bab-gradient bab-offset))
	   (bba (v. bba-gradient bba-offset))
	   (bbb (v. bbb-gradient bbb-offset))
	   (z1 (lerp aaa aab zf))
	   (z2 (lerp aba abb zf))
	   (z3 (lerp baa bab zf))
	   (z4 (lerp bba bbb zf))
	   (y1 (lerp z1 z2 yf))
	   (y2 (lerp z3 z4 yf))
	   (result (lerp y1 y2 xf)))
      result)))

(def-suite perlin
  :description "Test perlin functions")

(in-suite perlin)

(test lerp-0
  (let ((result (lerp 1 2 0)))
    (is (= 1 result))))

(test lerp-1
  (let ((result (lerp 1 2 1)))
    (is (= 2 result))))

(test lerp-0.5
  (let ((result (lerp 1 2 0.5)))
    (is (= 1.5 result))))

(test lerp-under-clamp
  (let ((result (lerp 1 2 -1)))
    (is (= 1 result))))

(test lerp-under-noclamp
  (let ((result (lerp 1 2 -1 :clamp-p nil)))
    (is (= 0 result))))
