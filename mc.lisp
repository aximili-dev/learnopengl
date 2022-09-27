(in-package :game-engine)

(defparameter *blocks*
  '((0 . (0 2 1 1 1 1))
    (1 . (2 2 2 2 2 2))))

(defclass world ()
  ((chunks :initform nil)))

(defparameter *chunk-width* 16)
(defparameter *chunk-height* 16)

(defclass chunk ()
  (voxels
   :initform (make-array (* *chunk-width* *chunk-width* *chunk-height*) :element-type 'integer))
  (mesh
   :initarg :mesh))

(defun chunk-coord-to-index (x y z)
  (+ x
     (* z *chunk-width*)
     (* y *chunk-width* *chunk-height*)))

(defun chunk-index-to-coord (i)
  (vec (mod i *chunk-width*)
       (floor (/ i (* *chunk-width* *chunk-width*)))
       (floor (/ i *chunk-width*))))

(defun create-chunk ()
  (let ((chunk (make-instance 'chunk)))
    (with-slots (voxels) chunk
      (dotimes (i (* *chunk-width*))
	(with-vec (x y z) (chunk-index-to-coord)
	  (if (< z 8)
	      (setf (aref voxels) 1)))))))

(defmacro vpush-all (v &rest values)
  (let ((values-sym (gensym)))
    `(let ((,values-sym ,values))
       (loop for value in ,values-sym
	     do (push value ,v)))))

(defvar *atlas* nil)

(defun push-voxel-vertices (atlas v-array i-array voxel-id voxel-coords)
  (macrolet ((push-vertices ((face-accessor) &body body)
	       `(let* ((tex-index (,face-accessor (cdr (assoc voxel-id *blocks*))))
		       (coords (texture-atlas-coordinates atlas tex-index))
		       (uv0 (first coords))
		       (uv1 (second coords))
		       (uv2 (third coords))
		       (uv3 (fourth coords)))
		  ,@body)))
    (with-vec (cx cy cz) voxel-coords
      (push-vertices (first)
		     (push (vertex (vec cx       (1+ cy)  cz     )  (v- +vx+) uv0) v-array)
		     (push (vertex (vec (1+ cx)  (1+ cy)  cz     )  (v- +vx+) uv1) v-array)
		     (push (vertex (vec (1+ cx)  (1+ cy)  (1+ cz))  (v- +vx+) uv2) v-array)
		     (push (vertex (vec cx       (1+ cy)  (1+ cz))  (v- +vx+) uv3) v-array)
		     (push 0 i-array) (push 1 i-array) (push 2 i-array)
		     (push 0 i-array) (push 2 i-array) (push 3 i-array))

      (push-vertices (second)
		     (push (vertex (vec (1+ cx)  cy       cz     )  (v- +vx+) uv0) v-array)
		     (push (vertex (vec cx       cy       cz     )  (v- +vx+) uv1) v-array)
		     (push (vertex (vec cx       cy       (1+ cz))  (v- +vx+) uv2) v-array)
		     (push (vertex (vec (1+ cx)  cy       (1+ cz))  (v- +vx+) uv3) v-array)
		     (push 4 i-array) (push 5 i-array) (push 6 i-array)
		     (push 4 i-array) (push 6 i-array) (push 7 i-array))

      (push-vertices (third)
		     (push (vertex (vec cx       cy       cz)       (v- +vx+) uv0) v-array)
		     (push (vertex (vec cx       (1+ cy)  cz)       (v- +vx+) uv1) v-array)
		     (push (vertex (vec cx       (1+ cy)  (1+ cz))  (v- +vx+) uv2) v-array)
		     (push (vertex (vec cx       cy       (1+ cz))  (v- +vx+) uv3) v-array)
		     (push 8 i-array) (push 9 i-array) (push 10 i-array)
		     (push 8 i-array) (push 10 i-array) (push 11 i-array))

      (push-vertices (fourth)
		     (push (vertex (vec cx       cy       (1+ cz))  (v- +vx+) uv0) v-array)
		     (push (vertex (vec cx       (1+ cy)  (1+ cz))  (v- +vx+) uv1) v-array)
		     (push (vertex (vec (1+ cx)  (1+ cy)  (1+ cz))  (v- +vx+) uv2) v-array)
		     (push (vertex (vec (1+ cx)  cy       (1+ cz))  (v- +vx+) uv3) v-array)
		     (push 12 i-array) (push 13 i-array) (push 14 i-array)
		     (push 12 i-array) (push 14 i-array) (push 15 i-array))

      (push-vertices (fifth)
		     (push (vertex (vec (1+ cx)  cy       (1+ cz))  (v- +vx+) uv0) v-array)
		     (push (vertex (vec (1+ cx)  (1+ cy)  (1+ cz))  (v- +vx+) uv1) v-array)
		     (push (vertex (vec (1+ cx)  (1+ cy)  cz     )  (v- +vx+) uv2) v-array)
		     (push (vertex (vec (1+ cx)  cy       cz     )  (v- +vx+) uv3) v-array)
		     (push 16 i-array) (push 17 i-array) (push 18 i-array)
		     (push 16 i-array) (push 18 i-array) (push 19 i-array))

      (push-vertices (sixth)
		     (push (vertex (vec (1+ cx)  cy       cz     )  (v- +vx+) uv0) v-array)
		     (push (vertex (vec (1+ cx)  (1+ cy)  cz     )  (v- +vx+) uv1) v-array)
		     (push (vertex (vec cx       (1+ cy)  cz     )  (v- +vx+) uv2) v-array)
		     (push (vertex (vec cx       cy       cz     )  (v- +vx+) uv3) v-array)
		     (push 20 i-array) (push 19 i-array) (push 20 i-array)
		     (push 20 i-array) (push 22 i-array) (push 23 i-array)))))

(defmethod construct-mesh ((chunk chunk))
  (with-slots (voxels mesh) chunk
    (let ((vertices '())
	  (indices '()))
      (loop for voxel across voxels
	    for i from 0
	    when (eql voxel 0)
	      do (push-voxel-vertices vertices indices voxel (chunk-index-to-coord i)))
      ())))
		   

(defmethod render-chunk ((chunk chunk) shader world-x world-y)
  (render-mesh mesh))
