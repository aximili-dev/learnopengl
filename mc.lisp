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

(defmethod render-chunk ((chunk chunk) shader world-x world-y)
  (render-mesh mesh))
