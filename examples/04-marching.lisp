(in-package :example-04-marching-cubes)

(defparameter *chunk-size* 3)
(defparameter *chunk-resulution* 5)

(defclass state ()
  ((chunks
    :accessor chunks
    :initform '()
    :documentation "Generated chunks")
   (camera
    :accessor camera
    :initform '()
    :documentation "Camera")))

(defparameter *state* '())

(defun run-example ()
  (with-graphics (graphics "04 Marching Cubes"
		  :hide-cursor t
		  :window-width 800
		  :window-height 600)

    (setup graphics)

    (graphics-run graphics #'render #'tick)))

(defun setup (graphics)
  (setf *state* (make-instance 'state))

  (let ((camera (make-instance 'fps-camera
			       :sensitivity 0.1
			       :speed 10.0)))
    (graphics-setup-fps-camera graphics camera)
    (setf (camera state) camera)))

(defun tick ())

(defun render ())

(defclass chunk ()
  ((mesh
    :accessor chunk-mesh
    :initarg :mesh
    :documentation "Mesh")))

(defun make-chunk (size resolution)
