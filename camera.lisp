(in-package :game-engine)

(defclass camera ()
  ((pos
    :accessor pos
    :initarg :pos
    :initform (vec 0 0 0))
   (front
    :accessor front
    :initarg :front
    :initform (vec 0 0 1))
   (up
    :accessor up
    :initarg :up
    :initform (vec 0 1 0))
   (speed
    :accessor speed
    :initarg :speed
    :initform 1.0
    :documentation "Camera speed in \"units\" per second")))

(defmacro with-camera-props ((pitch yaw) camera &body body)
  (let ((front-var (gensym)))
    `(with-slots ((,front-var front)) ,camera
       (let ((,pitch (asin (vy ,front-var)))
	     (,yaw (vangle +vx+ (vx_z ,front-var))))
	 ,@body))))

(defclass fps-camera (camera) 
  ((can-fly
    :initarg :can-fly 
    :initform t
    :documentation "If true, forward moves camera in looking direction.
If false, forward moves camera forward at same height")
   (sensitivity
    :initarg :sensitivity
    :initform 1)))

(defclass top-down-camera (camera)
  ((min-height
    :accessor min-height
    :initarg :min-height
    :initform 0.0)))

(defgeneric handle-keyboard (camera key dt)
  (:documentation "Handles keyboard input."))

(defmethod handle-keyboard ((camera fps-camera) key dt)
  (with-slots (speed pos front up) camera
    (let* ((distance (* speed dt))
	   (right (vc front up))
	   (direction (case key
			(:w (vc up right))
			(:a (v- right))
			(:s (v- (vc up right)))
			(:d (v+ right))
			(:space (vcopy up))
			(:c (v- up)))))
      (move camera (v* (vunit direction) distance)))))
      
(defgeneric handle-mouse-movement (camera dx dy)
  (:documentation "Handle mouse movement."))

(defmethod handle-mouse-movement ((camera fps-camera) dx dy)
  (with-slots (front sensitivity) camera
    (with-camera-props (pitch yaw) camera
      (incf pitch (* sensitivity (radians dy)))
      (decf yaw (* sensitivity (radians dx)))
      (print dx)
      (let* ((pitch (clamp pitch (- (/ pi 2)) (/ pi 2)))
	     (x (* (cos yaw) (cos pitch)))
	     (y (* (sin pitch)))
	     (z (* (sin yaw) (cos pitch))))
	(setf front (vunit (vec x y z)))))))

(defmethod view-matrix ((camera camera))
  (with-slots (pos front) camera
    (mlookat pos (v+ pos front) (vec 0 1 0))))

(defmethod move ((camera camera) delta)
  (with-slots (pos) camera
    (setf pos (v+ pos delta))))
