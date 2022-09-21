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

(defmacro with-camera-props ((pitch yaw) camera &rest body)
  (let ((front-var (gensym)))
    `(with-slots ((,front-var front)) ,camera
       (let ((,pitch (asin (vy ,front-var)))
	     (,yaw (vangle +vz+ (vx_z ,front-var))))
	 ,@body))))

(defclass fps-camera (camera)
  (can-fly
   :accessor can-fly
   :initarg :can-fly
   :initform t
   :documentation "If true, forward moves camera in looking direction.
If false, forward moves camera forward at same height"))

(defclass top-down-camera (camera)
  (min-height
   :accessor min-height
   :initarg :min-height
   :initform 0.0))

(defgeneric handle-keyboard (camera key dt))

(defmethod handle-keyboard ((camera fps-camera) key dt)
  (with-slots (speed) camera
    (let ((speed (* speed dt)))
      (with-camera-props (pitch yaw) camera

(defmethod view-matrix ((camera camera))
  (with-slots (pos pitch yaw) camera
    (let* ((pos (radians pos))
	   (pitch (radians pitch))
	   (yaw (radians yaw))
	   (direction (vec (cos (* yaw pitch))
			   (sin pitch)
			   (sin (* yaw pitch)))))
      (mlookat pos (v+ pos direction) (vec 0 1 0)))))

(defmethod move ((camera camera) delta)
  (nv+ (pos camera) delta))
