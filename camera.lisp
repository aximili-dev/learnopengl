(in-package :game-engine)

(defclass camera ()
  ((pos
    :accessor pos
    :initarg :pos
    :initform (vec 0 0 0))
   (pitch
    :accessor pitch
    :initarg :pitch
    :initform 0.0
    :documentation "Camera pitch in degrees")
   (yaw
    :accessor yaw
    :initarg :yaw
    :initform 0
    :documentation "Camera yaw in degrees")))

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
