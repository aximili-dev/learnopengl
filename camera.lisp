(in-package :game-engine)

(defclass camera ()
  ((pos
    :initarg :pos
    :initform (vec 0 0 0))
   (pitch
    :initarg :pitch
    :initform 0.0
    :documentation "Camera pitch in degrees.")
   (yaw
    :initarg :yaw
    :initform 90.0
    :documentation "Camera yaw in ccw degrees from positive x.")
   (roll
    :initarg :roll
    :initform 0.0
    :documentation "Camera roll around local z axis.")
   (speed
    :accessor speed
    :initarg :speed
    :initform 1.0
    :documentation "Camera speed in \"units\" per second")))

(defmacro with-camera-props ((front right) camera &body body)
  `(with-slots (pitch yaw) ,camera
     (let* ((x (* (cos (radians pitch)) (cos (radians yaw))))
	    (y (* (sin (radians pitch))))
	    (z (* (cos (radians pitch)) (sin (radians yaw))))
	    (,front (vunit (vec x y z)))
	    (,right (vunit (vc ,front +vy+))))
       ,@body)))

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
  (with-slots (speed pos) camera
    (with-camera-props (front right) camera
      (let* ((distance (* speed dt))
	     (direction (case key
			  (:w (vc +vy+ right))
			  (:a (v- right))
			  (:s (vc right +vy+))
			  (:d (v+ right))
			  (:space +vy+)
			  (:c (v- +vy+)))))
	(move camera (v* (vunit direction) distance))))))
      
(defgeneric handle-mouse-movement (camera dx dy)
  (:documentation "Handle mouse movement."))

(defmethod handle-mouse-movement ((camera fps-camera) dx dy)
  (with-slots (pitch yaw sensitivity) camera
    (incf pitch (* sensitivity dy))
    (setf pitch (clamp pitch -89.0 89.0))
    (incf yaw (* sensitivity dx))))

(defmethod view-matrix ((camera camera))
  (with-slots (pos) camera
    (with-camera-props (front right) camera
      (mlookat pos (v+ pos front) (vec 0 1 0)))))

(defmethod move ((camera camera) delta)
  (with-slots (pos) camera
    (setf pos (v+ pos delta))))

(defmethod process-tick ((camera camera) time dt)
  (with-slots (speed pos) camera
    (with-camera-props (front right) camera
      (let ((distance  (* speed dt))
	    (direction (cond
			 ((find :w *keys*) (vc +vy+ right))
			 ((find :a *keys*) (v- right))
			 ((find :s *keys*) (vc right +vy+))
			 ((find :d *keys*) (v+ right))
			 ((find :space *keys*) +vy+)
			 ((find :c *keys*) (v- +vy+)))))
	(if direction
	    (move camera (v* (vunit direction) distance)))))))
