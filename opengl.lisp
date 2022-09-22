(in-package :game-engine)

(defun make-gl-array (array type)
  (let* ((n-elements (length array))
	 (arr (gl:alloc-gl-array type n-elements)))
    (dotimes (i n-elements)
      (setf (gl:glaref arr i) (aref array i)))
    arr))

(defparameter *vertices*
  ;; Positions      Normals        Tex coords
  #(-0.5 -0.5 -0.5  0.0  0.0 -1.0  0.0  0.0
     0.5 -0.5 -0.5  0.0  0.0 -1.0  1.0  0.0
     0.5  0.5 -0.5  0.0  0.0 -1.0  1.0  1.0
     0.5  0.5 -0.5  0.0  0.0 -1.0  1.0  1.0
    -0.5  0.5 -0.5  0.0  0.0 -1.0  0.0  1.0
    -0.5 -0.5 -0.5  0.0  0.0 -1.0  0.0  0.0

    -0.5 -0.5  0.5  0.0  0.0  1.0  0.0  0.0
     0.5 -0.5  0.5  0.0  0.0  1.0  1.0  0.0
     0.5  0.5  0.5  0.0  0.0  1.0  1.0  1.0
     0.5  0.5  0.5  0.0  0.0  1.0  1.0  1.0
    -0.5  0.5  0.5  0.0  0.0  1.0  0.0  1.0
    -0.5 -0.5  0.5  0.0  0.0  1.0  0.0  0.0

    -0.5  0.5  0.5 -1.0  0.0  0.0  1.0  0.0
    -0.5  0.5 -0.5 -1.0  0.0  0.0  1.0  1.0
    -0.5 -0.5 -0.5 -1.0  0.0  0.0  0.0  1.0
    -0.5 -0.5 -0.5 -1.0  0.0  0.0  0.0  1.0
    -0.5 -0.5  0.5 -1.0  0.0  0.0  0.0  0.0
    -0.5  0.5  0.5 -1.0  0.0  0.0  1.0  0.0

     0.5  0.5  0.5  1.0  0.0  0.0  1.0  0.0
     0.5  0.5 -0.5  1.0  0.0  0.0  1.0  1.0
     0.5 -0.5 -0.5  1.0  0.0  0.0  0.0  1.0
     0.5 -0.5 -0.5  1.0  0.0  0.0  0.0  1.0
     0.5 -0.5  0.5  1.0  0.0  0.0  0.0  0.0
     0.5  0.5  0.5  1.0  0.0  0.0  1.0  0.0

    -0.5 -0.5 -0.5  0.0 -1.0  0.0  0.0  1.0
     0.5 -0.5 -0.5  0.0 -1.0  0.0  1.0  1.0
     0.5 -0.5  0.5  0.0 -1.0  0.0  1.0  0.0
     0.5 -0.5  0.5  0.0 -1.0  0.0  1.0  0.0
    -0.5 -0.5  0.5  0.0 -1.0  0.0  0.0  0.0
    -0.5 -0.5 -0.5  0.0 -1.0  0.0  0.0  1.0

    -0.5  0.5 -0.5  0.0  1.0  0.0  0.0  1.0
     0.5  0.5 -0.5  0.0  1.0  0.0  1.0  1.0
     0.5  0.5  0.5  0.0  1.0  0.0  1.0  0.0
     0.5  0.5  0.5  0.0  1.0  0.0  1.0  0.0
    -0.5  0.5  0.5  0.0  1.0  0.0  0.0  0.0
    -0.5  0.5 -0.5  0.0  1.0  0.0  0.0  1.0))

(defparameter *indices*
  #( 0  1  2  3  4  5
     6  7  8  9 10 11
    12 13 14 15 16 17
    18 19 20 21 22 23
    24 25 26 27 28 29
    30 31 32 33 34 35))

(defparameter *instance-positions*
  (list (vec  0    0    0)
	(vec  2    5   -15)
	(vec -1.5 -2.2 -2.5)
	(vec -3.8 -2.0 -12.3)
	(vec  2.4 -0.4 -3.5)
	(vec -1.7  3.0 -7.5)
	(vec  1.3 -2.0 -2.5)
	(vec  1.5  2.0 -2.5)
	(vec  1.5  0.2 -1.5)
	(vec -1.3  1.0 -1.5)))

(defparameter *shader-program* nil)

(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)

(defparameter *vbo* nil)
(defparameter *vao* nil)
(defparameter *ebo* nil)

(defparameter *light-pos* (vec 0 3 0))
(defparameter *light-shader* nil)
(defparameter *light-vao* nil)

(defparameter *texture* nil)
(defparameter *face-texture* nil)

(defparameter *font* nil)

(defparameter *camera* nil)



(defparameter *delta-time* 0.0)
(defparameter *last-frame* 0.0)

(defparameter *sensitivity* 0.1)

(defparameter *mouse-just-entered* nil)
(defparameter *last-x* 400)
(defparameter *last-y* 300)

(defparameter *keys* '())

;;;
;;; GLFW Callbacks
;;;
(defun resize (window width height)
  (with-slots (v-width v-height) *font*
    (setf v-width width)
    (setf v-height height))
  
  (setf *viewport-width* width)
  (setf *viewport-height* height)
  (gl:viewport 0 0 width height))

(defun process-input (window key scancode action mod-keys)
  (if (eql action :press)
      (case key
	(:escape (glfw:set-window-should-close window))
	(otherwise (push key *keys*))))
  (if (eql action :release)
      (flet ((is-key (other) (eql other key)))
	(setf *keys* (remove-if #'is-key *keys*)))))

(defun process-mouse (window x-pos y-pos)
  (let ((x-offset (- x-pos *last-x*))
	(y-offset (- *last-y* y-pos)))
    (setf *last-x* x-pos)
    (setf *last-y* y-pos)

    (if *mouse-just-entered*
	(setf *mouse-just-entered* nil)
	(handle-mouse-movement *camera* x-offset y-offset))))

(defun process-mouse-enter (window entered)
  (if entered
      (setf *mouse-just-entered* t)))

;;;
;;; OpenGL code
;;;
(defun setup (window)
  "Sets up OpenGL so the engine can work."
  (setf *camera* (make-instance 'fps-camera
				:sensitivity 0.1
				:speed 10.0
				:pos (vec 0 0 10)))
  
  ;; Load texture
  (setf *texture* (load-texture #P"./container.png"))
  (setf *face-texture* (load-texture #P"./awesomeface.png"))

  ;;Set shader program
  (setf *shader-program*
	(load-shader-from-disk #P"./shaders/model.vert"
			       #P"./shaders/model.frag"))

  (setf *light-shader*
	(load-shader-from-disk #P"./shaders/model.vert"
			       #P"./shaders/light.frag"))

  ;(shader-set-uniform *shader-program* "texture1" 0)
  ;(shader-set-uniform *shader-program* "texture2" 1)
  
  ;; Get VBO buffer
  (setf *vao* (car (gl:gen-vertex-arrays 1)))
  (setf *vbo* (car (gl:gen-buffers 1)))
  (setf *ebo* (car (gl:gen-buffers 1)))

  ;; Bind VAO
  (gl:bind-vertex-array *vao*)

  ;; Copy vertices into buffer
  (gl:bind-buffer :array-buffer *vbo*)
  (let ((arr (make-gl-array *vertices* :float)))
    (gl:buffer-data :array-buffer
		    :static-draw
		    arr)
    (gl:free-gl-array arr))

  ;; Copy indices into buffer
  (gl:bind-buffer :element-array-buffer *ebo*)
  (let ((arr (make-gl-array *indices* :unsigned-int)))
      (gl:buffer-data :element-array-buffer
		      :static-draw
		      arr)
      (gl:free-gl-array arr))

  ;; Set vertex attribute pointers
  (gl:vertex-attrib-pointer 0 3 :float :false (* 8 4) 0)
  (gl:enable-vertex-attrib-array 0)

  (gl:vertex-attrib-pointer 1 3 :float :false (* 8 4) (* 3 4))
  (gl:enable-vertex-attrib-array 0)

  (gl:vertex-attrib-pointer 2 2 :float :false (* 8 4) (* 6 4))
  (gl:enable-vertex-attrib-array 1)

  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0)

  ;; Lights
  (setf *light-vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *light-vao*)
  (gl:bind-buffer :array-buffer *vbo*)
  (gl:vertex-attrib-pointer 0 3 :float :false (* 5 4) 0)
  (gl:enable-vertex-attrib-array 0)

  (setf *font* (load-bitmap-font #P"./charmap.png" *viewport-width* *viewport-height*)))

(defun cleanup ()
  "Cleans up OpenGL."
  (format t "DEBUG: Cleaning up~%")
  (gl:delete-vertex-arrays (list *vao*))
  (gl:delete-buffers (list *vbo*))
  (gl:delete-buffers (list *ebo*))
  (gl:delete-textures (list *texture* *face-texture*))

  (shader-free *shader-program*)
  (unload-bitmap-font *font*))

(defun render (v-width v-height)
  "Renders everything."
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer :depth-buffer)
  
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *texture*)

  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *face-texture*)

  (shader-use *shader-program*)

  (shader-set-uniform *shader-program* "material.ambient" 1.0 0.5 0.31)
  (shader-set-uniform *shader-program* "material.diffuse" 1.0 0.5 0.31)
  (shader-set-uniform *shader-program* "material.specular" 0.5 0.5 0.5)
  (shader-set-uniform *shader-program* "material.shininess" 32.0)

  (with-vec (x y z) *light-pos*
    (shader-set-uniform *shader-program* "light.position" x y z))

  (shader-set-uniform *shader-program* "light.ambient" 0.2 0.2 0.2)
  (shader-set-uniform *shader-program* "light.diffuse" 0.5 0.5 0.5)
  (shader-set-uniform *shader-program* "light.specular" 1.0 1 1)

  (let ((view (view-matrix *camera*))
	(projection (mperspective 45 (/ v-width v-height) 0.1 1000)))

    (with-slots (pos) *camera*
      (with-vec (x y z) pos
	(shader-set-uniform *shader-program* "viewPos" x y z)))
    
    (shader-set-uniform *shader-program* "view" (marr view))
    (shader-set-uniform *shader-program* "projection" (marr projection))

    (dotimes (i 10)
      (let* ((step (/ (* 2 pi) 10))
	     (model (meye 4))
	     (scale 3)
	     (time (/ (get-internal-run-time) internal-time-units-per-second))
	     (angle (+ (* 2 time) (* step i)))
	     (i (* scale (sin angle)))
	     (j (* scale (cos angle))))
	(nmtranslate model (vec i 0 j))
	(shader-set-uniform *shader-program* "model" (marr model))

	(gl:bind-vertex-array *vao*)
	(gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length *indices*))))

    (let ((model (meye 4)))
      (nmtranslate model *light-pos*)
      (nmscale model (vec 0.2 0.2 0.2))

      (shader-set-uniform *light-shader* "view" (marr view))
      (shader-set-uniform *light-shader* "projection" (marr projection))
      (shader-set-uniform *light-shader* "model" (marr model))

      (gl:bind-vertex-array *vao*)
      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length *indices*)))

  (gl:bind-vertex-array 0)))

(defun render-debug-ui (fps frame-time time dt)
  "Renders debug info."
  (render-bmp-text *font* (format nil "FPS: ~3,3f  TIME: ~3,3f  FT: =3,3f  DT: ~3,3f" fps time frame-time dt) 1 0 0)

  (with-slots (pos) *camera*
    (with-camera-props (front right) *camera*
      (render-bmp-text *font* (format nil "POS: (~{~,2f~^ ~}) FRONT: (~{~,2f~^ ~}) |~,3f|"
				      (with-vec (x y z) pos
					(list x y z))
				  (with-vec (x y z) front
				    (list x y z))
				  (vlength front))
		       1 0 1)))
  (with-slots (pitch yaw) *camera*
    (render-bmp-text *font* (format nil "PITCH: ~,2f°   YAW: ~,2f°"
				    pitch
				    yaw)
		     1 0 2))
  (render-bmp-text *font* (format nil "X: ~4d  Y: ~4d" *last-x* *last-y*)
		   1 0 3)

  (render-bmp-text *font* (format nil "Keys: ~{~a~^ ~}" *keys*) 1 0 4))

;;;
;;; Main loop
;;;

(defun tick (time dt)
  (process-tick *camera* time dt))

;;; Time step algorithm from https://gafferongames.com/post/fix_your_timestep/
(defun run ()
  (with-glfw (window
	      :width 800
	      :height 600
	      :title "Game Engine")

    (setup-callbacks window
		     :resize #'resize
		     :keyboard #'process-input
		     :cursor-pos #'process-mouse
		     :cursor-enter #'process-mouse-enter)

    (let ((time          0.0)
	  (dt            0.01)
	  (current-time  (/ (get-internal-real-time) internal-time-units-per-second))
	  (acc           0.0))
      (setup window)
      
      (loop for frame from 0
	    until (glfw:window-should-close-p window)
	    do (progn
		 (let* ((new-time   (/ (get-internal-real-time) internal-time-units-per-second))
			(frame-time (- new-time current-time)))
		   ;(if (> frame-time 0.25)
		   ;    (setf frame-time 0.25))

		   (setf current-time new-time)
		   
		   (incf acc frame-time)

		   (loop while (>= acc dt)
			 do (progn
			      (tick time dt)
			      (incf time dt)
			      (decf acc dt)))

		   (setf (vy *light-pos*) (* 3 (sin (* current-time 2))))

		   (render *viewport-width* *viewport-height*)

		   (let ((fps (if (> frame-time 0)
				  (/ 1 frame-time)
				  -12.0)))
		     (render-debug-ui fps frame-time time dt))
		 
		   (glfw:swap-buffers window)
		   (glfw:poll-events))))
      (cleanup))))
