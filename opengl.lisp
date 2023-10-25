(in-package :game-engine)

(defun make-gl-array (array type)
  (let* ((n-elements (length array))
	 (arr (gl:alloc-gl-array type n-elements)))
    (dotimes (i n-elements)
      (setf (gl:glaref arr i) (aref array i)))
    arr))

(defmacro with-gl-array ((gl-array array type) &body body)
  `(let ((,gl-array (make-gl-array ,array ,type)))
     ,@body
     (gl:free-gl-array ,gl-array)))

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

(defparameter *mesh* nil)

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

(defparameter *point-light-positions*
  (list (vec  0.7  0.2  2.0)
	(vec  2.3 -3.3 -4.0)
	(vec -4.0  2.0 -12.0)
	(vec  0.0  0.0 -3.0)))

(defparameter *shader-program* nil)
(defparameter *shader-program-colored* nil)

(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)

(defparameter *vbo* nil)
(defparameter *vao* nil)
(defparameter *ebo* nil)

(defparameter *light-pos* (vec 0 3 0))
(defparameter *light-shader* nil)
(defparameter *light-vao* nil)

(defparameter *texture* nil)
(defparameter *texture-spec* nil)

(defparameter *font* nil)

(defparameter *camera* nil)



(defparameter *delta-time* 0.0)
(defparameter *last-frame* 0.0)

(defparameter *sensitivity* 0.1)

(defparameter *mouse-just-entered* nil)
(defparameter *last-x* 400)
(defparameter *last-y* 300)

(defparameter *keys* '())

(defparameter *model* '())
(defparameter *entity* '())

(defparameter *d20-mesh* '())
(defparameter *d20-model* '())
(defparameter *d20-entity* '())

(defparameter *blender-cube* '())
(defparameter *blender-cube-pretty* '())


(defparameter *point-cloud-vao* '())
(defparameter *point-cloud-vbo* '())

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

(defparameter *world* nil)

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
;;  (setf *texture* (load-texture #P"./container.png"))
;;  (setf *texture-spec* (load-texture #P"./container.specular.png"))

  ;;Set shader program
  (setf *shader-program*
	(load-shader-from-disk #P"./shaders/model.vert"
			       #P"./shaders/model.frag"))

  (setf *light-shader*
	(load-shader-from-disk #P"./shaders/model.vert"
			       #P"./shaders/light.frag"))

  (setf *shader-program-colored*
	(load-shader-from-disk #P"./shaders/model_1p.vert"
			       #P"./shaders/model_1p.frag"))
	

;;  (shader-set-uniform *shader-program* "material.diffuse" 0)
;;  (shader-set-uniform *shader-program* "material.specular" 1)

  ;; Get VBO buffer
;;  (setf *vao* (car (gl:gen-vertex-arrays 1)))
;;  (setf *vbo* (car (gl:gen-buffers 1)))
;;  (setf *ebo* (car (gl:gen-buffers 1)))

  ;; Bind VAO
;;  (gl:bind-vertex-array *vao*)

  ;; Copy vertices into buffer
;;  (gl:bind-buffer :array-buffer *vbo*)
;;  (let ((arr (make-gl-array *vertices* :float)))
;;    (gl:buffer-data :array-buffer
;;		    :static-draw
;;		    arr)
;;    (gl:free-gl-array arr))

  ;; Copy indices into buffer
;;  (gl:bind-buffer :element-array-buffer *ebo*)
;;  (let ((arr (make-gl-array *indices* :unsigned-int)))
;;      (gl:buffer-data :element-array-buffer
;;		      :static-draw
;;		      arr)
;;      (gl:free-gl-array arr))

  ;; Set vertex attribute pointers
;;  (gl:vertex-attrib-pointer 0 3 :float :false (* 8 4) 0)
;;  (gl:enable-vertex-attrib-array 0)

;;  (gl:vertex-attrib-pointer 1 3 :float :false (* 8 4) (* 3 4))
;;  (gl:enable-vertex-attrib-array 1)

;;  (gl:vertex-attrib-pointer 2 2 :float :false (* 8 4) (* 6 4))
;;  (gl:enable-vertex-attrib-array 2)

;;  (gl:bind-buffer :array-buffer 0)
;;  (gl:bind-vertex-array 0)

  ;; Lights
;;  (setf *light-vao* (gl:gen-vertex-array))
;;  (gl:bind-vertex-array *light-vao*)
;;  (gl:bind-buffer :array-buffer *vbo*)
;;  (gl:vertex-attrib-pointer 0 3 :float :false (* 5 4) 0)
;;  (gl:enable-vertex-attrib-array 0)

  (setf *font* (load-bitmap-font #P"./charmap.png" *viewport-width* *viewport-height*))

  (setf *model* (load-model #P"./models/cube.obj"
			    :diffuse-path #P"./container.png"
			    :specular-path #P"./container.specular.png"))

  (setf *entity* (make-instance 'entity
				:model *model*
				:shader *shader-program*
				:transform (transform (vec 0 0 0)
						      (vec 1 1 1))))

  (setf *d20-model* (load-model #P"./models/Dice_d20.obj"
			       :diffuse-path #P"./d20white.png"
			       :specular-path #P"./d20white.png"))

  (setf *d20-entity* (make-instance 'entity
				    :model *d20-model*
				    :shader *shader-program*
				    :transform (transform (vec 4 1 4)
							  (vec 3 3 3))))

  (let ((model (load-model #P"./models/cube_blender.obj"
			   :diffuse-path #P"./container.png"
			   :specular-path #P"./container.specular.png")))
    (setf *blender-cube* (make-instance 'entity
					:model model
					:shader *shader-program*
					:transform (transform (vec -4 4 0)
							      (vec 2 2 2)))))

  (let ((model (load-model #P"./models/cube_blender_pretty.obj"
			   :diffuse-path #P"./container.png"
			   :specular-path #P"./container.specular.png")))
    (setf *blender-cube-pretty* (make-instance 'entity
					       :model model
					       :shader *shader-program*
					       :transform (transform (vec -4 -4 0)
								     (vec 2 4 2)))))

  (let ((vao (gl:gen-vertex-array))
	(vbo (gl:gen-buffer))
	(ebo (gl:gen-buffer))
	(*perlin-points* (make-array 0 :adjustable t :fill-pointer 0))
	(indices (make-array 0 :adjustable t :fill-pointer 0))
	(w 150)
	(h 1)
	(max 5)
	(min -1))
    (setf *perlin-points* (make-array 0
				      :adjustable t
				      :fill-pointer 0))

    (dotimes (x w)
      (dotimes (y h)
	(dotimes (z w)

	  (let* (;;(seeds '(0 0 0 0 0 0))
		 (seeds '(1 2 3 4 5 6))
		 (seeds (reverse seeds))
		 (wpos (vec (/ x 10) (/ y 10) (/ z 10)))
	         (value (+ (* (perlin (v* wpos 0.2) :seed (nth 0 seeds)) 5)
			   (* (perlin (v* wpos 0.4) :seed (nth 1 seeds)) 2)
			      (perlin     wpos      :seed (nth 2 seeds))
			   (/ (perlin (v* wpos 2)   :seed (nth 3 seeds)) 2)
			   (/ (perlin (v* wpos 4)   :seed (nth 4 seeds)) 4)
			   (/ (perlin (v* wpos 8)   :seed (nth 5 seeds)) 8)))
		 (value (if (< value 0)
			    (/ value 4)
			    value))
		 (value (if (< value -1)
			    -1.0
			    value))
		 (color (height-to-color value min max)))
	    (when t
	      (vector-push-extend (vx wpos) *perlin-points*)
	      (vector-push-extend value *perlin-points*)
	      (vector-push-extend (vz wpos) *perlin-points*)
	      (vector-push-extend (vx color) *perlin-points*)
	      (vector-push-extend (vy color) *perlin-points*)
	      (vector-push-extend (vz color) *perlin-points*))))))

    (dotimes (z (1- w))
      (dotimes (x w)
	(vector-push-extend (+ (* z w) x) indices)
	(vector-push-extend (+ (* z w) x w) indices))

      (vector-push-extend #xFFFFFFFF indices))

    (print (length indices))
    
    (setf *point-cloud-vao* vao)
    (gl:bind-vertex-array vao)

    (let ((gl-arr (make-gl-array *perlin-points* :float)))
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))

    (let ((gl-arr (make-gl-array indices :unsigned-int)))
      (gl:bind-buffer :element-array-buffer ebo)
      (gl:buffer-data :element-array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))

    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float :false (* 6 4) 0)

    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 3 :float :false (* 6 4) (* 3 4))

    (gl:bind-vertex-array 0))

  )

(defun height-to-color (y min max)
  (let* ((diff (- max min))
	 (dist (- y min))
	 (y (float (/ dist diff))))
    (vec y y y)))

(defun height-to-color-2 (y min max)
  (let* ((diff (- max min))
	 (dist (- y min))
	 (y (/ dist diff)))
    (cond
      ((> y 0.9) (vec (/ #xEB #xFF)
		      (/ #xF1 #xFF)
		      (/ #xFF #xFF)))
      ((> y 0.7) (vec (/ #x54 #xFF)
		      (/ #x50 #xFF)
		      (/ #x3B #xFF)))
      ((> y 0.2) (vec (/ #x93 #xFF)
		      (/ #xC4 #xFF)
		      (/ #x8B #xFF)))
      (t (vec (/ #x85 #xFF)
	      (/ #xC7 #xFF)
	      (/ #xF2 #xFF))))))
     


(defun cleanup ()
  "Cleans up OpenGL."
  (format t "DEBUG: Cleaning up~%")
;;  (gl:delete-vertex-arrays (list *vao*))
;;  (gl:delete-buffers (list *vbo*))
;;  (gl:delete-buffers (list *ebo*))

  (shader-free *shader-program*)
  (unload-bitmap-font *font*)

  (free-model *model*))

(defun render (v-width v-height)
  "Renders everything."
  (gl:clear-color 0.21 0.36 0.43 1.0)
  (gl:clear :color-buffer :depth-buffer)
  
;;  (gl:active-texture 0)
;;  (gl:bind-texture :texture-2d (texture-id *texture*))

;;  (gl:active-texture 1)
;;  (gl:bind-texture :texture-2d (texture-id *texture-spec*))

  (shader-use *shader-program*)

;;  (shader-set-uniform *shader-program* "material.shininess" 32.0)
;;  (shader-set-uniform *shader-program* "material.diffuse" 0)
;;  (shader-set-uniform *shader-program* "material.specular" 1)

;;  (shader-set-uniform *shader-program* "dirLight.direction" 1.0 -1 1)
;;  (shader-set-uniform *shader-program* "dirLight.ambient" 0.2 0.2 0.2)
;;  (shader-set-uniform *shader-program* "dirLight.diffuse" 0.5 0.5 0.5)
;;  (shader-set-uniform *shader-program* "dirLight.specular" 1.0 1.0 1.0)

  (let ((i 0))
    (macrolet ((set-light-prop (i prop &rest values)
		 `(let ((loc (format nil "pointLights[~d].~a" ,i ,prop)))
		    (shader-set-uniform *shader-program* loc ,@values))))
      (dolist (light-pos *point-light-positions*)
	(with-vec (x y z) light-pos
	  (set-light-prop i "position" x y z)
	  (set-light-prop i "constant" 1.0)
	  (set-light-prop i "linear" 0.09)
	  (set-light-prop i "quadratic" 0.032)
	  (set-light-prop i "ambient" 0.2 0.2 0.2)
	  (set-light-prop i "diffuse" 0.5 0.5 0.5)
	  (set-light-prop i "specular" 1.0 1.0 1.0))
	(incf i))))

;;  (let ((view (view-matrix *camera*))
;;	(projection (mperspective 45 (/ v-width v-height) 0.1 1000)))

;;    (with-slots (pos) *camera*
;;      (with-vec (x y z) pos
;;	(shader-set-uniform *shader-program* "viewPos" x y z)))
    
;;    (shader-set-uniform *shader-program* "view" (marr view))
;;    (shader-set-uniform *shader-program* "projection" (marr projection))

;;    (dotimes (i 10)
;;      (let* ((step (/ 360 10))
;;	     (model (meye 4))
;;	     (scale 3)
;;	     (time (/ (get-internal-run-time) internal-time-units-per-second))
;;	     (angle (+ (* 2 time) (* step i)))
;;	     (i (* scale (sin angle)))
;;	     (j (* scale (cos angle))))
;;	(nmtranslate model (vec i 0 j))
;;	(shader-set-uniform *shader-program* "model" (marr model))

;;	(gl:bind-vertex-array *vao*)
;;	(gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length *indices*))))

;;    (dolist (light-pos *point-light-positions*)
;;      (let ((model (meye 4)))
;;	(nmtranslate model light-pos)
;;	(nmscale model (vec 0.2 0.2 0.2))

;;	(shader-set-uniform *light-shader* "view" (marr view))
;;	(shader-set-uniform *light-shader* "projection" (marr projection))
;;	(shader-set-uniform *light-shader* "model" (marr model))

;;	(gl:bind-vertex-array *vao*)
;;	(gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length *indices*))))

;;    (gl:bind-vertex-array 0))

  ;;(render-entity *entity* v-width v-height *camera*)
  ;;(render-entity *d20-entity* v-width v-height *camera*)
  ;;(render-entity *blender-cube* v-width v-height *camera*)
  ;;(render-entity *blender-cube-pretty* v-width v-height *camera*)

  (shader-use *shader-program-colored*)

  (let ((view (view-matrix *camera*))
	(proj (mperspective 45 (/ v-width v-height) 0.1 1000)))
    (shader-set-uniform *shader-program-colored* "view" (marr view))
    (shader-set-uniform *shader-program-colored* "projection" (marr proj))
    (shader-set-uniform *shader-program-colored* "model" (marr (meye 4))))

  ;;(gl:polygon-mode :front-and-back :line)
  (cl-opengl-bindings:primitive-restart-index #xFFFFFFFF)

  (gl:bind-vertex-array *point-cloud-vao*)
  (gl:draw-elements :triangle-strip
		    (gl:make-null-gl-array :unsigned-int)
		    :count 44849)
  (gl:polygon-mode :front-and-back :fill)
  (gl:bind-vertex-array 0)
  
  )

(defun render-debug-ui (fps frame-time time dt)
  "Renders debug info."
  (render-bmp-text *font* (format nil "FPS: ~3,3f  TIME: ~3,3f  FT: ~3,3f  DT: ~3,3f" fps time frame-time dt) 1 0 0)

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
  (process-tick *camera* time dt *keys*))

(defclass graphics ()
  ((v-width
    :initarg :v-width
    :initform 800
    :accessor graphics-v-width
    :documentation "The width of the viewport.")
   (v-height
    :initarg :v-height
    :initform 600
    :accessor graphics-v-height
    :documentation "The height of the viewport.")
   (title
    :initarg :title
    :initform "Graphics"
    :accessor graphics-title
    :documentation "The title of the running application.")
   (window
    :initarg :window
    :accessor graphics-window
    :documentation "The GLFW window handle.")
   (keys
    :initform '()
    :accessor graphics-keys
    :documentation "Current keyboard state.")
   (bmp-font
    :initarg :bmp-font
    :initform '()
    :accessor graphics-bmp-font
    :documentation "Bitmap font.")
   (debug-p
    :initarg :debug-p
    :initform '()
    :accessor graphics-debug-p
    :documentation "Whether to show the graphics UI.")
   (debug-text
    :initarg :debug-text
    :initform '()
    :accessor graphics-debug-text
    :documentation "A list of strings to show on screen, from the bottom down")
   (last-x
    :initform 0
    :accessor graphics-last-x
    :documentation "Last recorded cursor position")
   (last-y
    :initform 0
    :accessor graphics-last-y
    :documentation "Last recorded cursor position")
   (mouse-just-entered
    :initform '()
    :accessor graphics-mouse-just-entered
    :documentation "Whether the mouse just entered the window")))

(defmethod graphics-resize ((graphics graphics) window width height)
  (with-slots (v-width v-height) (graphics-bmp-font graphics)
    (setf v-width width)
    (setf v-height height))

  (setf (graphics-v-width graphics) width)
  (setf (graphics-v-height graphics) height)
  (gl:viewport 0 0 width height))

(defmethod graphics-run ((graphics graphics) render-fn &optional tick-fn)
  (let ((time         0.0)
	(dt           0.01)
	(current-time (/ (get-internal-real-time) internal-time-units-per-second))
	(acc          0.0))
    (loop for frame from 0
	  until (glfw:window-should-close-p (graphics-window graphics))
	  do (progn
	       (let* ((new-time   (/ (get-internal-real-time) internal-time-units-per-second))
		      (frame-time (- new-time current-time)))
		 (setf current-time new-time)

		 (incf acc frame-time)

		 (loop while (>= acc dt)
		       do (progn
			    (funcall tick-fn time dt)
			    (incf time dt)
			    (decf acc dt)))

		 (let ((fps (if (> frame-time 0)
				(/ 1 frame-time)
				-12.0)))
		   (funcall render-fn frame fps :debugp (graphics-debug-p graphics)))

		 (when (graphics-debug-p graphics)
		   (with-slots (debug-text bmp-font) graphics
		     (dotimes (y (length debug-text))
		       (render-bmp-text bmp-font
					(nth y debug-text)
					1
					0 y))))
		 (setf (graphics-debug-text graphics) '())
		
		     ;;(loop for y from (1- (length debug-text)) downto 0
		;;	   do (progn
		;;		(render-bmp-text bmp-font
		;;				 (nth (- (length debug-text) y) debug-text)
		;;				 1
		;;				 0 y)))))

		 (glfw:swap-buffers (graphics-window graphics))
		 (glfw:poll-events))))))
	     

(defmethod graphics-set-key-callback ((graphics graphics) callback)
  "callback should receive (window key scan action mod)."
  (with-slots (keys) graphics
    (let ((callback-sym (gensym)))
      (glfw:def-key-callback callback-sym (window key scan action mod)
	(if (eql action :press)
	    (case key
	      (:escape (glfw:set-window-should-close window))
	      (:f3 (setf (graphics-debug-p graphics) (not (graphics-debug-p graphics))))
	      (t (progn
		   (push key (graphics-keys graphics))
		   (funcall callback window key scan action mod)))))
	(if (eql action :release)
	    (flet ((is-key (other) (eql other key)))
	      (setf keys (remove-if #'is-key keys)))))
      (glfw:set-key-callback (quote callback-sym) (graphics-window graphics)))))

(defmethod graphics-set-mouse-enter-callback ((graphics graphics) callback)
  "callback should receive (window enterp)"
  (let ((callback-sym (gensym)))
    (glfw:def-cursor-enter-callback callback-sym (window enterp)
      (funcall callback window enterp))

    (glfw:set-cursor-enter-callback (quote callback-sym) (graphics-window graphics))))

(defmethod graphics-set-mouse-pos-callback ((graphics graphics) callback)
  (let ((callback-sym (gensym)))
    (glfw:def-cursor-pos-callback callback-sym (window x y)
      (funcall callback window x y))

    (glfw:set-cursor-position-callback (quote callback-sym) (graphics-window graphics))))

(defmethod graphics-setup-fps-camera ((graphics graphics) (camera camera))
  (flet ((process-mouse (window x-pos y-pos)
	   (with-slots (last-x last-y mouse-just-entered) graphics
	     (let ((x-offset (- x-pos last-x))
		   (y-offset (- last-y y-pos)))
	       (setf last-x x-pos)
	       (setf last-y y-pos)
	       (if mouse-just-entered
		   (setf mouse-just-entered nil)
		   (handle-mouse-movement camera x-offset y-offset)))))
	 (process-mouse-enter (window enterp)
	   (setf (graphics-mouse-just-entered graphics) t)))
    (graphics-set-mouse-enter-callback graphics #'process-mouse-enter)
    (graphics-set-mouse-pos-callback graphics #'process-mouse)))

	 
	       
    
(defmacro with-graphics ((graphics title
			  &key window-width window-height hide-cursor)
			 &body body)
  "Creates a window using GLFW. Creates an OpenGL context that links to the window."
  (let ((window (gensym))
	(resize-callback-name (gensym)))
    `(glfw:with-init
       (let* ((,window (glfw:create-window :width ,window-width
					   :height ,window-height
					   :title ,title
					   :context-version-major 4
					   :context-version-minor 5
					   :opengl-profile :opengl-core-profile))
	      (,graphics (make-instance 'graphics
				       :window ,window
				       :v-width ,window-width
				       :v-height ,window-height
				       :title ,title
				       :debug-p t
				       :bmp-font (load-bitmap-font #P"./charmap.png"
								   ,window-width
								   ,window-height))))
	 (glfw:def-window-size-callback ,resize-callback-name (window width height)
	   (graphics-resize ,graphics window width height))

	 (glfw:set-window-size-callback ',resize-callback-name ,window)

	 (graphics-set-key-callback ,graphics (lambda (window key scan action mod) '()))
	 
	 ;;; TODO: Setup mouse callbacks
	 ;;; Handle mouse movement
	 ;;; Handle mouse entering window


	 ,(if hide-cursor
	      `(glfw:set-input-mode :cursor :disabled))

	 (gl:viewport 0 0 ,window-width ,window-height)

	 (gl:enable :depth-test)

	 (gl:enable :cull-face)
	 (gl:cull-face :back)
	 
         ;;; https://community.khronos.org/t/adjust-gl-points-size/67980
	 ;;; (gl:enable :program-point-size)

	 ,@body))))
	 

;;; Time step algorithm from https://gafferongames.com/post/fix_your_timestep/
(defun run-engine ()
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
