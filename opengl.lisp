(in-package :game-engine)

(defun make-gl-array (array type)
  (let* ((n-elements (length array))
	 (arr (gl:alloc-gl-array type n-elements)))
    (dotimes (i n-elements)
      (setf (gl:glaref arr i) (aref array i)))
    arr))

(defparameter *vertices*
  ;; Positions      Colors         Tex coords
  #(-0.5 -0.5 -0.5  0.0  0.0
     0.5 -0.5 -0.5  1.0  0.0
     0.5  0.5 -0.5  1.0  1.0
     0.5  0.5 -0.5  1.0  1.0
    -0.5  0.5 -0.5  0.0  1.0
    -0.5 -0.5 -0.5  0.0  0.0
    -0.5 -0.5  0.5  0.0  0.0
     0.5 -0.5  0.5  1.0  0.0
     0.5  0.5  0.5  1.0  1.0
     0.5  0.5  0.5  1.0  1.0
    -0.5  0.5  0.5  0.0  1.0
    -0.5 -0.5  0.5  0.0  0.0
    -0.5  0.5  0.5  1.0  0.0
    -0.5  0.5 -0.5  1.0  1.0
    -0.5 -0.5 -0.5  0.0  1.0
    -0.5 -0.5 -0.5  0.0  1.0
    -0.5 -0.5  0.5  0.0  0.0
    -0.5  0.5  0.5  1.0  0.0
     0.5  0.5  0.5  1.0  0.0
     0.5  0.5 -0.5  1.0  1.0
     0.5 -0.5 -0.5  0.0  1.0
     0.5 -0.5 -0.5  0.0  1.0
     0.5 -0.5  0.5  0.0  0.0
     0.5  0.5  0.5  1.0  0.0
    -0.5 -0.5 -0.5  0.0  1.0
     0.5 -0.5 -0.5  1.0  1.0
     0.5 -0.5  0.5  1.0  0.0
     0.5 -0.5  0.5  1.0  0.0
    -0.5 -0.5  0.5  0.0  0.0
    -0.5 -0.5 -0.5  0.0  1.0
    -0.5  0.5 -0.5  0.0  1.0
     0.5  0.5 -0.5  1.0  1.0
     0.5  0.5  0.5  1.0  0.0
     0.5  0.5  0.5  1.0  0.0
    -0.5  0.5  0.5  0.0  0.0
    -0.5  0.5 -0.5  0.0  1.0))

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

(defparameter *vertex-shader-source*
  (with-output-to-string (s)
    (format s "#version 330 core~%")
    (format s "layout (location = 0) in vec3 aPos;~%")
    (format s "layout (location = 1) in vec2 aTexCoord;~%")
    (format s "out vec2 texCoord;~%")
    (format s "uniform mat4 view;~%")
    (format s "uniform mat4 model;~%")
    (format s "uniform mat4 projection;~%")
    (format s "void main ()~%")
    (format s "{~%")
    (format s "  gl_Position = projection * view * model * vec4(aPos, 1.0);~%")
    (format s "  texCoord = vec2(aTexCoord.x, aTexCoord.y);~%")
    (format s "}~c" #\Nul)))

(defparameter *fragment-shader-source*
  (with-output-to-string (s)
    (format s "#version 330 core~%")
    (format s "out vec4 FragColor;~%")
    (format s "in vec2 texCoord;~%")
    (format s "uniform sampler2D texture1;~%")
    (format s "uniform sampler2D texture2;~%")
    (format s "void main()~%")
    (format s "{~%")
    (format s "  FragColor = mix(texture(texture1, texCoord), texture(texture2, texCoord), 0.2);~%")
    (format s "}~c" #\Nul)))

(defparameter *shader-program* nil)

(defparameter *vbo* nil)
(defparameter *vao* nil)
(defparameter *ebo* nil)

   

(defparameter *texture* nil)
(defparameter *face-texture* nil)

(defun setup (window)
  ;; Load texture
  (setf *texture* (load-texture #P"./container.png"))
  (setf *face-texture* (load-texture #P"./awesomeface.png"))

  ;;Set shader program
  (setf *shader-program*
	(load-shader-program #P"./shaders/model.vert"
			     #P"./shaders/model.frag"))

  (shader-set-uniform *shader-program* "texture1" 0)
  (shader-set-uniform *shader-program* "texture2" 1)
  
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
  (gl:vertex-attrib-pointer 0 3 :float :false (* 5 4) 0)
  (gl:enable-vertex-attrib-array 0)

  (gl:vertex-attrib-pointer 1 2 :float :false (* 5 4) (* 3 4))
  (gl:enable-vertex-attrib-array 1)

  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0))

(defun cleanup ()
  (format t "DEBUG: Cleaning up~%")
  (gl:delete-vertex-arrays (list *vao*))
  (gl:delete-buffers (list *vbo*))
  (gl:delete-buffers (list *ebo*))
  (gl:delete-program *shader-program*))

(defun resize (window width height)
  (gl:viewport 0 0 width height))

(glfw:def-window-size-callback window-size-callback (window width height)
  (resize window width height))

(defparameter *camera-pos*   (vec 0 0 3))
(defparameter *camera-front* (vec 0 0 -1))
(defparameter *camera-up*    (vec 0 1 0))

(defparameter *camera-speed* 1500.0)

(defparameter *delta-time* 0.0)
(defparameter *last-frame* 0.0)

(defparameter *sensitivity* 0.1)
(defparameter *last-x* 400)
(defparameter *last-y* 300)

(defparameter *yaw* 0)
(defparameter *pitch* 0)

(defun process-input (window key scancode action mod-keys)
  (let ((*camera-speed* (* *camera-speed* *delta-time*)))
    (when (eql action :press)
      (case key
	(:escape (glfw:set-window-should-close window))
	(:w (nv+ *camera-pos* (v* *camera-speed* *camera-front*)))
	(:s (nv- *camera-pos* (v* *camera-speed* *camera-front*)))
	(:a (nv- *camera-pos* (v* (vunit (vc *camera-front* *camera-up*))
				  *camera-speed*)))
	(:d (nv+ *camera-pos* (v* (vunit (vc *camera-front* *camera-up*))
				  *camera-speed*)))))))

(glfw:def-key-callback process-input (window key scancode action mod-keys)
  (process-input window key scancode action mod-keys))

(defun clamp (value floor ceiling)
  (min (max value floor) ceiling))

(defun process-mouse (window x-pos y-pos)
  (let ((x-offset (* (- x-pos *last-x*) *sensitivity*))
	(y-offset (* (- *last-y* y-pos) *sensitivity*)))
    (setf *last-x* x-pos)
    (setf *last-y* y-pos)

    (incf *yaw* x-offset)
    (setf *pitch* (clamp (+ *pitch* y-offset) -89 89)))

  (let* ((yaw (radians *yaw*))
	 (pitch (radians *pitch*))
	 (x (cos (* yaw pitch)))
	 (y (sin pitch))
	 (z (sin (* yaw pitch)))
	 (direction (vec x y z)))
    (setf *camera-front* (vunit direction))))
	 

(glfw:def-cursor-pos-callback process-cursor-pos (window x-pos y-pos)
  (process-mouse window x-pos y-pos))

(defun render ()
  (gl:clear-color 0.9 0.3 0.3 1.0)
  (gl:clear :color-buffer :depth-buffer)
  
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *texture*)

  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *face-texture*)

  (gl:use-program *shader-program*)

  (dotimes (i (length *instance-positions*))
    (let ((model (meye 4))
	  (view (mlookat *camera-pos* *camera-front* *camera-up*))
	  (projection (mperspective (radians 45) (/ 800 600) 0.1 1000)))
      (nmtranslate model (nth i *instance-positions*))
      (nmrotate model (vec (/ 0.5 (+ 0.1 i)) (* i 0.1) 0) (radians (+ 10 (* 10 i (glfw:get-time)))))

      (shader-set-uniform *shader-program* "view" (marr view))
      (shader-set-uniform *shader-program* "model" (marr model))
      (shader-set-uniform *shader-program* "projection" (marr projection)))
  
    (gl:bind-vertex-array *vao*)
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length *indices*)))

  (gl:bind-vertex-array 0))

(defun print-frame-rate (count t0)
  (let ((time (get-internal-real-time)))
    (when (= t0 0)
      (setq t0 time))
    (when (>= (- time t0)
	      (* 1 internal-time-units-per-second))
      (let* ((seconds (/ (- time t0) internal-time-units-per-second))
	     (fps (/ count seconds)))
	(format t "~d frames in ~3,1f seconds = ~6,3f fps"
		count seconds fps)
	(print-user-position)
	(format t "~%"))
      (setq t0 time)
      (setq count 0)
      t)))

(defun print-user-position ()
  (format t " POS: ~a - YAW: ~2d - PITCH: ~2d"
	  *camera-pos*
	  *yaw*
	  *pitch*))
      

(defun run ()
  (glfw:with-init
    (let ((time (get-internal-real-time))
	  (window (glfw:create-window :width 800
				      :height 600
				      :title "LearnOpenGL"
				      :context-version-major 3
				      :context-version-minor 3
				      :opengl-profile :opengl-core-profile)))

      (glfw:set-window-size-callback 'window-size-callback)
      (glfw:set-key-callback 'process-input)

      ;(glfw:set-input-mode :cursor :disabled window)
      (glfw:set-cursor-position-callback 'process-cursor-pos)
      
      (gl:viewport 0 0 800 600)
      (gl:enable :depth-test)

      (setup window)

      (loop for frame from 0
	    until (glfw:window-should-close-p window)
	    do (progn
		 (render)
		 
		 (glfw:swap-buffers window)
		 (glfw:poll-events)

		 (let ((current-frame (glfw:get-time)))
		   (setf *delta-time* (- current-frame *last-frame*))
		   (setf *last-frame* current-frame))
		 
		 (if (eql 0 (mod frame 200))
		     (let* ((new-time (get-internal-real-time))
			    (dt (- new-time time))
			    (seconds (/ dt internal-time-units-per-second))
			    (fps (/ 200.0 seconds)))
		       (format t "FPS: ~3,3d" fps)
		       (print-user-position)
		       (format t "~%")
		       (setq time new-time)))))
      (cleanup))))
