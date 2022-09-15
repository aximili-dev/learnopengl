(in-package :dev.litvak.game-engine)

(defclass gl-window (glut:window)
  ()
  (:default-initargs
   :width 800
   :height 600
   :title "opengl.lisp"
   :mode '(:double :rgb :depth)))

(defun make-gl-array (array type)
  (let* ((n-elements (length array))
	 (arr (gl:alloc-gl-array type n-elements)))
    (dotimes (i n-elements)
      (setf (gl:glaref arr i) (aref array i)))
    arr))

(defparameter *vertices*
  #( 0.5  0.5  0.0
     0.5 -0.5  0.0
    -0.5 -0.5  0.0
    -0.5  0.5  0.0))

(defparameter *indices*
  #(0 1 3
    1 2 3))

(defvar *vertex-shader-source*
  (with-output-to-string (s)
    (format s "#version 330 core~%")
    (format s "layout (location = 0) in vec3 aPos;~%")
    (format s "void main ()~%")
    (format s "{~%")
    (format s "  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);~%")
    (format s "}~c" #\Nul)))

(defvar *vertex-shader*)

(defvar *fragment-shader-source*
  (with-output-to-string (s)
    (format s "#version 330 core~%")
    (format s "out vec4 FragColor;~%")
    (format s "void main()~%")
    (format s "{~%")
    (format s "  FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);~%")
    (format s "}~c" #\Nul)))

(defvar *fragment-shader*)

(defvar *shader-program*)

(defvar *vbo*)
(defvar *vao*)
(defvar *ebo*)

(defun assert-compile (shader)
  (unless (gl:get-shader shader :compile-status)
    (error (format nil "Error in shader: ~a"
		   (gl:get-shader-info-log shader)))))

(defmacro gl-assert (object get-iv status-key get-info-log)
  `(unless (,get-iv ,object ,status-key)
     (format t "Error")
     (error (format nil "Error in ~a ~a: ~a"
		    (function ,get-iv)
		    ,status-key
		    (,get-info-log ,object)))))
   
(defmethod glut:display-window :before ((window gl-window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1)
  (gl:shade-model :flat)

  ;; Compile vertex shader
  (setf *vertex-shader* (gl:create-shader :vertex-shader))
  (gl:shader-source *vertex-shader* (list *vertex-shader-source*))
  (gl:compile-shader *vertex-shader*)
  (gl-assert *vertex-shader*
	     gl:get-shader :compile-status
	     gl:get-shader-info-log)

  ;; Compile fragment shader
  (setf *fragment-shader* (gl:create-shader :fragment-shader))
  (gl:shader-source *fragment-shader* (list *fragment-shader-source*))
  (gl:compile-shader *fragment-shader*)
  (gl-assert *fragment-shader*
	     gl:get-shader :compile-status
	     gl:get-shader-info-log)

  ;;Set shader program
  (setf *shader-program* (gl:create-program))
  (gl:attach-shader *shader-program* *vertex-shader*)
  (gl:attach-shader *shader-program* *fragment-shader*)
  (gl:link-program *shader-program*)
  (gl-assert *shader-program*
	     gl:get-program :link-status
	     gl:get-program-info-log)

  ;; Clean up shaders after linking them in the program
  (gl:delete-shader *vertex-shader*)
  (gl:delete-shader *fragment-shader*)

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
  (gl:vertex-attrib-pointer 0 3 :float :false (* 3 4) 0)
  (gl:enable-vertex-attrib-array 0)

  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0))

(defmethod glut:display ((window gl-window))
  (gl:clear-color 0.2 0.3 0.3 1.0)
  (gl:clear :color-buffer)
  
  ;; Use our shader program
  (gl:use-program *shader-program*)
  (gl:bind-vertex-array *vao*)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count 6)
  (gl:bind-vertex-array 0)

  (glut:swap-buffers))

(defmethod glut:reshape ((w gl-window) width height)
  (gl:viewport 0 0 width height))

(defmethod glut:keyboard ((w gl-window) key x y)
  (format t "Got key: ~@c (~d ~d)~%" key x y)
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:close ((w gl-window))
  (gl:delete-vertex-arrays (list *vao*))
  (gl:delete-buffers (list *vbo*))
  (gl:delete-buffers (list *ebo*))
  (gl:delete-program *shader-program*))

(defun show-window ()
  (let ((w (make-instance 'gl-window)))
    ;(unwind-protect
	 (glut:display-window w)))
      ;(when (not (glut::destroyed w))
	;(setf (glut::destroyed w) t)
	;(glut:destroy-window (glut:id w))))))

(defun run ()
  (let ((glut:*run-main-loop-after-display* nil))
    (show-window)
    (glut:main-loop)))
