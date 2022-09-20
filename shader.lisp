(in-package :game-engine)

(defmacro gl-assert (object get-iv status-key get-info-log)
  "After-the-fact error checking for OpenGL

Use #'gl:get-shader and #'gl:get-shader-info-log for shaders.
Use #'gl:get-program and #'gl:get-program-info-log for programs."
  `(unless (,get-iv ,object ,status-key)
     (format t "Error")
     (error (format nil "Error in ~a ~a: ~a"
		    (function ,get-iv)
		    ,status-key
		    (,get-info-log ,object)))))

(defclass shader ()
  ((vs
    :initarg :vs
    :documentation "Vertex shader.")
   (fs
    :initarg :fs
    :documentation "Fragment shader.")
   (program
    :initarg :program
    :documentation "Shader program.")))

(defun create-shader (source type)
  "Creates a shader from source text."
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader (list source))
    (gl:compile-shader shader)
    (gl-assert shader
	       gl:get-shader :compile-status
	       gl:get-shader-info-log)
    shader))

(defun create-shader-program (vs fs)
  "Creates a vertex and fragment shader, and links them together in a program."
  (let ((program (gl:create-program)))
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    (gl-assert program
	       gl:get-program :link-status
	       gl:get-program-info-log)
    program))

(defun load-shader-from-disk (vertex-path fragment-path)
  (let* ((vs-source (read-file-to-string vertex-path))
	 (fs-source (read-file-to-string fragment-path))
	 (vs (create-shader vs-source :vertex-shader))
	 (fs (create-shader fs-source :fragment-shader))
	 (program (create-shader-program vs fs)))
    (make-instance 'shader
		   :vs vs
		   :fs fs
		   :program program)))

(defmethod shader-use ((shader shader))
  (with-slots (program) shader
    (gl:use-program program)))

(defmethod shader-set-uniform ((shader shader) location &rest vector)
  "Sets a uniform in a shader program. Changes the active shader."
  (with-slots ((program-id program)) shader
    (let ((location (gl:get-uniform-location program-id location))
	  (first (car vector)))
      (if (eql location -1)
	  (error "Couldn't find location ~a in program ~d" location program-id))
      (let ((func (cond ((typep first 'integer) #'gl:uniformi)
			((typep first 'real)    #'gl:uniformf)
			((typep first 'boolean) #'gl:uniformi)
			((typep first 'array)   #'gl:uniform-matrix-4fv)
			(t (error "Unrecognized type in vector")))))
	(gl:use-program program-id)
	(apply func location vector)))))

(defmethod shader-get-logs ((shader shader) stream)
  (with-slots (vs fs) shader
    (let ((vs-logs (gl:get-shader-info-log vs))
	  (fs-logs (gl:get-shader-info-log fs)))
      (format stream "VS: ~a~%" vs-logs)
      (format stream "FS: ~a~%" fs-logs))))

(defmethod shader-free ((shader shader))
  (with-slots (vs fs program) shader
    (gl:delete-shader vs)
    (gl:delete-shader fs)
    (gl:delete-program program)))
