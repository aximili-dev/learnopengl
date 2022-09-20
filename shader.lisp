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

(defun create-shader (source type)
  "Creates a shader from source text."
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader (list source))
    (gl:compile-shader shader)
    (gl-assert shader
	       gl:get-shader :compile-status
	       gl:get-shader-info-log)
    shader))

(defun create-shader-program (vertex-source fragment-source &optional (destroy-shaders t))
  "Creates a vertex and fragment shader, and links them together in a program."
  (let ((vs (create-shader vertex-source :vertex-shader))
	(fs (create-shader fragment-source :fragment-shader))
	(program (gl:create-program)))
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    (gl-assert program
	       gl:get-program :link-status
	       gl:get-program-info-log)
    (when destroy-shaders
      (format t "DEBUG: Deleting shaders after creating program~%")
      (gl:delete-shader vs)
      (gl:delete-shader fs))
    program))

(defun read-file-to-string (path)
  (with-open-file (in path)
    (with-output-to-string (s)
      (loop for line = (read-line in nil)
	    while line do (format s "~a~%" line))
      s)))

(defun load-shader-program (vertex-path fragment-path &optional (destroy-shaders t))
  (let ((vs (read-file-to-string vertex-path))
	(fs (read-file-to-string fragment-path)))
    (create-shader-program vs fs destroy-shaders)))

(defun shader-set-uniform (program-id location &rest vector)
  "Sets a uniform in a shader program. Changes the active shader."
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
      (apply func location vector))))
