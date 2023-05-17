(in-package :game-engine)

(defun vertex-attrib-list (v)
  (wavefront:with-vertex (position normal tex-coords) v
    (with-vec (px py pz) position
      (with-vec (nx ny nz) normal
	(with-vec (tx ty) tex-coords
	  (list px py pz
		nx ny nz
		tx ty))))))

(defclass mesh ()
  ((vertices
    :initarg :vertices
    :initform '())
   (indices
    :initarg :indices
    :initform '())
   (vao :initarg :vao)
   (vbo :initarg :vbo)
   (ebo :initarg :ebo)))

(defun load-mesh (path)
  (multiple-value-bind (vertices indices) (wavefront:parse-obj path)
    (create-mesh vertices indices)))

(defun create-mesh (vertices indices)
  ;;; Initialize our OpenGL resources
  (let ((vao (gl:gen-vertex-array))
	(vbo (gl:gen-buffer))
	(ebo (gl:gen-buffer)))
    ;;; Bind the Vertex Array Object for this mesh
    ;;; It will store references to the bound VBO and EBO
    (gl:bind-vertex-array vao)
    
    ;;; Write our mesh to OpenGL buffers
    (let* ((vertex-attrib-lists (map 'list #'vertex-attrib-list vertices))
	   (vertex-attrib-arr   (apply #'concatenate 'vector vertex-attrib-lists))
	   (gl-arr (make-gl-array vertex-attrib-arr :float)))
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))

    (let* ((indices-arr (make-array (length indices) :initial-contents indices))
	   (gl-arr (make-gl-array indices-arr :unsigned-int)))
      (gl:bind-buffer :element-array-buffer ebo)
      (gl:buffer-data :element-array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))

    ;;; Configure the vertex format
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float :false (* 8 4) 0)

    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 3 :float :false (* 8 4) (* 3 4))

    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 2 :float :false (* 8 4) (* 6 4))

    (gl:bind-vertex-array 0)

    (make-instance 'mesh
		   :vertices vertices
		   :indices indices
		   :vao vao
		   :vbo vbo
		   :ebo ebo)))

(defmethod free-mesh ((mesh mesh))
  (with-slots (vao vbo ebo) mesh
    (gl:delete-vertex-arrays (list vao))
    (gl:delete-buffers (list vbo ebo))))
		      
(defmethod render-mesh ((mesh mesh))
  (with-slots (indices textures vao vbo ebo) mesh
    (gl:bind-vertex-array vao)
    (gl:draw-elements :triangles
		      (gl:make-null-gl-array :unsigned-int)
		      :count (length indices))
    (gl:bind-vertex-array 0)))

(defmacro build-mesh ((vertices indices textures) &body body)
  `(let ((,vertices '())
	 (,indices '())
	 (,textures '()))
     ,@body
     (create-mesh ,vertices ,indices ,textures)))
