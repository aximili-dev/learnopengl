(in-package :game-engine)

(defclass vertex ()
  ((position
    :initarg :position)
   (normal
    :initarg :normal)
   (tex-coords
    :initarg :tex-coords)))

(defmethod vertex-attrib-list ((v vertex))
  (with-slots (position normal tex-coords) v
    (with-vec (px py pz) position
      (with-vec (nx ny nz) normal
	(with-vec (tx ty) tex-coords
	  (list px py pz
		nx ny nz
		tx ty))))))

(defun vertex (position normal tex-coords)
  (make-instance 'vertex
		 :position position
		 :normal normal
		 :tex-coords tex-coords))

(defclass texture ()
  ((id
    :initarg :id)
   (type
    :initarg :type)
   (path
    :initarg :path)))

(defclass mesh ()
  ((vertices
    :initarg :vertices
    :initform '())
   (indices
    :initarg :indices
    :initform '())
   (textures
    :initarg :indices
    :initform '())
   (vao :initarg :vao)
   (vbo :initarg :vbo)
   (ebo :initarg :ebo)))

(defun create-mesh (vertices indices textures)
  (let ((vao (gl:gen-vertex-array))
	(vbo (gl:gen-buffer))
	(ebo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)

    (let* ((vertex-attrib-lists (mapcar #'vertex-attrib-list vertices))
	   (vertex-attrib-arr   (apply #'concatenate 'vector vertex-attrib-lists))
	   (gl-arr (make-gl-array vertex-attrib-arr :float)))
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))

    (let ((indices-arr (make-array (length indices) :initial-contents indices))
	  (gl-arr (make-gl-array indices-arr :unsigned-int)))
      (gl:bind-buffer :element-array-buffer ebo)
      (gl:buffer-data :element-array-buffer :static-draw gl-arr)
      (gl:free-gl-array gl-arr))

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
		   :textures textures
		   :vao vao
		   :vbo vbo
		   :ebo ebo)))
		      
(defmethod render-mesh ((mesh mesh) shader)
  (with-slots (indices textures vao) mesh
    (loop with diffuse-nr = 0
	  with specular-nr = 0
	  
	  for texture in textures
	  for i from 0

	  with id = (slot-value texture 'id)
	  with type = (slot-value texture 'type)

	  do (progn (gl:active-texture i)

		    (let* ((number (case type
				     (:diffuse (progn (incf diffuse-nr) diffuse-nr))
				     (:specular (progn (incf specular-nr) specular-nr))))
			   (location (format nil "material.~(~a~)_~d" type number)))
		      (shader-set-uniform shader location id))))

    (gl:active-texture 0)

    (gl:bind-vertex-array vao)
    (gl:draw-elements :triangles (gl-make-null-gl-array :unsigned-int))
    (gl:bind-vertex-array 0)))
