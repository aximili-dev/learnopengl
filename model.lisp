(in-package :game-engine)

(defclass model ()
  ((mesh
    :accessor model-mesh
    :initarg :mesh
    :initform (error "Need to provide a mesh"))
   (diffuse-texture
    :accessor model-diffuse
    :initarg :diffuse
    :initform (error "Need to provide diffuse texture"))
   (specular-texture
    :accessor model-specular
    :initarg :specular
    :initform (error "Need to provide specular texture"))))

(defun load-model (mesh-path &key diffuse-path specular-path)
  (let ((mesh (load-mesh mesh-path))
	(diffuse (if diffuse-path (load-texture diffuse-path)))
	(specular (if specular-path (load-texture specular-path))))
    (make-instance 'model
		   :mesh mesh
		   :diffuse diffuse
		   :specular specular)))

(defmethod free-model ((model model))
  (with-slots (diffuse-texture specular-texture) model
    (free-texture diffuse-texture)
    (free-texture specular-texture)))

(defmethod render-model ((model model) shader &key diffuse-location specular-location)
  (with-slots (mesh diffuse-texture specular-texture) model
    (when diffuse-location
      (gl:active-texture 0)
      (gl:bind-texture :texture-2d (texture-id diffuse-texture))
      (shader-set-uniform shader diffuse-location (texture-id diffuse-texture)))
    (when specular-location
      (gl:active-texture 1)
      (gl:bind-texture :texture-2d (texture-id specular-texture))
      (shader-set-uniform shader specular-location (texture-id specular-texture)))
    (render-mesh mesh)))
