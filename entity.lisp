(in-package :game-engine)

(defclass entity ()
  ((model
    :documentation "An instance of the model class"
    :accessor entity-model
    :initarg :model
    :initform '())
   (shader
    :documentation "An instance of the shader class"
    :accessor entity-shader
    :initarg :shader
    :initform '())
   (transform
    :accessor entity-transform
    :initarg :transform
    :initform (transform))
   (specular-index
    :accessor entity-specular-index
    :initarg :specular-index
    :initform '())
   (diffuse-index
    :accessor entity-diffuse-index
    :initarg :diffuse-index
    :initform '())))

(defmethod render-entity ((entity entity) v-width v-height camera)
  (with-slots (model shader transform diffuse-index specular-index) entity
    (shader-set-uniform shader "material.shininess" 32.0)
    (shader-set-uniform shader "material.diffuse" 0)
    (shader-set-uniform shader "material.specular" 1)

    (shader-set-uniform shader "dirLight.direction" 1.0 -1 1)
    (shader-set-uniform shader "dirLight.ambient" 0.2 0.2 0.2)
    (shader-set-uniform shader "dirLight.diffuse" 0.5 0.5 0.5)
    (shader-set-uniform shader "dirLight.specular" 1.0 1.0 1.0)

    (let ((view (view-matrix camera))
	  (projection (mperspective 45 (/ v-width v-height) 0.1 1000)))

      (with-slots (pos) camera
	(with-vec (x y z) pos
	  (shader-set-uniform shader "viewPos" x y z)))

      (shader-set-uniform shader "view" (marr view))
      (shader-set-uniform shader "projection" (marr projection))
      (shader-set-uniform shader "model" (marr (tmat4 transform))))

    (render-model model shader :diffuse-location "material.diffuse" :specular-location "material.specular")))
