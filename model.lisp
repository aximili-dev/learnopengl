(in-package :game-engine)

(defclass model ()
  ((meshes
    :accessor model-meshes
    :initarg :meshes
    :initform '())
   (directory
    :accessor model-directory
    :initform (error "Need to provide path to create model"))))
   
(defmethod render-model ((model model) shader)
  (with-slots (meshes) model
    (dolist (mesh meshes)
      (render-mesh mesh shader))))

(defmethod load-model ((model model) path)
  (let ((scene (ai:import-into-lisp path
				    :processing-flags '(:ai-process-triangulate
							:ai-process-flip-uvs))))
    (when ((or (not schene) (ai:scene-incomplete-p scene)))
      (error "ERROR: (assimp) ~s" (%ai:ai-get-error-string)))

    (setf (model-directory model) (pathname-directory path))

    (process-node model (ai:root-node scene) scene)))

(defmethod process-node ((model model) node schene)
  (dolist (mesh (ai:meshes node))
    (push (process-mesh mesh scene)
	  (model-meshes model)))

  (dolist (child (ai:children node))
    (process-node model child scene)))


(defun process-mesh (mesh scene)
  (let ((vertices '())
	(indices  '())
	(textures '()))
    (dotimes (i (length (ai:vertices mesh)))
      (let* ((vertex (aref (ai:vertices mesh) i))
	     (normal (aref (ai:normals mesh)  i))
	     (texture-coords (aref (ai:texture-coords) 0))
	     
	     (pos (vec (aref vertex 0)
		       (aref vertex 1)
		       (aref vertex 2)))
	     (nor (vec (aref normal 0)
		       (aref normal 1)
		       (aref normal 2)))
	     (tex (if texture-coords
		      (vec (aref (aref texture-coords i) 0)
			   (aref (aref texture-coords i) 1))
		      (vec 0 0))))
	(push vertices (make-instance 'vertex
				      :position   pos
				      :normal     nor
				      :tex-coords tex))))

    (loop for face across (ai:faces mesh)
	  do (loop for index across face
		   do (push index indices)))
				
    (when (>= (ai:material-index mesh) 0)
      (let* ((material (aref (ai:materials scene) (ai:material-index mesh)))
	     (diffuse-maps (load-mat-textures model mat :diffuse "texture_diffuse"))
	     (specular-maps (load-mat-textures model mat :specular "texture_specular")))
	(dolist (diff diffuse-maps)
	  (push diff textures))
	(dolist (spec specular-maps)
	  (push spec textures))))))
      
(defmethod load-mat-textures ((model model) mat tex-type type-name)
  (let ((textures '()))
    (dotimes (i (%ai:ai-get-material-texture-count mat tex-type))
      ())))
