(in-package :example-02-meshes)

(defun run-example ()
  (with-graphics (graphics "02 Meshes"
		  :hide-cursor t
		  :window-width 800
		  :window-height 600)
    (let* ((camera (make-instance 'fps-camera
				  :sensitivity 0.1
				  :speed 10.0
				  :pos (vec 0 0 10)))
	   (model-shader (load-shader-from-disk #P"./shaders/model.vert"
						#P"./shaders/model.frag"))
	   (cube-model (load-model #P"./models/cube.obj"
				   :diffuse-path #P"./container.png"
				   :specular-path #P"./container.specular.png"))
	   (cube-entity (make-instance 'entity
				       :model cube-model
				       :shader model-shader
				       :transform (transform)))
	   (d20-model (load-model #P"./models/Dice_d20.obj"
				  :diffuse-path #P"./d20white.png"
				  :specular-path #P"./d20white.png"))
	   (d20-entity (make-instance 'entity
				      :model d20-model
				      :shader model-shader
				      :transform (transform (vec 4 1 4)
							    (vec 3 3 3))))
	   (light-positions (list (vec  0.7  0.2  2.0)
				  (vec  2.3 -3.3 -4.0)
				  (vec -4.0  2.0 -12.0)
				  (vec  0.0  0.0 -3.0))))
      (flet ((tick (time dt)
	       (process-tick camera time dt (graphics-keys graphics)))
	     (render (current-frame fps)
	       (gl:clear-color 0.21 0.36 0.43 1.0)
	       (gl:clear :color-buffer :depth-buffer)
	       
	       (shader-use model-shader)
      
	       ;; This abomination should probably be cleaned up somehow
	       (let ((i 0))
		 (macrolet ((set-light-prop (i prop &rest values)
			      `(let ((loc (format nil "pointLights[~d].~a" ,i ,prop)))
				 (shader-set-uniform model-shader loc ,@values))))
		   (dolist (light-pos light-positions)
		     (with-vec (x y z) light-pos
		       (set-light-prop i "position" x y z)
		       (set-light-prop i "constant" 1.0)
		       (set-light-prop i "linear" 0.09)
		       (set-light-prop i "quadratic" 0.032)
		       (set-light-prop i "ambient" 0.2 0.2 0.2)
		       (set-light-prop i "diffuse" 0.5 0.5 0.5)
		       (set-light-prop i "specular" 1.0 1.0 1.0))
		     (incf i))))

	       (render-entity d20-entity
			      (graphics-v-width graphics)
			      (graphics-v-height graphics)
			      camera)

	       (push (format nil "FPS: ~3,3f" fps) (graphics-debug-text graphics))))
	(graphics-run graphics #'render #'tick)))))
	

      

	  
	   
				  
