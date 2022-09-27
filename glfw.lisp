(in-package :game-engine)

(defmacro setup-callback (name fn window def-callback set-callback arg-list)
  `(progn
     (,def-callback ,name ,arg-list
       (funcall ,fn ,@arg-list))
     (,set-callback ',name ,window)))

(defun setup-callbacks (window &key resize keyboard cursor-pos cursor-enter)
  "Defines and sets up glfw callbacks."

  (if resize
      (setup-callback resize-cb resize
		      window
		      glfw:def-window-size-callback
		      glfw:set-window-size-callback
		      (win w h)))

  (if keyboard
      (setup-callback keyboard-cb keyboard
		      window
		      glfw:def-key-callback
		      glfw:set-key-callback
		      (win key scan action mod)))

  (if cursor-pos
      (setup-callback cursor-pos-cb cursor-pos
		      window
		      glfw:def-cursor-pos-callback
		      glfw:set-cursor-position-callback
		      (win x y)))

  (if cursor-enter
      (setup-callback cursor-enter-cb cursor-enter
		      window
		      glfw:def-cursor-enter-callback
		      glfw:set-cursor-enter-callback
		      (win entered))))
  
(defmacro with-glfw ((window
		     &key width height title (hide-cursor t))
		     &body body)
  `(glfw:with-init
     (let ((,window (glfw:create-window :width ,width
					:height ,height
					:title ,title
					:context-version-major 4
					:context-version-minor 3
					:opengl-profile :opengl-core-profile)))

       ,(if hide-cursor
	    `(glfw:set-input-mode :cursor :disabled))

       (gl:viewport 0 0 ,width ,height)
       
       (gl:enable :depth-test)

       ,@body)))
