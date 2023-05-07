(in-package :game-engine)

(defclass texture ()
  ((id
    :initarg :id
    :accessor texture-id
    :documentation "ID returned by glGenTexture")
   (width
    :initarg :width
    :accessor texture-width
    :documentation "Texture width in pixels")
   (height
    :initarg :height
    :accessor texture-height
    :documentation "Texture height in pixels")))
   

(defun load-texture (path &key (dont-flip-y))
  "Loads a texture from a file. Flips y axis by default."
  (let ((type (pathname-type path)))
    (cond ((equal type "png") (load-png-texture path :flip-y (not dont-flip-y)))
	  (t (error "~a textures are not supported!" type)))))

(defun load-png-texture (path &key flip-y)
  (let* ((png (pngload:load-file path :flatten t :flip-y flip-y))
	 (width (pngload:width png))
	 (height (pngload:height png))
	 (texture-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture-id)

    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d :texture-2d
		     0 :rgba
		     width height
		     0 :rgba :unsigned-byte
		     (pngload:data png))

    (gl:generate-mipmap :texture-2d)

    (make-instance 'texture
		   :id texture-id
		   :width width
		   :height height)))

(defmethod free-texture ((texture texture))
  (gl:delete-texture (texture-id texture)))
		     
