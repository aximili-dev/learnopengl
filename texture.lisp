(in-package :game-engine)

(defun load-texture (path &key (dont-flip-y))
  "Loads a texture from a file. Flips y axis by default."
  (let ((type (pathname-type path)))
    (cond ((equal type "png") (load-png-texture path :flip-y (not dont-flip-y)))
	  (t (error "~a textures are not supported!" type)))))

(defun load-png-texture (path &key flip-y)
  (let ((png (pngload:load-file path :flatten t :flip-y flip-y))
	(texture-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture-id)

    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d :texture-2d
		     0 :rgba
		     (pngload:width png)
		     (pngload:height png)
		     0 :rgba :unsigned-byte
		     (pngload:data png))

    (gl:generate-mipmap :texture-2d)

    texture-id))
		     
