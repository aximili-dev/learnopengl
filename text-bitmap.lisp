(in-package :game-engine)

(defclass bitmap-font ()
  ((x-start
    :initarg :x-start)
   (y-start
    :initarg :y-start)
   (x-stride
    :initarg :x-stride)
   (y-stride
    :initarg :y-stride)
   (char-width
    :initarg :char-width)
   (char-height
    :initarg :char-height)
   (texture-width
    :initarg :texture-width)
   (texture-height
    :initarg :texture-height)
   (texture-id
    :initarg :texture-id)
   (program-id
    :initarg :program-id)))

(defparameter *initial-bmp-text*

(defun load-bitmap-font (path)
  (let* ((png    (pngload:load-file path :flatten t :flip-y t))
	 (width  (pngload:width  png))
	 (height (pngload:height png))
	 (texture-id (gl:gen-texture))
	 (program-id (load-shader-program #P"./shaders/bmp_font.vert"
					  #P"./shaders/bmp_font.frag")))
    (gl:bind-texture :texture-2d texture-id)

    (gl:tex-image-2d :texture-2d
		     0 :rgba
		     width height
		     0 :rgba :unsigned-byte
		     (pngload:data png))

    (make-instance 'bitmap-font
		   :x-start 98
		   :y-start 66
		   :x-stride 14
		   :y-stride 13
		   :char-width 7
		   :char-height 11
		   :texture-width width
		   :texture-height height
		   :texture-id texture-id
		   :program-id program-id)))

(defmethod render-bmp-char ((font bitmap-font) char x y)
  (with-slots (texture-id program-id) font
    (let ((view (meye 4))
	  (model (meye 4))
	  (projection (meye 4)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d texture-id)

    (gl:use-program program-id)

    (shader-set-uniform program-id "view" (marr view))
    (shader-set-uniform program-id "model" (marr model))
    (shader-set-uniform program-id "projection" (marr projection))

(defmethod render-bmp-text ((font bitmap-font) text x y)
  (with-slots (char-width) font
    (loop for c across text
	  for i from 0
	  with x 
	  do (render-bmp-char font c (+ x char-width) y))))
