(in-package :game-engine)

(defclass bitmap-font ()
  ((x-start :initarg :x-start)
   (y-start :initarg :y-start)
   (x-stride :initarg :x-stride)
   (y-stride :initarg :y-stride)
   (char-width :initarg :char-width)
   (char-height :initarg :char-height)
   (texture-width :initarg :texture-width)
   (texture-height :initarg :texture-height)
   (texture-id :initarg :texture-id)
   (program-id :initarg :program-id)
   (vao :initarg :vao)
   (vbo :initarg :vbo)
   (ebo :initarg :ebo)))

(defclass text-grid ()
  ((width :initarg :width)
   (height :initarg :height)
   (char-width :initarg :char-width)
   (char-height :initarg :char-height)
   (vbo :initarg :vbo)
   (vao :initarg :vao)))

(defparameter *char-vertices*
  #(0.0  0.0  0.0  1.0
    7.0  0.0  1.0  1.0
    7.0 11.0  1.0  0.0
    0.0 11.0  0.0  0.0))

(defparameter *char-indices*
  #(0 1 2
    0 2 3))

(defun generate-text-vertices (width height char)
  (loop for j from 0 below height
	collect (loop for i from 0 below width
		      collect (let ((i (float i))
				    (j (float j)))
				(list i j char 0 0
				      i j char 0 1
				      i j char 1 1
				      i j char 1 0)))))

(defun generate-text-indices (width height)
  (loop for i from 0 below (* width height)
	collect (let* ((tl (* i 4))
		       (tr (1+ tl))
		       (br (1+ tr))
		       (bl (1+ br)))
		  (list tl tr br
			tl br bl))))

(defparameter *initial-bmp-text* "Hello world")

(defun load-bitmap-font (path)
  (let* ((png    (pngload:load-file path :flatten t :flip-y t))
	 (width  (pngload:width  png))
	 (height (pngload:height png))
	 (texture-id (gl:gen-texture))
	 (vao (gl:gen-vertex-array))
	 (vbo (gl:gen-buffer))
	 (ebo (gl:gen-buffer))
	 (program-id (load-shader-program #P"./shaders/bmp_font.vert"
					  #P"./shaders/bmp_font.frag")))
    (gl:bind-texture :texture-2d texture-id)

    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d :texture-2d
		     0 :rgb
		     width height
		     0 :rgb :unsigned-byte
		     (pngload:data png))

    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer vbo)
    (let ((arr (make-gl-array *char-vertices* :float)))
      (gl:buffer-data :array-buffer
		      :dynamic-draw
		      arr)
      (gl:free-gl-array arr))

    (gl:bind-buffer :element-array-buffer ebo)
    (let ((arr (make-gl-array *char-indices* :unsigned-int)))
      (gl:buffer-data :element-array-buffer
		      :static-draw
		      arr)
      (gl:free-gl-array arr))

    (gl:vertex-attrib-pointer 0 2 :float :false (* 4 4) 0)
    (gl:enable-vertex-attrib-array 0)

    (gl:vertex-attrib-pointer 1 2 :float :false (* 4 4) (* 2 4))
    (gl:enable-vertex-attrib-array 1)

    (shader-set-uniform program-id "fontTexture" 0)

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
		   :program-id program-id
		   :vao vao
		   :vbo vbo
		   :ebo ebo)))

(defmethod get-char-tex-coords ((font bitmap-font) char)
  (let ((low (ldb (byte 4 0) (char-code char)))
	(high (ldb (byte 12 4) (char-code char))))
    (cons (+ 98 (* low 14))
	  (+ 66 (* (- high 2) 13)))))

(defmethod render-bmp-char ((font bitmap-font) char scale x y)
  (with-slots (texture-id texture-width texture-height program-id vao vbo char-width char-height) font
    (let ((model (meye 4))
	  (projection (mortho 0 800 600 0 -1 1000)))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d texture-id)

      (gl:use-program program-id)

      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      
      (gl:with-gl-mapped-buffer (arr :array-buffer :write-only :float)
	(let* ((tex-coords (get-char-tex-coords font char))
	       (x (car tex-coords))
	       (y (- texture-height (cdr tex-coords))))

	  (loop for i from 0 below 16
		collecting (gl:glaref arr i))
	  
	  (setf (gl:glaref arr 2) (float (/ x texture-width)))
	  (setf (gl:glaref arr 3) (float (/ y texture-height)))

	  (setf (gl:glaref arr 6) (float (/ (+ x char-width) texture-width)))
	  (setf (gl:glaref arr 7) (float (/ y texture-height)))

	  (setf (gl:glaref arr 10) (float (/ x texture-width)))
	  (setf (gl:glaref arr 11) (float (/ (- y char-height) texture-height)))

	  (setf (gl:glaref arr 14) (float (/ (+ x char-width) texture-width)))
	  (setf (gl:glaref arr 15) (float (/ (- y char-height) texture-height)))))

      (gl:get-program-info-log program-id)

      (nmscale model (vec scale scale 1))
      (nmtranslate model (vec (* scale 7 x)
			      (* scale 11 y)
			      1))

      (shader-set-uniform program-id "model" (marr model))
      (shader-set-uniform program-id "projection" (marr projection))

      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count 6))))

(defmethod render-bmp-text ((font bitmap-font) text scale x y)
  (with-slots (char-width) font
    (loop for c across text
	  for i from 0
	  with x 
	  do (render-bmp-char font c (+ x char-width) y))))
