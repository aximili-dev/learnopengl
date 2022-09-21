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
   (ebo :initarg :ebo)
   (v-width :initarg :v-width)
   (v-height :initarg :v-height)))

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

(defun load-bitmap-font (path v-width v-height)
  (let* ((png    (pngload:load-file path :flatten t :flip-y t))
	 (width  (pngload:width  png))
	 (height (pngload:height png))
	 (texture-id (gl:gen-texture))
	 (vao (gl:gen-vertex-array))
	 (vbo (gl:gen-buffer))
	 (ebo (gl:gen-buffer))
	 (program-id (load-shader-from-disk #P"./shaders/bmp_font.vert"
					    #P"./shaders/bmp_font.frag")))
    (gl:bind-texture :texture-2d texture-id)

    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

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
		   :ebo ebo
		   :v-width v-width
		   :v-height v-height)))

(defmethod unload-bitmap-font ((font bitmap-font))
  (with-slots (vao vbo ebo program-id texture-id) font
    (gl:delete-vertex-arrays (list vao))
    (gl:delete-buffers (list vbo ebo))
    (gl:delete-texture texture-id)

    (shader-free program-id)))

(defmethod get-char-tex-coords ((font bitmap-font) char)
  (let ((low (ldb (byte 4 0) (char-code char)))
	(high (ldb (byte 12 4) (char-code char))))
    (cons (+ 98 (* low 14))
	  (+ 66 (* (- high 2) 13)))))

(defmethod render-bmp-char ((font bitmap-font) char scale char-x char-y)
  (with-slots (texture-id texture-width texture-height
	       program-id vao vbo
	       char-width char-height
	       v-width v-height) font
    (let ((model (meye 4))
	  (projection (mortho 0 v-width v-height 0 -1 1000))
	  (texture-id texture-id))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d texture-id)

      (shader-use program-id)

      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      
      (let* ((tex-coords (get-char-tex-coords font char))
	     (xl (float (car tex-coords)))
	     (yt (float (- texture-height (cdr tex-coords))))
	     (xr (+ xl char-width))
	     (yb (- yt char-height))
	     (vertices (list 0.0  0.0  (/ xl texture-width) (/ yt texture-height)
			     7.0  0.0  (/ xr texture-width) (/ yt texture-height)
			     7.0 11.0  (/ xr texture-width) (/ yb texture-height)
			     0.0 11.0  (/ xl texture-width) (/ yb texture-height)))
	     (gl-arr (make-gl-array (make-array (length vertices) :initial-contents vertices) :float)))
	(gl:buffer-sub-data :array-buffer gl-arr)
	(gl:free-gl-array gl-arr))

      (nmscale model (vec scale scale 1))
      (nmtranslate model (vec (* 7 char-x)
			      (* 11 char-y)
			      1))

      (shader-set-uniform program-id "model" (marr model))
      (shader-set-uniform program-id "projection" (marr projection))

      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count 6))))

(defmethod render-bmp-text ((font bitmap-font) text scale x y)
  (with-slots (char-width) font
    (loop for c across text
	  for i from 0
	  do (render-bmp-char font c scale (+ x i) y))))
