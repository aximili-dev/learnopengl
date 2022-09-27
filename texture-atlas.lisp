(in-package :game-engine)

(defclass texture-atlas ()
  ((texture
    :initarg :texture
    :accessor texture-atlas-texture
    :documentation "The underlying texture.")
   (tile-width
    :initarg :tile-width
    :accessor texture-atlas-tile-width
    :documentation "Width in pixels for each tile in the atlas.")
   (tile-height
    :initarg :tile-height
    :accessor texture-atlas-tile-height
    :documentation "Height in pixels for each tile in the atlas.")))

(defmacro with-atlas-props ((n-tiles) atlas &body body)
  (let ((atlas-sym (gensym)))
    `(let* ((,atlas-sym ,atlas)
	    (,n-tiles (/ (texture-width (texture-atlas-texture ,atlas-sym))
			 (texture-atlas-tile-width ,atlas-sym))))
       ,@body)))

(defmethod texture-atlas-coordinates ((atlas texture-atlas) index)
  (with-slots (texture tile-width tile-height) atlas
    (with-atlas-props (n-tiles) atlas
      (let* ((tex-width (texture-width texture))
	     (tex-height (texture-height texture))
	     (x (* tile-width (mod index n-tiles)))
	     (y (* tile-height (floor (/ index n-tiles))))
	     (x (/ x tex-width))
	     (y (1- (/ y tex-height)))
	     (width (/ tile-width tex-width))
	     (height (/ tile-height tex-height)))
	(list (cons x           (- y height))
	      (cons x           y)
	      (cons (+ x width) y)
	      (cons (+ x width) (- y height)))))))
		   
