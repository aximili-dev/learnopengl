(in-package :game-engine)

(defclass engine ()
  ((world
    :accessor world
    :initform '()
    :documentation "Entity Tree. Uses entity IDs")
   (entities
    :accessor entities
    :initform '()
    :documentation "List of actual entities.")
   (camera
    :accessor camera
    :initform '()
    :documentation "Holds the camera")
   (shaders
    :accessor shaders
    :initform '()
    :documentation "An alist where the key is the type of shader program")
   (textures
    :accessor textures
    :initform '()
    :documentation "An alist where the key is the name of a texture, and the value is the ID"))
  (:documentation "Holds all the information for the engine to run"))
