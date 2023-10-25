;;;;
;;;; resource.lisp
;;;;

(in-package :game-engine)

(defparameter *next-resource-id* 0)

;;; Resource class
(defclass resource ()
  ((id :initarg :id
       :documentation "Unique identifier for this resource"))
  (:documentation "Base class for all engine resources."))
   
(defgeneric load-resource ()
  "Loads a resource into memory.

Before the program ends, unload-resource should be called")

;;; OpenGL resources
(defclass gl-resource ()
  ((gl-name :initarg :gl-name
	    :documentation "The identifier returned by OpenGL when creating the resource"))
  (:documentation "A resource in OpenGL"))



(defclass file-resource ()
  ((path :initarg :path
	 :documentation "The filesystem path to this resource"))
  (:documentation "A resource in a file system"))
