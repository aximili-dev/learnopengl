(in-package :game-engine)

(defclass entity ()
  ((id
    :accessor entity-id
    :initarg :id)))

(defclass component ())

(defclass component-list ()
  ((components
    :accessor component-list-components
    :initform (vector))
   (component-type
    :accessor component-list-type
    :initarg :component-type
    :initform (error "Need to provide component type"))))

(defclass system ()
  ((process
    :accessor system-process
    :initarg :process
    :initform (error "Need to provide process function."))))

(defmacro defsystem (name lambda-list)
  '())

(defsystem render-system (transform mesh material)
  (render mesh material transform))
