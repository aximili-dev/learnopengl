(asdf:defsystem :game-engine
  :description "Game engine"
  :version "0.1.0"
  :author "Daniel Litvak"
  :license "GPLv3"
  :depends-on (:3d-matrices
	       :3d-vectors
	       :cl-opengl
	       :cl-glfw3
	       :pngload)
  :components ((:file "packages")
	       (:file "camera")
	       (:file "opengl")
	       (:file "shader")
	       (:file "text-bitmap")
	       (:file "texture")
	       (:file "util")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem :game-engine/executable
  :description "Game engine executable"
  :version "0.1.0"
  :build-operation "program-op"
  :build-pathname "game-engine"
  :entry-point "game-engine:run"
  :depends-on (:game-engine))
