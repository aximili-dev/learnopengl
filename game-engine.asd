(asdf:defsystem :game-engine
  :description "Game engine"
  :version "0.1.0"
  :author "Daniel Litvak"
  :license "GPLv3"
  :depends-on (:3d-matrices
	       :3d-quaternions
	       :3d-transforms
	       :3d-vectors
	       :city-hash
	       :cl-glfw3
	       :cl-opengl
	       :cl-ppcre
	       :classimp
	       :pngload
	       :fiveam)
  :components ((:file "packages")
	       (:file "camera")
	       ;(:file "ecs")
	       (:file "engine")
	       (:file "entity")
	       (:file "glfw")
	       (:file "io")
	       ;(:file "mc")
	       (:file "mesh")
	       (:file "model")
	       (:file "opengl")
	       (:file "perlin")
	       (:file "shader")
	       ;(:file "simplex")
	       (:file "text-bitmap")
	       (:file "texture")
	       (:file "util")
	       (:file "wavefront")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem :game-engine/executable
  :description "Game engine executable"
  :version "0.1.0"
  :build-operation "program-op"
  :build-pathname "game-engine"
  :entry-point "game-engine:run-engine"
  :depends-on (:game-engine))

