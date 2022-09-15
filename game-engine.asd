(asdf:defsystem :game-engine
  :description "Game engine"
  :version "0.1.0"
  :author "Daniel Litvak"
  :license "GPLv3"
  :depends-on (:cl-opengl
               :cl-glut
	       :cl-glu)
  :components ((:file "package")
	       (:file "opengl" :depends-on ("package"))))
