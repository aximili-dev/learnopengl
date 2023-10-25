(in-package :common-lisp-user)

(defpackage :example-01-triangle
  (:use
   :common-lisp
   :game-engine)
  (:export
   :run-example))

(defpackage :example-02-meshes
  (:use
   :common-lisp
   :game-engine
   :3d-vectors
   :3d-matrices
   :org.shirakumo.flare.quaternion
   :org.shirakumo.flare.transform
   :wavefront)
  (:export
   :run-example))

(defpackage :example-04-marching-cubes
  (:use
   :common-lisp
   :game-engine
   :perlin
   :infix-math
   :3d-vectors)
  (:export
   :run-example))
