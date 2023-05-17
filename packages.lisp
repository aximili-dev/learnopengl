(in-package :common-lisp-user)

(defpackage :wavefront
  (:use
   :common-lisp
   :3d-vectors
   :fiveam)
  (:export
   :parse-obj
   :with-vertex
   :vertex-position
   :vertex-normal
   :vertex-tex-coords))

(defpackage :perlin
  (:use
   :common-lisp
   :3d-vectors
   :fiveam)
  (:export
   :perlin))

(defpackage :game-engine
  (:use
   :common-lisp
   :3d-vectors
   :3d-matrices
   :org.shirakumo.flare.quaternion
   :org.shirakumo.flare.transform
   :fiveam
   :perlin)
  (:export
   :run-engine)
  (:shadow :speed))
