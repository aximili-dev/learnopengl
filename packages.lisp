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
   :camera
   :fps-camera
   :entity
   :graphics
   :graphics-bmp-font
   :graphics-debug-text
   :graphics-keys
   :graphics-run
   :graphics-v-height
   :graphics-v-width
   :graphics-window
   :load-model
   :load-shader-from-disk
   :process-tick
   :render-entity
   :render-mesh
   :render-model
   :run-engine
   :shader-set-uniform
   :shader-use
   :shader-free
   :with-graphics
   :with-gl-array)
  (:shadow :speed))
