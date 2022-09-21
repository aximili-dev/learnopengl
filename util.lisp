(in-package :game-engine)

(defun degrees (radians)
  (/ (* radians 180) pi))

(defun radians (degrees)
  (/ (* degrees pi) 180))

(defun clamp (value floor ceiling)
  (min (max value floor) ceiling))
