(in-package :game-engine)

(defun read-file-to-string (path)
  (with-open-file (in path)
    (with-output-to-string (s)
      (loop for line = (read-line in nil)
	    while line do (format s "~a~%" line))
      s)))
