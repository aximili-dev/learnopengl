(in-package :game-engine)

(defun parse-obj (input-path)
  "Parses a Wavefront OBJ file and returns a model"
  (let ((vertex-hash-table (make-hash-table :test 'equalp))
	(vertices (make-array 0 :adjustable t :fill-pointer 0))
	(indices (make-array 0 :adjustable t :fill-pointer 0))
	(faces
	  (with-open-file (in input-path :direction :input)
	    (loop for line = (read-line in nil)
		  while line
		  for type = (subseq line 0 2)
		  
		  if (string= type "v ")
		    collect (parse-vertex line)
		      into vertices
	  
		  if (string= type "vt")
		    collect (parse-tex-coord line)
		      into tex-coords
	  
		  if (string= type "vn")
		    collect (parse-vertex-normal line)
		      into normals

		  if (string= type "f ")
		    collect (parse-face line vertices tex-coords normals)))))

    (loop for face in faces
	  do (loop for vertex in face
		   do (if (gethash vertex vertex-hash-table)
			  (vector-push-extend (gethash vertex vertex-hash-table) indices)
			  (progn
			    (setf (gethash vertex vertex-hash-table) (length vertices))
			    (vector-push-extend (length vertices) indices)
			    (vector-push-extend vertex vertices)))))

    (values vertices indices)))
				  
	  
(defun parse-vertex (line)
  "Parses a vertex line and returns a vector with x, y and z coordinates"
  (let ((coords (cl-ppcre:split " " (subseq line 2))))
    (vec (parse-float:parse-float (first coords))
	 (parse-float:parse-float (second coords))
	 (parse-float:parse-float (third coords)))))

(defun parse-tex-coord (line)
  "Parses a vertex texture coordinate line and returns a vector with x and y coordinates"
  (let ((coords (cl-ppcre:split " " (subseq line 3))))
    (vec (parse-float:parse-float (first coords))
	 (parse-float:parse-float (second coords)))))

(defun parse-vertex-normal (line)
  "Parses a vertex normal line and returns a vector with x, y and z coordinates"
  (let ((coords (cl-ppcre:split " " (subseq line 3))))
    (vec (parse-float:parse-float (first coords))
	 (parse-float:parse-float (second coords))
	 (parse-float:parse-float (third coords)))))

(defun parse-face (line vertices tex-coords normals)
  (let ((indices (cl-ppcre:split " " (subseq line 2))))
    (cond
      ((cl-ppcre:scan "^\\d+$" (first indices)) (parse-face-v indices vertices))
      ((cl-ppcre:scan "^\\d+\\/\\d+$" (first indices)) (parse-face-vt indices vertices tex-coords))
      ((cl-ppcre:scan "^\\d+\\/\\d+\\/\\d+$" (first indices)) (parse-face-vtn indices vertices tex-coords normals))
      ((cl-ppcre:scan "^\\d+\\/\\/\\d+$" (first indices)) (parse-face-vn indices vertices normals)))))

(defun parse-face-v (indices vertices)
  (mapcar #'(lambda (index)
	      (make-vertex
	       :position (nth (parse-integer index) vertices)
	       :tex-coords (vec 0 0)
	       :normal (vec 0 0 0)))
	  indices))
  
(defun parse-face-vt (indices vertices tex-coords)
  (mapcar #'(lambda (vt-indices)
	      (cl-ppcre:register-groups-bind ((#'parse-integer v vt)) ("(\\d+)\\/(\\d+)" vt-indices)
		(make-vertex
		 :position (nth v vertices)
		 :tex-coords (nth vt tex-coords)
		 :normal (vec 0 0 0))))
	  indices))

(defun parse-face-vtn (indices vertices tex-coords normals)
  (mapcar #'(lambda (vtn-indices)
	      (cl-ppcre:register-groups-bind ((#'parse-integer v vt vn)) ("(\\d+)\\/(\\d+)\\/(\\d+)" vtn-indices)
		(make-vertex
		 :position (nth v vertices)
		 :tex-coords (nth vt tex-coords)
		 :normal (nth vn normals))))
	  indices))

(defun parse-face-vn (indices vertices normals)
  (mapcar #'(lambda (vn-indices)
	      (cl-ppcre:register-groups-bind ((#'parse-integer v vn)) ("(\\d+)\\/\\/(\\d+)" vn-indices)
		(make-vertex
		 :position (nth v vertices)
		 :tex-coords nil
		 :normal (nth vn normals))))
	  indices))
