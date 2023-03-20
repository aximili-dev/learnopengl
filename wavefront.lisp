(in-package :wavefront)

(defstruct vertex
  position
  tex-coords
  normal)

(defun parse-obj (input-path)
  "Parses a Wavefront OBJ file and returns a mesh by its vertices and indices"
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
  "Parses a face by its indices and returns a list of 3 vertex structs"
  (let ((indices (cl-ppcre:split " " (subseq line 2))))
    (cond
      ((cl-ppcre:scan "^\\d+$" (first indices)) (parse-face-v indices vertices))
      ((cl-ppcre:scan "^\\d+\\/\\d+$" (first indices)) (parse-face-vt indices vertices tex-coords))
      ((cl-ppcre:scan "^\\d+\\/\\d+\\/\\d+$" (first indices)) (parse-face-vtn indices vertices tex-coords normals))
      ((cl-ppcre:scan "^\\d+\\/\\/\\d+$" (first indices)) (parse-face-vn indices vertices normals)))))

(defun parse-face-v (indices vertices)
  (make-face-v (mapcar #'(lambda (index) (parse-integer index))
		       indices)
	       vertices))

(defun make-face-v (indices vertices)
  "Make a face using only vertex positions"
  (mapcar #'(lambda (index)
	      (make-vertex
	       :position (vcopy (nth index vertices))
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
		 :tex-coords (vec 0 0)
		 :normal (nth vn normals))))
	  indices))

;; Unit tests
(def-suite wavefront
  :description "Test wavefront functions")

(in-suite wavefront)

(test parse-vertex
  (let ((result (parse-vertex "v 0 0 0")))
    (is (v= (vec 0 0 0) result))))

(test parse-vertex-values
  (let ((result (parse-vertex "v 1 2 3.5")))
    (is (v= (vec 1 2 3.5) result))))

(test parse-tex-coord
  (let ((result (parse-tex-coord "vt 0 0")))
    (is (v= (vec 0 0) result))))

(test parse-tex-coord-values
  (let ((result (parse-tex-coord "vt 5 3.2")))
    (is (v= (vec 5 3.2) result))))

(test parse-vertex-normal
  (let ((result (parse-vertex-normal "vn 0 0 0")))
    (is (v= (vec 0 0 0) result))))

(test parse-vertex-normal-values
  (let ((result (parse-vertex-normal "vn 9 3 2.2")))
    (is (v= (vec 9 3 2.2) result))))

(test make-face-v
  (let* ((v0 (vec 1 2 3))
	 (v1 (vec 2 3 4))
	 (v2 (vec 3 4 5))
	 (vec2-0 (vec 0 0))
	 (vec3-0 (vec 0 0 0))
	 (vertices (list v0 v1 v2))
	 (indices '(0 1 2))
	 (result (make-face-v indices vertices)))
    (is (equalp (list (make-vertex
		       :position (vcopy v0)
		       :tex-coords (vcopy vec2-0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v1)
		       :tex-coords (vcopy vec2-0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v2)
		       :tex-coords (vcopy vec2-0)
		       :normal (vcopy vec3-0)))
		result))))

(test parse-face-v
  (let* ((v0 (vec 1 2 3))
	 (v1 (vec 2 3 4))
	 (v2 (vec 3 4 5))
	 (vec2-0 (vec 0 0))
	 (vec3-0 (vec 0 0 0))
	 (vertices (list v0 v1 v2))
	 (indices '("0" "1" "2"))
	 (result (parse-face-v indices vertices)))
    (is (equalp (list (make-vertex
		       :position (vcopy v0)
		       :tex-coords (vcopy vec2-0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v1)
		       :tex-coords (vcopy vec2-0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v2)
		       :tex-coords (vcopy vec2-0)
		       :normal (vcopy vec3-0)))
		result))))

(test parse-face-vt-same-tex-coord
  (let* ((v0 (vec 1 2 3))
	 (v1 (vec 2 3 4))
	 (v2 (vec 3 4 5))
	 (t0 (vec 0.0 1.0))
	 (vec3-0 (vec 0 0 0))
	 (vertices (list v0 v1 v2))
	 (tex-coords (list t0))
	 (indices '("0/0" "1/0" "2/0"))
	 (result (parse-face-vt indices vertices tex-coords)))
    (is (equalp (list (make-vertex
		       :position (vcopy v0)
		       :tex-coords (vcopy t0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v1)
		       :tex-coords (vcopy t0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v2)
		       :tex-coords (vcopy t0)
		       :normal (vcopy vec3-0)))
		result))))

(test parse-face-vt-different-tex-coords
  (let* ((v0 (vec 1 2 3))
	 (v1 (vec 2 3 4))
	 (v2 (vec 3 4 5))
	 (t0 (vec 0.0 1.0))
	 (t1 (vec 0.5 0.1))
	 (t2 (vec 0.3 0.9))
	 (vec3-0 (vec 0 0 0))
	 (vertices (list v0 v1 v2))
	 (tex-coords (list t0 t1 t2))
	 (indices '("0/0" "1/1" "2/2"))
	 (result (parse-face-vt indices vertices tex-coords)))
    (is (equalp (list (make-vertex
		       :position (vcopy v0)
		       :tex-coords (vcopy t0)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v1)
		       :tex-coords (vcopy t1)
		       :normal (vcopy vec3-0))
		      (make-vertex
		       :position (vcopy v2)
		       :tex-coords (vcopy t2)
		       :normal (vcopy vec3-0)))
		result))))

(test parse-face-vtn
  (let* ((v0 (vec 1 2 3))
	 (v1 (vec 2 3 4))
	 (v2 (vec 3 4 5))
	 (t0 (vec 0.0 1.0))
	 (t1 (vec 0.3 1.0))
	 (t2 (vec 0.9 0.05))
	 (n0 (vec 0.9 1.0))
	 (n1 (vec 0.3 0.0))
	 (n2 (vec 0.9 0.05))
	 (vertices (list v0 v1 v2))
	 (tex-coords (list t0 t1 t2))
	 (normals (list n0 n1 n2))
	 (indices '("0/0/0" "1/1/1" "2/2/2"))
	 (result (parse-face-vtn indices vertices tex-coords normals)))
    (is (equalp (list (make-vertex
		       :position (vcopy v0)
		       :tex-coords (vcopy t0)
		       :normal (vcopy n0))
		      (make-vertex
		       :position (vcopy v1)
		       :tex-coords (vcopy t1)
		       :normal (vcopy n1))
		      (make-vertex
		       :position (vcopy v2)
		       :tex-coords (vcopy t2)
		       :normal (vcopy n2)))
		result))))

(test parse-face-vn
  (let* ((v0 (vec 1 2 3))
	 (v1 (vec 2 3 4))
	 (v2 (vec 3 4 5))
	 (n0 (vec 0.9 1.0))
	 (n1 (vec 0.3 0.0))
	 (n2 (vec 0.9 0.05))
	 (vertices (list v0 v1 v2))
	 (normals (list n0 n1 n2))
	 (indices '("0//0" "1//1" "2//2"))
	 (result (parse-face-vn indices vertices normals)))
    (is (equalp (list (make-vertex
		       :position (vcopy v0)
		       :tex-coords (vec 0 0)
		       :normal (vcopy n0))
		      (make-vertex
		       :position (vcopy v1)
		       :tex-coords (vec 0 0)
		       :normal (vcopy n1))
		      (make-vertex
		       :position (vcopy v2)
		       :tex-coords (vec 0 0)
		       :normal (vcopy n2)))
		result))))
