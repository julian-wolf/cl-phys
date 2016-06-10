(in-package :cl-phys.math)

(defgeneric matrix-transpose (original-matrix)
  (:documentation "Transpose a matrix."))

(defmethod matrix-transpose ((original-matrix array))
  (let* ((m (array-dimension original-matrix 0))
         (n (array-dimension original-matrix 1))
         (transposed-matrix (make-array (list n m)
                                        :initial-element nil)))
    (loop for i from 0 below m
       do (loop for j from 0 below n
             do (setf (aref transposed-matrix j i)
                      (aref original-matrix i j))))
    transposed-matrix))

(defmethod matrix-transpose ((original-matrix cons))
  (apply #'mapcar #'list original-matrix))

(defgeneric matrix-multiply (matrix &rest other-matrices)
  (:documentation "Multiply a series of matrices together.
Cons cells are promoted to simple-arrays when arguments are
given of mixed types."))

(defmethod matrix-multiply ((matrix array) &rest other-matrices)
  (case (length other-matrices)
    (0 matrix)
    (1 (let* ((other-matrix (car other-matrices))
              (other-matrix (if (subtypep (type-of other-matrix)
                                          'array)
                                other-matrix
                                (make-array (list (length other-matrix)
                                                  (if (listp (car other-matrix))
                                                      (length (car other-matrix))
                                                      1))
                                            :initial-contents
                                            (if (listp (car other-matrix))
                                                other-matrix
                                                (mapcar #'list
                                                        other-matrix)))))
              (m (array-dimension matrix 0))
              (n (array-dimension matrix 1))
              (l (array-dimension other-matrix 1))
              (matrix-product (make-array (list m l)
                                          :initial-element nil)))
         (if (= n (array-dimension other-matrix 0))
             (progn
               (loop for i from 0 below m
                  do (loop for k from 0 below l
                        do (setf (aref matrix-product i k)
                                 (loop for j from 0 below n
                                    summing (* (aref matrix i j)
                                               (aref other-matrix j k))))))
               matrix-product)
             (error "Matrix dimensions must match."))))
    (otherwise (matrix-multiply matrix
                                (apply #'matrix-multiply other-matrices)))))

(defmethod matrix-multiply ((matrix cons) &rest other-matrices)
  (case (length other-matrices)
    (0 matrix)
    (1 (let ((other-matrix (car other-matrices)))
         (cond
           ((subtypep (type-of other-matrix)
                      'array)
            (matrix-multiply (make-array (list (length matrix)
                                               (length (car matrix)))
                                         :initial-contents matrix)
                             other-matrix))
           ((= (length (car matrix))
               (length other-matrix))
            (mapcar (lambda (row)
                      (apply #'mapcar (lambda (&rest column)
                                        (apply #'+ (mapcar #'* row column)))
                             other-matrix))
                    matrix))
           (t
            (error "Matrix dimensions must match.")))))
    (otherwise (matrix-multiply matrix
                                (apply #'matrix-multiply other-matrices)))))

(defgeneric diagonal-matrix (diagonals)
  (:documentation "Create a diagonal square matrix
with elements given by diagonals."))

(defmethod diagonal-matrix ((diagonals cons))
  (let* ((n (length diagonals))
         (matrix (make-array (list n n)
                             :initial-element 0)))
    (do ((k 0 (1+ k)))
        ((= k n))
      (setf (aref matrix k k)
            (nth k diagonals)))
    matrix))

(defun identity-matrix (n)
  "Create an n-by-n identity matrix."
  (let ((matrix (make-array (list n n)
                            :initial-element 0)))
    (do ((k 0 (1+ k)))
        ((= k n))
      (setf (aref matrix k k)
            1))
    matrix))

(defgeneric solve-system (matrix input-vector)
  (:documentation "Solve a system A x = b for x, given
the matrix A and the input-vector b. Methods should
use Gaussian elimination with partial pivoting."))

(defun %n-swap-rows (matrix a b &optional
                                  (min-index 0)
                                  (max-index (- (array-dimension matrix 1) 1)))
  "Swap rows a and b of matrix between columns min-index and max-index."
  (do ((k min-index (1+ k)))
      ((> k max-index))
    (let ((old-matrix-a-k-val (aref matrix a k)))
      (setf (aref matrix a k)
            (aref matrix b k))
      (setf (aref matrix b k)
            old-matrix-a-k-val)))
  matrix)

(defmethod solve-system ((matrix array) input-vector)
  (if (apply #'= (array-dimensions matrix))
      (let* ((n (array-dimension matrix 1))
             (solution-vector (make-array n :initial-element nil)))
        (do ((k 0 (1+ k)))
            ((= k (1- n)))
          (let* ((max-index (loop
                               with max-index = k
                               with max-val = (abs (aref matrix k k))
                               for l from (1+ k) below n
                               when (> (abs (aref matrix l k)) max-val)
                               do (setf max-val (abs (aref matrix l k))
                                        max-index l)
                               end
                               finally (return max-index)))
                 (max-val (aref matrix max-index k)))
            (if (zerop max-val)
                (error "Matrix is singular!")
                (progn
                  (%n-swap-rows matrix k max-index k (1- n))
                  (let ((old-input-vector-k-val (aref input-vector k 0)))
                    (setf (aref input-vector k 0)
                          (aref input-vector max-index 0))
                    (setf (aref input-vector max-index 0)
                          old-input-vector-k-val))))
            (do ((i (1+ k) (1+ i)))
                ((= i n))
              (let ((multiplier (/ (aref matrix i k)
                                   (aref matrix k k))))
                (unless (zerop multiplier)
                  (do ((j (1+ k) (1+ j)))
                      ((= j n))
                    (setf (aref matrix i j)
                          (- (aref matrix i j)
                             (* multiplier (aref matrix k j))))))
                (do ((j 0 (1+ j)))
                    ((>= j k))
                  (setf (aref matrix i j) 0))
                (setf (aref input-vector i 0)
                      (- (aref input-vector i 0)
                         (* multiplier (aref input-vector k 0))))))))
        (setf (aref solution-vector (1- n))
              (/ (aref input-vector (1- n) 0)
                 (aref matrix (1- n) (1- n))))
        (do ((k (- n 2) (1- k)))
            ((minusp k))
          (setf (aref solution-vector k)
                (/ (- (aref input-vector k 0)
                      (loop for j from (1+ k) below n
                         summing (* (aref matrix k j)
                                    (aref solution-vector j))))
                   (aref matrix k k))))
        solution-vector)
      (error "Input matrix must be square.")))
