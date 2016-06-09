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
                                                  (length (car other-matrix)))
                                            :initial-contents other-matrix)))
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
