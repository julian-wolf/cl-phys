(in-package :cl-phys.math)

(defun matrix-transpose (original-matrix)
  (let* ((m (array-dimension original-matrix 0))
         (n (array-dimension original-matrix 1))
         (transposed-matrix (make-array (list n m)
                                        :initial-element nil)))
    (loop for i from 0 below m
       do (loop for j from 0 below n
             do (setf (aref transposed-matrix j i)
                      (aref original-matrix i j))))
    transposed-matrix))

(defun matrix-multiply (matrix &rest other-matrices)
  (case (length other-matrices)
    (0 matrix)
    (1 (let* ((other-matrix (car other-matrices))
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
