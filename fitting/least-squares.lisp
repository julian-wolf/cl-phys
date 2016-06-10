(in-package :cl-phys.fitting)

(defun least-squares (expr var data initial-guesses-plist
                      &key (epsilon 1d-6) (maximum-iterations 30))
  (multiple-value-bind
        (xdata ydata-true eydata)
      (all-data data)
    (let* ((interned-var (intern (symbol-name var)
                                 "KEYWORD"))
           (fitting-function (expression->function expr))
           (y-variances (if ydata-true
                            (mapcar (lambda (datum)
                                      (expt datum 2))
                                    eydata)
                            (mapcar (lambda (_)
                                      1)
                                    ydata-true)))
           (weight-matrix (diagonal-matrix (mapcar (lambda (datum)
                                                     (/ 1 datum))
                                                   y-variances)))
           (number-of-iterations 0))
      (labels ((least-squares-fit (fitting-function guesses-plist)
                 (incf number-of-iterations)
                 (let* ((ydata-fitted (mapcar (lambda (datum)
                                                (apply fitting-function
                                                       (cons interned-var
                                                             (cons datum
                                                                   guesses-plist))))
                                              xdata))
                        (residuals (mapcar #'- ydata-true ydata-fitted))
                        (jacobian (jacobian expr var xdata guesses-plist))
                        (jacobian^T (matrix-multiply (matrix-transpose jacobian)
                                                     weight-matrix))
                        (jacobian^T-jacobian (matrix-multiply jacobian^T
                                                              jacobian))
                        (jacobian^T-residuals (matrix-multiply jacobian^T residuals))
                        (changes-to-guesses (solve-system jacobian^T-jacobian
                                                          jacobian^T-residuals))
                        (new-guesses-plist
                         (loop for change across changes-to-guesses
                            for key in guesses-plist by #'cddr
                            for guess in (cdr guesses-plist) by #'cddr
                            appending (list key (+ change guess)))))
                   (if (or (loop for change across changes-to-guesses
                              always (> epsilon (abs change)))
                           (>= number-of-iterations maximum-iterations))
                       (values new-guesses-plist
                               (lambda (datum)
                                 (apply fitting-function
                                        (cons interned-var
                                              (cons datum new-guesses-plist))))
                               (/ (apply #'+ (mapcar #'/
                                                     (mapcar (lambda (residual)
                                                               (expt residual 2))
                                                             residuals)
                                                     y-variances))
                                  (- (length y-variances)
                                     (/ (length guesses-plist)
                                        2)))
                               number-of-iterations)
                       (least-squares-fit fitting-function new-guesses-plist)))))
      (least-squares-fit fitting-function initial-guesses-plist)))))
