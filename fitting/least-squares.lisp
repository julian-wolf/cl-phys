(in-package :cl-phys.fitting)

(defun least-squares (expr var data
                      &optional initial-guesses-plist
                      &key (epsilon 1d-6) (maximum-iterations 30))
  (if (is-linear-in-parameters expr var)
      (linear-least-squares expr var data)
      (nonlinear-least-squares expr var data initial-guesses-plist
                               epsilon maximum-iterations)))

(defun linear-least-squares (expr var data)
  (multiple-value-bind
        (xdata ydata-true eydata)
      (all-data data)
    (let* ((interned-var (intern (symbol-name var)
				 "KEYWORD"))
	   (fitting-function (expression->function expr))
	   (basis-functions (expression->basis-functions expr)) ;; TODO: defun
           (y-variances (if eydata
                            (mapcar (lambda (datum)
                                      (expt datum 2))
                                    eydata)
                            (mapcar (lambda (_)
                                      1)
                                    ydata-true)))
           (weight-matrix (diagonal-matrix (mapcar (lambda (datum)
                                                     (/ 1 datum))
                                                   y-variances)))
	   (n-data (length xdata))
	   (n-functions (length basis-functions))
           (basis-matrix (make-array (list n-data n-functions)
				     :initial-element nil)))
      (dotimes (i n-data)
	(dotimes (j n-functions)
	  (setf (aref basis-matrix i j)
		(fapply (nth basis-functions j)
			(nth xdata i)))))
      (let* ((basis-matrix^T (matrix-multiply (matrix-transpose basis-matrix)
					      weight-matrix))
	     (basis-matrix^T-basis-matrix (matrix-multiply
					   basis-matrix^T basis-matrix))
	     (results (solve-system basis-matrix^T-basis-matrix
				    (matrix-multiply
				     basis-matrix^T ydata-true)))
	     (results-plist nil) ;; TODO: fill in
	     (output-fitting-function
	      (lambda (datum)
		(apply fitting-function
		       (cons interned-var
			     (cons datum results-plist)))))
	     (ydata-fitted (apply output-fitting-function xdata))
	     (residuals (mapcar #'- ydata-true ydata-fitted))
	     (chi^2 nil) ;; TODO
	     (ndof nil)
	     (covariance-matrix nil)) ;; TODO
	(values results-plist
		output-fitting-function
		(/ chi^2 ndof)
		covariance-matrix
		basis-matrix^T-basis-matrix
		(matrix-inverse basis-matrix^T-basis-matrix)
		residuals)))))
      

(defun nonlinear-least-squares (expr var data initial-guesses-plist
                                &key (epsilon 1d-6) (maximum-iterations 30))
  (multiple-value-bind
        (xdata ydata-true eydata)
      (all-data data)
    (let* ((interned-var (intern (symbol-name var)
                                 "KEYWORD"))
           (fitting-function (expression->function expr))
           (y-variances (if eydata
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
                   (if (or (every (lambda (change)
                                    (> epsilon (abs change)))
                                  changes-to-guesses)
                           (>= number-of-iterations maximum-iterations))
                       (let* ((output-fitting-function
                               (lambda (datum)
                                 (apply fitting-function
                                        (cons interned-var
                                              (cons datum new-guesses-plist)))))
                              (squared-residuals (mapcar (lambda (residual)
                                                           (expt residual 2))
                                                         residuals))
                              (R^2 (apply #'+ squared-residuals))
                              (chi^2 (apply #'+ (mapcar #'/
                                                        squared-residuals
                                                        y-variances)))
                              (ndata (length y-variances))
                              (ndof (- ndata (/ (length guesses-plist)
                                                2)))
                              (covariance-matrix
                               (matrix-multiply (/ R^2 ndata)
                                                (matrix-inverse
                                                 jacobian^T-jacobian))))
                         (values new-guesses-plist
                                 output-fitting-function
                                 (/ chi^2 ndof)
                                 covariance-matrix
                                 jacobian^T-jacobian
                                 (matrix-inverse jacobian^T-jacobian)
                                 residuals
                                 number-of-iterations))
                       (least-squares-fit fitting-function new-guesses-plist)))))
        (least-squares-fit fitting-function initial-guesses-plist)))))
