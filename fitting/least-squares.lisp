(in-package :cl-phys.fitting)

(defun least-squares (expr var data initial-guesses-plist
                      &key
                        (fitting-function (expression->function expr))
                        (epsilon 1d-7)
                        (jacobian^T nil)
                        (jacobian^T-jacobian nil))
  (let* ((xdata (mapcar #'car data))
         (ydata-true (mapcar #'cadr data))
         (ydata-fitted (mapcar (lambda (datum)
                                 (apply fitting-function
                                        (append
                                         (list (intern (symbol-name var)
                                                       "KEYWORD")
                                               datum)
                                         initial-guesses-plist)))
                               xdata))
         (residuals (mapcar #'- ydata-true ydata-fitted))
         (jacobian (unless (and jacobian^T jacobian^T-jacobian)
                     (jacobian expr var xdata initial-guesses-plist)))
         (jacobian^T (or jacobian^T
                         (matrix-transpose jacobian)))
         (jacobian^T-jacobian (or jacobian^T-jacobian
                                  (matrix-multiply jacobian^T jacobian)))
         (jacobian^T-residuals (matrix-multiply jacobian^T residuals))
         (changes-to-guesses (solve-system jacobian^T-jacobian
                                           jacobian^T-residuals))
         (new-guesses-plist (loop for change across changes-to-guesses
                               for key in initial-guesses-plist by #'cddr
                               for guess in (cdr initial-guesses-plist) by #'cddr
                               appending (list key (+ change guess)))))
    (if (loop for change across changes-to-guesses always (< (abs change)
                                                             epsilon))
        (values new-guesses-plist
                (lambda (x)
                  (apply fitting-function (cons (intern (symbol-name var)
                                                        "KEYWORD")
                                                (cons x new-guesses-plist))))
                nil)
        (least-squares expr var data new-guesses-plist
                       :fitting-function fitting-function
                       :epsilon epsilon
                       :jacobian^T jacobian^T
                       :jacobian^T-jacobian jacobian^T-jacobian))))
