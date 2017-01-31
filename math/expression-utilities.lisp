(in-package :cl-phys.math)
 
(defun convert-expression (expr)
  "Intern all of the variables found in an expression
expr. Return a new expression, with all variables
interned, and a list of variables that have been interned."
  (cond
    ((null expr)
     (values nil nil))
    ((arithmetic-expression-p expr)
     (multiple-value-bind
           (converted-expression variable-list)
         (convert-expression (cdr expr))
       (values (cons (car expr)
                     converted-expression)
               variable-list)))
    ((listp expr)
     (let ((arg (car expr)))
       (multiple-value-bind
             (converted-expression variable-list)
           (convert-expression (cdr expr))
         (cond
           ((null arg)
            (values nil nil))
           ((number-p arg)
            (values (cons arg converted-expression)
                    variable-list))
           ((variable-p arg)
            (let ((interned-arg (intern (symbol-name arg))))
              (values (cons interned-arg converted-expression)
                      (if (find arg variable-list)
                          variable-list
                          (cons arg variable-list)))))
           ((arithmetic-expression-p arg)
            (multiple-value-bind
                  (converted-expression-arg variable-list-arg)
                (convert-expression arg)
              (values (cons converted-expression-arg
                            converted-expression)
                      (remove-duplicates (append variable-list-arg
                                                 variable-list)))))
           (t
            (error "Expression not of supported list type."))))))
    ((variable-p expr)
     (let ((interned-expr (intern (symbol-name expr))))
       (values interned-expr (cons expr nil))))
    ((number-p expr)
     (values expr nil))
    (t
     (error "Expression not of supported type. Uh oh!"))))

(defun expression->function (expr)
  "Turn an expression into a function."
  (multiple-value-bind
        (converted-expression variable-list)
      (convert-expression expr)
    (coerce `(lambda ,(append (cons '&key nil)
                              (mapcar (lambda (var) 
                                        (list (intern (symbol-name var))
                                              0))
                                      variable-list)
                              (cons '&allow-other-keys nil))
               ,converted-expression)
            'function)))

(defun depends-on (var expr)
  (let ((val (car expr)))
    (cond
      ((null val)
       nil)
      ((listp val)
       (or (depends-on var val)
           (depends-on var (cdr expr))))
      (t
       (or (eq val var)
           (depends-on var (cdr expr)))))))

(defun is-linear-in-parameters (expr var)
  (let ((parameters (remove var (nth-value 1 (convert-expression expr)))))
    (notany (lambda (parameter)
              (let* ((derivative (differentiate-analytic expr parameter)))
                (cond
                  ((numberp derivative)
                   nil)
                  ((arithmetic-expression-p derivative)
                   (some (lambda (other-parameter)
                           (depends-on other-parameter derivative))
                         (remove parameter parameters)))
                  ((variable-p derivative)
                   (some (lambda (other-parameter)
                           (eql other-parameter derivative))
                         (remove parameter parameters)))
                  (t
                   (error (format nil "Weird derivative ~a" derivative))))))
            parameters)))
