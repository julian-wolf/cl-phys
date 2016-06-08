(in-package :cl-phys.calculus)

(defun differentiate-analytic (expr var)
  "Try to analytically differentiate an
expression expr with respect to variable var."
  (cond
    ((number-p expr)
     0)
    ((variable-p expr)
     (if (same-variable-p expr var)
         1
         0))
    ((sum-p expr)
     (make-sum (mapcar (lambda (summand)
                         (differentiate-analytic summand var))
                       (summands expr))))
     ((product-p expr)
     (let ((multiplicands (multiplicands expr)))
       (make-sum (loop for multiplicand in multiplicands
                    for i from 0
                    collecting
                      (make-product
                       (cons (differentiate-analytic multiplicand  var)
                             (loop for multiplier in multiplicands
                                for j from 0
                                unless (= i j)
                                collect multiplier)))))))
    ((sin-p expr)
     (make-product (differentiate-analytic (argument expr) var)
                   (make-cos (argument expr))))
    ((cos-p expr)
     (make-product -1
                   (differentiate-analytic (argument expr) var)
                   (make-sin (argument expr))))
    ((tan-p expr)
     (make-quotient (differentiate-analytic (argument expr) var)
                    (make-expt (make-cos (argument expr)) 2)))
    ((asin-p expr)
     (make-product (differentiate-analytic (argument expr) var)
                   (make-expt (make-difference 1 (make-expt expr 2))
                              -1/2)))
    ((acos-p expr)
     (make-product -1
                   (differentiate-analytic (argument expr) var)
                   (make-expt (make-difference 1 (make-expt expr 2))
                              -1/2)))
    ((atan-p expr)
     (make-quotient (differentiate-analytic (argument expr) var)
                    (make-sum 1 (make-expt expr 2))))
    ((exp-p expr)
     (make-product (differentiate-analytic (argument expr) var)
                   expr))
    ((log-p expr)
     (make-quotient (differentiate-analytic (argument expr) var)
                    (argument expr)))
    ((expt-p expr)
     (let* ((base (base expr))
            (power (power expr))
            (power-derivative (differentiate-analytic power var))
            (prefactor (make-expt base (make-difference (list power 1))))
            (first-term (make-product power
                                      (differentiate-analytic base var)))
            (second-term (if (eq power-derivative 0)
                             0
                             (make-product base (make-log base)))))
       (make-product prefactor (make-sum first-term second-term))))
    (t (error 'bad-math-expression-error
              :message "Expression not differentiable."
              :expression expr))))

(defun eval-derivative-analytic (expr var values-plist)
  "Analytically evaluate the derivative of an expression expr
with respect to the variable var at the point described by
values-plist."
  (apply (expression->function
          (differentiate-analytic expr var))
         values-plist))

(defun eval-derivative-numeric (expr var values-plist
                                &optional (epsilon 1.0d-7))
  "Numerically evaluate the derivative of an expression expr
with respect to the variable var at the point described by
values-plist."
  (let ((expr-function (expression->function expr))
        (var-symbol (find-symbol (string var))))
    (labels ((increment-var (values-plist)
               ;; Increment the variable var by epsilon.
               (if (null values-plist)
                   nil
                   (let ((key (car values-plist))
                         (val (cadr values-plist)))
                     (cons key
                           (cons (if (eq (find-symbol (string key))
                                         var-symbol)
                                     (+ val epsilon)
                                     val)
                                 (increment-var
                                  (cddr values-plist))))))))
      (/ (- (apply expr-function (increment-var values-plist))
            (apply expr-function values-plist))
         epsilon))))

(defun %to-var-list (var-plist)
  "Turn a var-plist of the form (:a 2 :b 5 :c 17) into
a list of variables of the form (list 'a 'b 'c)."
  (let ((var-key (car var-plist)))
    (if (null var-key)
        nil
        (cons (intern (symbol-name var-key))
              (%to-var-list (cddr var-plist))))))

(defun %minimal-sorted-parameter-list (generated-var-list supplied-var-list)
  "Check that all elements of generated-var-list are present in
supplied-var-list and remove any extra elements from the latter."
  (if (loop for generated-var in generated-var-list
         always (find generated-var supplied-var-list :test #'equal))
      (loop for supplied-var in supplied-var-list
         when (find supplied-var generated-var-list :test #'equal)
         collect it)
      (error "supplied-var-list does not contain all necessary variables.")))

(defun jacobian (expr var var-values parameter-values-plist)
  "Construct the jacobian of expr for values of var given
by var-values with parameters (all arguments to expr except
var) given by parameter-values-plist."
  (multiple-value-bind
        (converted-expression variable-list)
      (convert-expression expr)
    (let* ((parameter-list (%minimal-sorted-parameter-list
                            (remove var variable-list)
                            (%to-var-list parameter-values-plist)))
           (m (length var-values))
           (n (length parameter-list))
           (jacobian-matrix (make-array (list m n)
                             :initial-element nil)))
      (loop for j from 0 below n
         do (let ((derivative-function
                    (expression->function
                     (differentiate-analytic expr (nth j parameter-list)))))
              (loop for i from 0 below m
                 do (setf (aref jacobian-matrix i j)
                          (apply derivative-function
                                 (append (list (intern (symbol-name var)
                                                       "KEYWORD")
                                               (nth i var-values))
                                         parameter-values-plist))))))
      jacobian-matrix)))
                                                   
                                                   
                                 
