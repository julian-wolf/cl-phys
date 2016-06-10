(in-package :cl-phys.fitting)

(defun xdata (data)
  (mapcar #'car data))

(defun %ydata-all (data)
  (mapcar #'cadr data))

(defun ydata (data)
  (let ((ydata-all (%ydata-all data)))
    (cond
      ((every #'listp ydata-all)
       (mapcar #'car ydata-all))
      ((notany #'listp ydata-all)
       ydata-all)
      (t
       (error "The existence of errors must be homogenous in all data.")))))

(defun eydata (data)
  (mapcar #'cadadr data))

(defun all-data (data)
  (let ((ydata-all (%ydata-all data)))
    (cond
      ((every #'listp ydata-all)
       (loop for datum in data
          collecting (car datum) into xdata
          collecting (caadr datum) into ydata
          collecting (cadadr datum) into eydata
          finally (return (values xdata ydata eydata))))
      ((notany #'listp ydata-all)
       (loop for datum in data
          collecting (car datum) into xdata
          collecting (cadr datum) into ydata
          finally (return (values xdata ydata nil))))
      (t
       (error "The existence of errors must be homogenous in all data.")))))
