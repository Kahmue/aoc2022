(defpackage #:aoc2022
  (:use :cl))

(in-package #:aoc2022)

(defun day1_1 (file)
  (let ((result 0)
	(cur 0))
    (with-open-file (stream file)
      (loop :for row = (read-line stream NIL NIL)
	    :while row
	    :do (progn
		  (if (not (zerop (length row)))
		      (incf cur (parse-integer row))
		      (progn
			(when (> cur result)
			    (setf result cur))
			(setf cur 0)))))
      result)))

(defun day1_2 (file)
  (let ((result NIL)
	(cur 0))
    (with-open-file (stream file)
      (loop :for row = (read-line stream NIL NIL)
	    :while row
	    :do (progn
		  (if (not (zerop (length row)))
		      (incf cur (parse-integer row))
		      (progn
			(push cur result)
			(setf cur 0))))))
    (setf result (sort result #'>))
    (+ (first result) (second result) (third result))))
