(in-package #:aoc2022)

(defparameter *priorities* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun priority (item)
  (1+ (position item *priorities*)))

(defun find-badges (p1 p2 p3)
  (find-doublette p3 (map 'string #'identity (find-doublette p1 p2))))

(defun day3_1 (file)
  (let ((result 0))
    (with-row-in-file (string file)
      (incf result (priority (car (find-doublette (subseq string 0 (floor (/ (length string) 2))) (subseq string (floor (/ (length string) 2))))))))
    result))

(defun day3_2 (file)
  (let ((result 0))
    (with-open-file (stream file)
      (loop :for p1 = (read-line stream NIL NIL)
	    :for p2 = (read-line stream NIL NIL)
	    :for p3 = (read-line stream NIL NIL)
	    :while (and p1 p2 p3)
	    :do (progn
		  (incf result (priority (car (find-badges p1 p2 p3)))))))
    result))


