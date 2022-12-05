(defpackage #:aoc2022
  (:use :cl))

(in-package #:aoc2022)

(defun containsp (e1 e2)
  (destructuring-bind (e1-s e1-e) e1
    (destructuring-bind (e2-s e2-e) e2
      (if (and (<= e1-s e2-s) (>= e1-e e2-e))
	  e1
	  (if (and (<= e2-s e1-s) (>= e2-e e1-e))
	      e2
	      NIL)))))

(defun overlap (e1 e2)
  (destructuring-bind (e1-s e1-e) e1
    (destructuring-bind (e2-s e2-e) e2
      (cond
	((containsp e1 e2) t)
	((or (between e2-s e1-s e1-e) (between e2-e e1-s e1-e)) t)
	((or (between e1-s e2-s e2-e) (between e1-e e2-s e2-e)) t)
	(t NIL)))))


(defun day4_1 (file)
  (let ((result 0))
    (with-row-in-file (string file)
      (let* ((st (split-string-on #\, string))
	     (e1 (mapcar #'parse-integer (split-string-on #\- (car st))))
	     (e2 (mapcar #'parse-integer (split-string-on #\- (cadr st)))))
	(if (containsp e1 e2)
	    (incf result))))
    result))

(defun day4_2 (file)
  (let ((result 0))
    (with-row-in-file (string file)
      (let* ((st (split-string-on #\, string))
	     (e1 (mapcar #'parse-integer (split-string-on #\- (car st))))
	     (e2 (mapcar #'parse-integer (split-string-on #\- (cadr st)))))
	(if (overlap e1 e2)
	    (incf result))))
    result))




