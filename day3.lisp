(defpackage #:aoc2022
  (:use :cl))

(in-package #:aoc2022)

(defun priority (item)
  (if (upper-case-p item)
      (+ 27 (- (char-code item) (char-code #\A)))
      (1+ (- (char-code item) (char-code #\a)))))

(defun find-doublette (string1 string2)
  (loop :for char :across string1
	:when (find char string2)
	  :collect char))

(defun find-badges (p1 p2 p3)
  (find-doublette p3 (map 'string #'identity (find-doublette p1 p2))))

(defun day3_1 (file)
  (let ((result 0))
    (with-open-file (stream file)
      (loop :for string = (read-line stream NIL NIL)
	    :while string
	    :do (progn
		  (incf result (priority (car (find-doublette (subseq string 0 (floor (/ (length string) 2))) (subseq string (floor (/ (length string) 2))))))))))
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
    result)
  )


