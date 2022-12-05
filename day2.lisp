(defpackage #:aoc2022
  (:use :cl))

(in-package #:aoc2022)

(defparameter X 0)
(defparameter Y 1)
(defparameter Z 2)

(defparameter A 0)
(defparameter B 1)
(defparameter C 2)



#|

A > C
B > A
C > B

0 0 - 3
0 1 - 6
0 2 - 0
1 0 - 0
1 1 - 3
1 2 - 6
2 0 - 6
2 1 - 0
2 2 - 3

(mod (a + 1) 3)
(mod (a + 2) 3)
|#

(defun score (val)
  (destructuring-bind (he me) val
    (cond
      ((= me (mod (+ he 2) 3)) (+ 0 (1+ me))) ;; lost
      ((= me (mod (+ he 1) 3)) (+ 6 (1+ me))) ;; won

      ((= me he) (+ 3 (1+ me)))))) ;; draw

(defun day2_1 (file)
  (with-open-file (stream file)
    (reduce #'+ (map 'list #'score 
		     (loop :for p1 = (read stream NIL NIL)
			   :for me = (read stream NIL NIL)
			   :while (and p1 me)
			   :collect (list (symbol-value p1) (symbol-value me)))))))

(defun pick-shape (shape outcome)
  (+ (* 3 outcome) (1+ (case outcome
			 (0 (mod (+ shape 2) 3))
			 (1 (mod (+ shape 0) 3))
			 (2 (mod (+ shape 1) 3))))))

(defun day2_2 (file)
  (with-open-file (stream file)
    (reduce #'+
	    (mapcar (lambda (x) (pick-shape (car x) (cadr x)))
		    (loop :for p1 = (read stream NIL NIL)
			  :for me = (read stream NIL NIL)
			  :while (and p1 me)
			  :collect (list (symbol-value p1) (symbol-value me)))))))


