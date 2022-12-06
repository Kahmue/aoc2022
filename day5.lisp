(in-package :aoc2022)

(defparameter *stacks* (make-array 10))

(defun init-stacks () ;; init stacks
  (setf (aref *stacks* 1) '(J Z G V T D B N))
  (setf (aref *stacks* 2) '(F P W D M R S))
  (setf (aref *stacks* 3) '(Z S R C V))
  (setf (aref *stacks* 4) '(G H P Z J T R))
  (setf (aref *stacks* 5) '(F Q Z D N J C T))
  (setf (aref *stacks* 6) '(M F S G W P V N))
  (setf (aref *stacks* 7) '(Q P B V C G))
  (setf (aref *stacks* 8) '(N P B Z))
  (setf (aref *stacks* 9) '(J P W)))

(defun move-crates (from to amount)
  (loop :repeat amount :do (push (pop (aref *stacks* from)) (aref *stacks* to))))

(defun enhanced-move-crates (from to amount)
  (let (temp)
    (loop :repeat amount :do (push (pop (aref *stacks* from)) temp))
    (loop :repeat amount :do (push (pop temp) (aref *stacks* to)))))

(defun parse-line (line)
  (let (amount from to)
    (with-input-from-string (stream line)
      (setf amount (read-next-number stream)
	    from (read-next-number stream)
	    to (read-next-number stream)))
    (values amount from to)))

(defun print-stack-top (stack)
  (format t "~a" (car stack)))

(defun day5_1 (filename)
  (init-stacks)
  (with-open-file (stream filename)
    (skip-rows 10 stream)
    (loop :for line = (read-line stream NIL NIL)
	  :while line
	  :do (multiple-value-bind (amount from to) (parse-line line)
		(move-crates from to amount)))
    (loop :for stack :from 1 :below 10
	  :do (print-stack-top (aref *stacks* stack)))))

(defun day5_2 (filename)
  (init-stacks)
  (with-open-file (stream filename)
    (skip-rows 10 stream)
    (loop :for line = (read-line stream NIL NIL)
	  :while line
	  :do (multiple-value-bind (amount from to) (parse-line line)
		(enhanced-move-crates from to amount)))
    (loop :for stack :from 1 :below 10
	  :do (print-stack-top (aref *stacks* stack)))))
