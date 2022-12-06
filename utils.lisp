(in-package #:aoc2022)

(defmacro with-row-in-file ((row-var file) &body body)
  (let ((stream-var (gensym)))
    `(with-open-file (,stream-var ,file)
       (loop :for ,row-var = (read-line ,stream-var NIL NIL)
	     :while ,row-var
	  :do (progn
		,@body)))))

(defun find-doublette (string1 string2)
  (loop :for char :across string1
	:when (find char string2)
	  :collect char))

(defun split-string-on (delimiter string)
  (let ((pos (position delimiter string)))
    (if pos
	(list (subseq string 0 pos) (subseq string (1+ pos)))
	string)))

(defun between (num range-min range-max)
  (if (and (>= num range-min) (<= num range-max))
      t
      NIL))

(defun skip-rows (num stream)
  (loop :repeat num
	:do (read-line stream)))

(defun read-next-number (stream)
  (loop :for char = (peek-char NIL stream)
	:while char
	:do (if (between (char-code char) (char-code #\0) (char-code #\9))
		(return-from read-next-number (read stream))
		(read-char stream))))
