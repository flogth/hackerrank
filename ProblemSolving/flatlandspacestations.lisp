(defun solve ()
  (let* ((input (read-lines))
         (lines (mapcar #'read-ints input))
         (n (caar lines))
         (is (second lines))
         (stations (sort is #'<)))
    (format t "~d" (maximum-distance n stations) )))

(defun maximum-distance (n xs)
  (let ((start (car xs))
        (end (- n 1 (car (last xs))))
        (between
          (floor
           (loop for i in xs
                 for j in (cdr xs)
                 maximize (abs (- i j))) 2)))
    (max start end between)))

(defun read-lines ()
  (loop for s = (read-line *standard-input* nil nil)
        while s
        collect s))

(defun read-ints (line)
  (let ((w (words line)))
    (mapcar #'parse-integer w)))

(defun words (str)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space str :start i)
        collect (subseq str i j)
        while j))

(solve)
