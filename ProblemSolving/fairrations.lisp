(defun solve ()
  (let* ((input (read-lines))
         (lines (mapcar #'read-ints input))
         (l2 (mapcar (lambda (x) (mod x 2)) (second lines)))
         (people (make-array (list (length l2))
                             :element-type 'bit
                             :initial-contents l2)))
    (format t "~a" (minimum-loaves people))))

(defun minimum-loaves (people)
  (let ((n (hamming-weight people)))
    (if (oddp n)
        "NO"
      (loop for x = (copy-seq people) then (flip-bits x (msb x))
            while (plusp (hamming-weight x) )
            counting x into i
            finally (return (* 2 i))))))

(defun msb (bits)
  (loop for i across bits
        while (zerop i)
        count i))

(defun flip-bits (bits i)
  (setf (sbit bits i) (logxor (sbit bits i) 1))
  (setf (sbit bits (1+ i)) (logxor (sbit bits (1+ i)) 1))
  bits)

(defun hamming-weight (bits)
  (reduce #'+ bits))

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
