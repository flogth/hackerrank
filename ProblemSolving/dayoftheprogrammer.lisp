(defun solve ()
  (let* ((input (read-line))
         (year (parse-integer input))
         (day (day-256 year)))
    (format t "~d.09.~d" day year)))

(defun day-256 (year)
  (cond
    ((< year 1918)
     (if (divisible? year 4) 12 13))
    ((= year 1918) 26)
    (t
     (cond
       ((divisible? year 400) 12)
       ((and (divisible? year 4) (not (divisible? year 100)) ) 12)
       (t 13)))))

(defun divisible? (number divisor)
  (zerop (mod number divisor)))

(solve)
