(uiop:define-package #:aoc2021/day2
  (:use #:cl))

(in-package #:aoc2021/day2)

(defparameter *test-actions* (list
                        "forward 5"
                        "down 5"
                        "forward 8"
                        "up 3"
                        "down 8"
                        "forward 2"))

(defun read-puzzle-input-lines (filename)
  (uiop:with-safe-io-syntax ()
    (uiop:read-file-lines (asdf:system-relative-pathname "aoc2021" (format nil "puzzle-inputs/~a" filename)))))

;; part 1
(print
 (let ((puzzle-input (read-puzzle-input-lines "day2.txt"))
       (depth 0)
       (horizontal 0))
   (loop
     for instruction in puzzle-input
     for (direction distance) = (uiop:split-string instruction :separator " ")
     if (equal direction "up")
       do (setf depth (- depth (parse-integer distance)))
     if (equal direction "down")
       do (setf depth (+ depth (parse-integer distance)))
     if (equal direction "forward")
       do (setf horizontal (+ horizontal (parse-integer distance))))
   (* horizontal depth)))
