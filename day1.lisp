;;;; -*- Mode: common-lisp; -*-

;; https://adventofcode.com/2021/day/1

;; part 1
(print
 (loop
   for depth in (uiop:read-file-lines "day1-data.txt")
   if (and
       (not (null history))
       (> (parse-integer depth) (car (last history))))
     count depth into total
   append (list (parse-integer depth)) into history
   finally
      (return total)))
