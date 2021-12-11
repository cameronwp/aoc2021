(uiop:define-package #:aoc2021/day3
  (:use #:cl
        #:bit-smasher))

(in-package #:aoc2021/day3)

(defun read-puzzle-input-lines (filename)
  (uiop:with-safe-io-syntax ()
    (uiop:read-file-lines (asdf:system-relative-pathname "aoc2021" (format nil "puzzle-inputs/~a" filename)))))

(defun read-puzzle-input-forms (filename)
  (uiop:with-safe-io-syntax ()
    (uiop:read-file-forms (asdf:system-relative-pathname "aoc2021" (format nil "puzzle-inputs/~a" filename)))))

;; operations on bit vectors
;; https://dept-info.labri.fr/~strandh/Teaching/MTP/Common/David-Lamkins/chapter18.html

(defparameter *test-report* '(00100
                              11110
                              10110
                              10111
                              10101
                              01111
                              00111
                              11100
                              10000
                              11001
                              00010
                              01010))

;; part 1
(print
 (let* ((report (read-puzzle-input-lines "day3.txt"))
        (summary '(0 0 0 0 0 0 0 0 0 0 0 0)))
   (loop for input in report
         for digits = (map 'list #'digit-char-p (coerce input 'list))
         do (setf summary (mapcar #'+ summary digits)))
   (*
    ;; gamma rate
    (bits->int (map 'bit-vector
                    #'(lambda (b) (if (> b 500) 1 0))
                    summary))
    ;; epsilon rate
    (bits->int (map 'bit-vector
                    #'(lambda (b) (if (< b 500) 1 0))
                    summary)))))
