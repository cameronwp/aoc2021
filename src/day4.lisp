(uiop:define-package #:aoc2021/day4
  (:use #:cl
        #:cl-ppcre))

(in-package #:aoc2021/day4)

(defparameter *test-input* '(
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
  ""
  "22 13 17 11  0"
  " 8  2 23  4 24"
  "21  9 14 16  7"
  " 6 10  3 18  5"
  " 1 12 20 15 19"
  ""
  " 3 15  0  2 22"
  " 9 18 13 17  5"
  "19  8  7 25 23"
  "20 11 10 24  4"
  "14 21 16 12  6"
  ""
  "14 21 17 24  4"
  "10 16 15  9 19"
  "18  8 23 26 20"
  "22 11 13  6  5"
  " 2  0 12  3  7"))

(defun read-puzzle-input-lines (filename)
  (uiop:with-safe-io-syntax ()
    (uiop:read-file-lines (asdf:system-relative-pathname "aoc2021" (format nil "puzzle-inputs/~a" filename)))))

;; part 1

;; https://stackoverflow.com/a/3513158/11494693
(defun rotate (list-of-lists)
  "Transpose a list of lists"
  (apply #'mapcar #'list list-of-lists))

(defun pull-lists (row)
  "Get sublists of 5 cells"
  (loop for n upto (- (list-length row) 5) by 5
        append (list (subseq row n (+ n 5)))))

(defun make-columns (rows)
  "Make lists of columns"
  (loop for n upto (- (list-length rows) 5) by 5
        for bingo-card = (subseq rows n (+ n 5))
        for transposed = (rotate bingo-card)
        append transposed))

(defun find-winner-index (rows active-picks)
  "Get the index of the row that won. returns nil if no winners"
  (loop for r upto (- (list-length rows) 1)
        if (null (set-difference (nth r rows) active-picks
                                 :test #'string=))
          return r))

(defun get-index-range (i &key (range 5))
  "Get the lower and upper bounds for an index in a set range"
  (let* ((lower (- i (mod i range)))
         (upper (+ lower range)))
    (values lower upper)))

(defun filter-list-of-lists (seq subseq)
  "Remove all elements from subseq from seq. seq should be a list of lists"
  ;; flatten the list of bingo cells
  (let ((all (loop for s in seq append s)))
    ;; filter the set
    (set-difference all subseq :test #'string=)))

(defun sum-unmarked-cells (collection index picks)
  "Get the sum of the unmarked cells for a board"
  (uiop:nest
   (multiple-value-bind (lower upper)
       (get-index-range index))
   (let ((bingo-card (subseq collection lower upper))))
   (let ((unmarked (filter-list-of-lists bingo-card picks)))
   (let ((cell-sum (loop for cell in unmarked sum (parse-integer cell))))
     cell-sum)))

(defun day4part1 ()
  (uiop:nest
   (let* ((input (read-puzzle-input-lines "day4.txt"))
          (picks (uiop:split-string (car input) :separator ","))
          ;; pregenerate all possible winning rows & columns
          (rows (loop for l from 1 upto (list-length input)
                      ;; input strings may have double spaces or start with a space, eg. " 1 2 3  4 5"
                      for line = (cl-ppcre:regex-replace-all "\\W+" (string-trim '(#\Space) (nth l input)) " ")
                      unless (or (null line) (string= line "") (string= line "NIL"))
                        append (list (uiop:split-string line :separator " "))))
          (columns (make-columns rows))))
    (multiple-value-bind (winning-draw sum-rest)
      (loop for p from 5 upto (list-length picks)
            for active-picks = (subseq picks 0 p)
            for r = (find-winner-index rows active-picks)
            for c = (find-winner-index columns active-picks)
            if r
              return (values (parse-integer (car (last active-picks)))
                             (sum-unmarked-cells rows r active-picks))
            if c
              return (values (parse-integer (car (last active-picks)))
                             (sum-unmarked-cells columns c active-picks))))
    (* winning-draw sum-rest)))

(print (day4part1))
