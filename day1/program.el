;; -*- lexical-binding: t -*-

(defun n-strictly-increasing-steps (filename)
  "AoC: Day 1, Part 1"
  (let ((list (__file-to-number-list filename)))
    (__n-strictly-increasing-steps list)))

(defun n-strictly-increasing-window-steps (filename)
  "AoC: Day 1, Part 2"
  (let*
      ((list (__file-to-number-list filename))
       (window-sums
        (seq-mapn '+ list (cdr list) (cdr (cdr list)))))
    (__n-strictly-increasing-steps window-sums)))

(defun __n-strictly-increasing-steps (seq)
  "Count the number of strictly increasing steps between contiguous elements of a list, front to back"
  (let*
      ((steps (seq-mapn '- (cdr seq) seq))
       (inc-steps (seq-filter (lambda (elem) (> elem 0)) steps)))
    (seq-length inc-steps)))

(defun __file-to-number-list (filename)
  "Map the lines of a file into a list of numbers"
  (let*
      ((str
        (with-temp-buffer
         (insert-file-contents filename)
         (buffer-string)))
       (str-list
        (split-string str))
       )
    (seq-map 'string-to-number str-list)))

;; Tests

(ert-deftest day1-part1 ()
  (should (equal (n-strictly-increasing-steps "./test-input") 7))
  (should (equal (n-strictly-increasing-steps "./input") 1215)))

(ert-deftest day1-part2 ()
  (should (equal (n-strictly-increasing-window-steps "./test-input") 5))
  (should (equal (n-strictly-increasing-window-steps "./input") 1150)))
