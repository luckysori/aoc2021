;; -*- lexical-binding: t -*-

;; Part 1

(defun power-consumption (filename)
  "AoC: Day 3, Part 1"
  (let
      ((bin-nums (__file-to-binary-nums filename)))
    (* (gamma bin-nums) (epsilon bin-nums))))

(defun gamma (ns)
  (binary-list-to-int (freq ns 'majority)))

(defun epsilon (ns)
  (binary-list-to-int (freq ns 'minority)))

(defun freq (ns fn)
  (let
      ((sum (-reduce (-partial '-zip-with '+) ns))
       (length (seq-length ns)))
    (-map (-partial fn length) sum)))

(defun minority (n total)
  (abs (- (majority n total) 1)))

(defun majority (total n)
  (round (/ n (float total))))

;; Part 2

(defun life-support-rating (filename)
  "AoC: Day 3, Part 2"
  (let*
      ((bin-nums (__file-to-binary-nums filename))
       (oxygen (oxygen-generator-rating-rec bin-nums 0))
       (co2 (co2-scrubber-rating-rec bin-nums 0)))
    (* (binary-list-to-int oxygen)
       (binary-list-to-int co2))))

(defun oxygen-generator-rating-rec (bins i)
  (if (equal (seq-length bins) 1)
      (car bins)
    (oxygen-generator-rating-rec (keep-most-popular bins i) (+ i 1))))

(defun keep-most-popular (bins i)
  (let ((eps (freq bins 'majority-up)))
    (-filter
     (lambda (n) (equal (nth i n) (nth i eps))) bins)))

(defun majority-up (total n)
  (__round-up (/ n (float total))))

(defun __round-up (n)
  (if (equal n 0.5)
      1
    (round n)))

(defun co2-scrubber-rating-rec (bins i)
  (if (equal (seq-length bins) 1)
      (car bins)
    (co2-scrubber-rating-rec (keep-least-popular bins i) (+ i 1))))

(defun keep-least-popular (bins i)
  (let ((eps (freq bins 'minority-down)))
    (-filter
     (lambda (n) (equal (nth i n) (nth i eps))) bins)))

(defun minority-down (total n)
  (__round-down (/ (- total n) (float total))))

(defun __round-down (n)
  (if (equal n 0.5)
      0
    (round n)))

(defun __file-to-binary-nums (filename)
  "Map the lines of a file into a list of binary numbers"
  (let*
      ((str
        (with-temp-buffer
         (insert-file-contents filename)
         (buffer-string)))
       (lines
        (split-string str "\n"))
       (bin-strs
        (seq-map 'split-string lines))
       (bin-strs2 (seq-map 'car bin-strs)))
    (seq-map (lambda (n) (seq-map 'char-to-num n)) bin-strs2)))

(defun char-to-num (c)
  (string-to-number (char-to-string c)))

(defun binary-list-to-int (xs)
  (string-to-number (concat (-map 'string-to-char (-map 'number-to-string xs))) 2))

;; Tests

(ert-deftest day3-part1 ()
  (let ((test-input-res (power-consumption "./test-input"))
        (input-res (power-consumption "./input")))
    (should (equal test-input-res 198))
    (should (equal input-res 1458194))))

(ert-deftest day3-part2 ()
  (let ((test-input-res (life-support-rating "./test-input"))
        (input-res (life-support-rating "./input")))
    (should (equal test-input-res 230))
    (should (equal input-res 2829354))))
