;; -*- lexical-binding: t -*-

;; Part 1

(defun run-commands (filename)
  "AoC: Day 2, Part 1"
  (let* ((commands (__file-to-number-list filename))
        (coords (seq-map 'to-coord commands)))
    (seq-reduce 'add coords (make-coord :x 0 :y 0))))

(defun to-coord (command)
  "Convert a command into a vector expressed as coordinates"
  (let* ((action (car command))
         (distance (cdr command)))
    (pcase action
      ("forward" (make-coord :x distance :y 0))
      ("up" (make-coord :x 0 :y distance))
      ("down" (make-coord :x 0 :y (- distance))))))

(cl-defstruct coord x y)

;; Part 2

(defun travel (filename)
  "AoC: Day 2, Part 1"
  (let* ((commands (__file-to-number-list filename))
         (moves (seq-map 'to-move commands)))
    (seq-reduce 'move moves (make-pos :x 0 :y 0 :aim 0))))

(defun to-move (command)
  "Convert a command into a vector expressed as coordinates"
  (let* ((action (car command))
         (n (cdr command)))
    (pcase action
      ("forward" (make-forward :distance n))
      ("up" (make-up :angle n))
      ("down" (make-down :angle n)))))

(cl-defstruct pos x y aim)
(cl-defstruct forward distance)
(cl-defstruct up angle)
(cl-defstruct down angle)

(cl-defgeneric add (a b))
(cl-defmethod add ((a coord) (b coord))
  (make-coord :x (+ (coord-x a) (coord-x b))
              :y (+ (coord-y a) (coord-y b))))

(cl-defgeneric move (p action))

(cl-defmethod move ((p pos) (f forward))
  (let ((dist (forward-distance f)))
    (make-pos :x (+ (pos-x p) dist)
              :y (+ (pos-y p) (* (pos-aim p) dist))
              :aim (pos-aim p))))

(cl-defmethod move ((p pos) (f up))
    (make-pos :x (pos-x p)
              :y (pos-y p)
              :aim (- (pos-aim p) (up-angle f))))

(cl-defmethod move ((p pos) (f down))
    (make-pos :x (pos-x p)
              :y (pos-y p)
              :aim (+ (pos-aim p) (down-angle f))))

;; Common

(defun __file-to-number-list (filename)
  "Map the lines of a file into a list of commands"
  (let*
      ((str
        (with-temp-buffer
         (insert-file-contents filename)
         (buffer-string)))
       (lines
        (split-string str "\n"))
       (commands
        (seq-map 'split-string lines)))
    (seq-map
     (lambda (c) (cons (pop c) (string-to-number (pop c))))
     commands)))

;; Tests

(ert-deftest day2-part1 ()
  (let ((coord-test-input (run-commands "./test-input"))
        (coord-input (run-commands "./input")))
    (should (equal (* (abs (coord-x coord-test-input))
                      (abs (coord-y coord-test-input))) 150))
    (should (equal (* (abs (coord-x coord-input))
                      (abs (coord-y coord-input))) 1924923))))

(ert-deftest day2-part2 ()
  (let ((pos-test-input (travel "./test-input"))
        (pos-input (travel "./input")))
    (should (equal (* (abs (pos-x pos-test-input))
                      (abs (pos-y pos-test-input))) 900))
    (should (equal (* (abs (pos-x pos-input))
                      (abs (pos-y pos-input))) 1982495697))))
