#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))


(print "Test 1: ")  
(equal? (sequence 3 11 2) (list 3 5 7 9 11))
(print "Test 1: ")
(equal? (sequence 3 8 3) (list 3 6))
(print "Test 1: ")
(equal? (sequence 3 2 1) null)

(print "Test 2: ")
(equal? (string-append-map (list "foo1" "foo2" "foo3") "bar")
        (list "foo1bar" "foo2bar" "foo3bar"))

(print "Test 3: ")
(= 3 (list-nth-mod (list 1 2 3 4 5) 12))

(print "Test 5: ")
(equal? (stream-for-n-steps funny-number-stream 10)
        (list 1 2 3 4 -5 6 7 8 9 -10))

(print "Test 6: ")
(equal? (stream-for-n-steps dan-then-dog 10)
        (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg"))

(print "Test 7: ")
(equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 10)
        (list (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 -5) (cons 0 6) (cons 0 7) (cons 0 8) (cons 0 9) (cons 0 -10)))

(print "Test 8: ")
(equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 10)
        (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a") (cons 3 "b") (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b")))
(print "Test 8: ")
(equal? (stream-for-n-steps (cycle-lists (list 1 2 3 4 5) (list "a" "b" "c")) 12)
        (list (cons 1 "a") (cons 2 "b") (cons 3 "c") (cons 4 "a") (cons 5 "b") (cons 1 "c") (cons 2 "a") (cons 3 "b") (cons 4 "c") (cons 5 "a") (cons 1 "b") (cons 2 "c")))

(print "Test 9: ")
(equal? (vector-assoc 5 (vector 1 2 (cons 5 32) 3 (cons 5 33) 4 5 6 7)) (cons 5 32))

(print "Test 10: ")
(define myfunc (cached-assoc (list (cons 1 2) (cons 2 4) (cons 3 8) (cons 4 16) (cons 5 32) (cons 6 64)) 3))
(equal? (myfunc 3) (cons 3 8))