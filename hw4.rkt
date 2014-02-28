
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
             (car (list-tail xs i)))]))

; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Problem 5
(define funny-number-stream
  (letrec ([funny1 (lambda (x)
                     (let ([x1 (if (= 0 (remainder x 5)) (- x) x)]) 
                       (cons x1 (lambda () (funny1 (+ x 1))))))])
    (lambda () (funny1 1))))

; Problem 6
(define dan-then-dog
  (letrec ([dan (lambda (x) (cons (if x "dan.jpg" "dog.jpg")
                                  (lambda () (dan (not x)))))])
    (lambda () (dan #t))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([zerof (lambda(s) (cons (cons 0 (car (s)))
                                   (lambda () (zerof (cdr (s))))))])
    (lambda() (zerof s))))
  
; Problem 8
(define (cycle-lists xs ys)
  (letrec ([zip (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                   (lambda () (zip (+ n 1)))))])
    (lambda () (zip 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([vecc (lambda (n)
                   (cond [(= n (vector-length vec)) #f]
                         [(pair? (vector-ref vec n))
                           (if (equal? v (car (vector-ref vec n)))
                           (vector-ref vec n)
                           (vecc (+ n 1)))]
                         [#t (vecc (+ n 1))]))])
  (vecc 0)))

; Problem 10
(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [i 0] [j 1]
           [f (lambda (x)
                (let ([ans (vector-assoc x vec)])
                  (if ans ans
                      (let ([new-ans (assoc x xs)])
                        (if new-ans
                            (begin (vector-set! vec i new-ans)
                              (set! i j)
                              (if (= j (- n 1))
                                  (set! j 0) (set! j (+ j 1)))
                              new-ans)
                            new-ans)))))])
    f))

; Problem 11 (Challenge)

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1] [y e2])
       (letrec ([fe (lambda (e)
                (if (< e x)
                    (begin (let ([f e2]) (fe f)))
                    #t))])
       (fe y)))]))
  