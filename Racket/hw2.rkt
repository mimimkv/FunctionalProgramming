#lang racket



;task1
(define (cartesian-product xs ys)
  (define (helper i j lst) ;i and j are counters for xs and xy, respectively
    (cond [(null? i) lst]
          [(null? j) (helper (cdr i) ys lst)]
          [else (helper i (cdr j) (cons (cons (car i) (car j)) lst))]))
  (reverse (helper xs ys '())))

(cartesian-product '(1 2) '(3 4))
(cartesian-product '(1 2 3 4 5) '(6 7))
(cartesian-product '(1 2 3 4 5) '(6 7 8))

;task2

;we define a helper function that checks if a number is prime
(define (isPrime n)
  (define (helper d)
    (cond  [(= d n) #t]
           [(= (remainder n d) 0) #f]
           [else (helper (+ d 1))]))
  (if (= n 1)
      #f
      (helper 2)))


(define (factorize n)
  (define (helper i lst num) ;i is a candidate divisor
    (cond [(< num 2) lst] 
          [(and (isPrime i)
                (= (remainder num i) 0))
           (if (= (remainder (/ num i) i) 0)
               (helper i (cons i lst) (/ num i))
               (helper (+ i 1) (cons i lst) (/ num i)))]
          [else    (helper (+ i 1) lst num)]))
  (reverse (helper 2 '() n)))

(factorize 6)
(factorize 13)
(factorize 123)
(factorize 152)
(factorize 32)