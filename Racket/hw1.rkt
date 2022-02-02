#lang racket

;task 1
(define (automorphic? n)
  (define (helper num square)
    (cond   [(= num 0) #t]
            [(= (remainder num 10) (remainder square 10))
               (helper (quotient num 10) (quotient square 10))]
            [else   #f]))
    
  (if (= n (* n n)) 
      #t
      (helper n (* n n))))

(automorphic? 5)
(automorphic? 25)         
(automorphic? 36) 
(automorphic? 890625)


;task 2
;Примери:(nth-cuban 1)    -> 7      (23-13)
;(nth-cuban 4)   -> 61     (53-43)
;(nth-cuban 50)  -> 55897  (1373-1363)
;(nth-cuban 100) -> 283669 (3083-3073)

;here we define a function that checks if a number is prime
(define (prime? n)
  (define (helper d)
    (cond [(= d n) #t]
          [(= 0 (remainder n d)) #f]
          [else (helper (+ d 1))]))
  (if (= n 1)
      #f
      (helper 2)))

;we define a function that calculates the difference between the cubes of the two numbers
(define (diff a b)
  (- (expt b 3) (expt a 3)))

;curr is the current, i is a counter, it checks if we have reached the nth number
;number is the number we want to find
;curr and next define the sequential numbers that we raise to the third power
(define (nth-cuban n)
  (define (helper curr next i number)
    (cond [(>= i n) number]
          [(prime? (diff curr next))
           (helper next (+ 1 next) (+ i 1) (diff curr next))]
          [else    (helper next (+ next 1) i number)]))
  (helper 1 2 0 0))
(nth-cuban 1)
(nth-cuban 2)
(nth-cuban 3)
(nth-cuban 4)
(nth-cuban 50)
(nth-cuban 100)




(define (cubic-diff n) (- (expt n 3) (expt (- n 1) 3)))
(define (nth-cuban1 n)
  (define (helper k counter)
    (cond [(= n counter)                 (cubic-diff (- k 1))] 
          [(prime? (cubic-diff k))       (helper (+ 1 k) (+ 1 counter))]
          [(not (prime? (cubic-diff k))) (helper (+ 1 k) counter)]))
  (helper 2 0))
