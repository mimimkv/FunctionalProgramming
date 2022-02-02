#lang racket


;Задача 1.Да се дефинира myfib, която получава един аргумент n и връща n-тото число на Фибоначи, чрез линейно итеративен процес.
;(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)
(define (fib n)
  (cond [(= n 0) 1]
        [(= n 1) 1]
        [else (+ (fib (- n 2)) (fib (- n 1)))]))
(fib 4)

(define (myfib n)
  (helper 1 1 1 n))

(define (helper prev cur i n)
  (if (>= i n)
      cur
      (helper cur (+ prev cur) (+ i 1) n)))
  
(myfib 3)
(myfib 50)

;Задача 2. Да се напише функция mygcd a b, която връща НОД(a, b).
(define (gcd a b) 
  (cond [(= a b) a]
   [(> a b) (gcd (- a b) b)]
   [else (gcd a (- b a))]))
(gcd 5 15)

;Задача 3. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1, за който d < x.
(define (maxdivisor x)
  (helper1 (- x 1) x))

(define (helper1 d x)
  (if (= (remainder x d) 0)
      d
      (helper1 (- d 1) x)))

(maxdivisor 21)

(define (maxdiv1 x)
  (define (helper1 d)
    (if (= (remainder x d) 0)
        d
        (helper1 (- d 1))))
  (helper1 (- x 1)))
(maxdiv1 21)

;Задача 4. Да се дефинира функция sum-odds, която намира сумата на нечетните числа в затворения интервал [a, b].
(define (sum-odds a b)
  (cond [(> a b) 0]
        [(= (remainder a 2) 1) (+ a (sum-odds (+ a 2) b))]
        [else                   (sum-odds (+ a 1) b)]))

(sum-odds 1 10)


(define (sum-odds-iter a b)
  (define (helper sum a)
    (if (> a b)
        sum
        (helper (+ a sum) (+ a 2))))
  (if (= (remainder a 2) 1)
      (helper 0 a)
      (helper 0 (+ a 1))))
(sum-odds-iter 1 10)

;Задача 5. Да се дефинира предикат prime?, който проверява дали естественото число n е просто.
(define (prime? n)
  (define (helper d)
    (cond [(= d n) #t]
          [(= 0 (remainder n d)) #f]
          [else                  (helper (+ d 1))]))
  (cond [(= n 1) #f]
        [(= n 2) #t]
        [else    (helper 2)]))
        
(prime? 1)
(prime? 2)
(prime? 3)
(prime? 15)
(prime? 17)

(define (prime1? n)
  (define (helper d)
    (cond [(= d n)               #t]
          [(= 0 (remainder n d)) #f]
          [else                  (helper (+ d 1))]))
  (if (= n 1)
      #f
      (helper 2)))

(prime1? 1)
(prime1? 2)
(prime1? 4)
(prime1? 17)