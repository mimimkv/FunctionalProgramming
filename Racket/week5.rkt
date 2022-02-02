#lang racket

;Зад. 1. Да се дефинира процедура от по-висок ред (derive2 f eps),
;която намира втора производна на едноаргументната реална функция f с точност eps.
(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x )) eps)))
(define (g x)
  (* 2 x x))

(define (derive2 f eps)
  (derive (derive f eps) eps))
(define g-s
  (derive2 g 0.001))
(g-s 2)
(g-s 10)

;Зад. 2. Да се дефинира процедура от по-висок ред (derive-n f n eps), която намира
;n-та производна на едноаргументната реална функция f с точност eps.

(define (derive-n f n eps)
  (if (= n 0)
      f
      (derive (derive-n f (- n 1) eps) eps)))
((derive-n g 3 0.001) 10)

;Зад. 3. Да се дефинира процедура от по-висок ред (repeated f n), която намира
;n-кратна композиция на едноаргументната реална функция f:

;а) без използване на процедурата my-compose от зад. 2. б, Упражнение 4;


; f n ->  f(f (f....(f x)....)))
(define (repeated f n)
  (define (helper f n x)
    (if (= n 0)
        x
        (f (helper f (- n 1) x))))
  (λ (x) (helper f n x)))

((repeated (λ (x) (+ x 1)) 5) 1)


(define (repeated-1 f n)
  (λ (x)
    (if (= n 0)
        x
        (f ((repeated-1 f (- n 1)) x)))))
((repeated-1 (λ (x) (+ x 1)) 5) 1)

;б) с използване на процедурата my-compose от зад. 2. б, Упражнение 4.

(define (my-compose f g)
  (λ (x) (f (g x))))

(define (repeated-c f n)
  (if (= n 1)
      f
      (my-compose f (repeated-c f (- n 1)))))
((repeated-c (λ (x) (+ x 1)) 5) 1)

;Зад. 4. Да се дефинират процедури от по-висок ред (derive-x f eps) и (derive-y f eps),
;които намират съответно първа частна производна по x и първа частна производна по y
;на двуаргументната реална функция f(x,y) с точност eps.



;Зад. 5. Да се дефинира функция (newton-sqrt x), която пресмята корен квадратен по метода на
;Нютон с итеративно подобряване на приближението (https://en.wikipedia.org/wiki/Newton%27s_method).

(define (newton-sqrt x)
  (define (iter n sqrt-x)
    (if (= n 0)
        sqrt-x
        (iter (- n 1)
              (- sqrt-x (/ (- (* sqrt-x sqrt-x) x)
                           (* 2 sqrt-x))))))
  (iter 10 (/ x 2)))
(newton-sqrt 625)

;Зад. 6.

;а) Да се реализира фунцкия (sum-digit-divisors n), която намира сумата на положителните цифри на
;дадено естествено число, които са му делители.


(define (sum-digit-divisors n)
  (define (helper k)
    (cond [(= k 0)                              0]
          [(= (remainder k 10) 0)               (helper (quotient k 10))]
          [(= 0 (remainder n (remainder k 10))) (+ (remainder k 10)
                                                   (helper (quotient k 10)))]
          [else                                 (helper (quotient k 10))]))
  (helper n))

(sum-digit-divisors 2524)
(sum-digit-divisors 12340)
(sum-digit-divisors 21399)
(sum-digit-divisors 6564)
(sum-digit-divisors 8764)

                    

;б) Да се реализира фунцкия (same-sum a b), която намира броя на двойките числа (m, n), за които
;a <= m < n <= b и функцията sum-digit-divisors връща един и същ резултат.

;Пример: (same-sum 28 35) -> 2 ; двойките са (28,32) и (29,34)

(define (same-sum a b)
  (define (helper-m m)
    (define (helper-n n)
      (cond [(> n b) 0]
            [(= (sum-digit-divisors m)
                (sum-digit-divisors n)) (+ 1 (helper-n (+ n 1)))]
            [else                       (helper-n (+ n 1))]))
    (if (= m b)
        0
        (+ (helper-n (+ m 1)) (helper-m (+ m 1)))))
  (helper-m a))

(same-sum 1 100)
(same-sum 28 35)

