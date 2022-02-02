#lang racket

(define (next x)
  (+ x 4))

(define next-1
  (λ (x) (+ x 4)))

(define (next-h x)
  (define (helper)
    (+ x 4))
  helper)


(define (next-h1 x)
  (define (helper x)
    (+ x 4))
  (λ (x) (helper x)))
((next-h1 2) 2)

;Зад. 1. Да се дефинира предикат (substr? a b), който проверява дали a е подниз на b,
;където a и b са естествени числа (например 123 е подниз на 5123783).
(define (suffix? a b)
  (cond [(< a 10)               (= a (remainder b 10))]
        [(not (= (remainder a 10) (remainder b 10))) #f]
        [else                     (suffix? (quotient a 10) (quotient b 10))]))
  
(define (suffix1? a b)
  (or (and (< a 10)
           (= a (remainder b 10)))
      (and (= (remainder a 10) (remainder b 10))
           (suffix1? (quotient a 10) (quotient b 10)))))

(define (substr? a b)
  (cond [(> a b)  #f]
        [(suffix? a b) #t]
        [else      (substr? a (quotient b 10))]))


(define (substr1? a b)
  (and (<= a b)
       (or (suffix? a b)
           (substr1? a (quotient b 10)))))

;Зад. 2. Дефинирайте следните функции:
;a). (my-identity x), функцията идентитет: връща каквото и дадете.

(define (my-identity x)
  x)
((my-identity (λ (x) (+ x 4))) 2)

;б). (my-compose f g), която връща композицията на функциите f и g.
; h(x) = f(g(x))

(define (my-compose f g)
  (λ (x) (f (g x))))
(my-compose (λ (x) (+ 1 x)) (λ (x) (* x 2)))
((my-compose (λ (x) (+ 1 x)) (λ (x) (* x 2))) 5)

;в). (my-negate p?), която приема предикат p? и връща предиката (not p?).
(define (my-negate p?)
  (λ (x) (not (p? x))))
(define (my-negate-comp p?)
  (my-compose not p?))

(define (even? n)
  (= 0 (remainder n 2)))
(even? 11)
((negate even?) 2)

(define (odd? n)
  ((my-negate even?) n))
(odd? 11)


(define (odd1? n)
  ((my-negate-comp even?) n))
(odd1? 13)


;г). (my-curry f x), която приема многоаргумента функция f и
;първи аргумент x и връща функцията получена от частичното
;прилагане на x върху f.

(define (f a b c)
  (+ a (* b c)))

((((curry f) 1) 2) 3)

(define (my-curry f a)
  (λ (b c) (f a b c)))
(define f-1-m
  (my-curry f 1))
(f-1-m 2 3)




;Зад. 3. Да се дефинира процедура от по-висок ред (difference F a b), която по дадени едноаргументна
;реална функция F и две реални числа a и b намира разликата F(b) - F(a).
;Да се оцени примерно орбъщение към процедурата.
(define (difference F a b)
  (- (F b) (F a)))
(difference (λ (x) (+ x 2)) 1 4)


;Зад. 4. Чрез използване на lambda израз да се дефинира процедурен обект, който е
;еквивалентен на f, ако имаме дефиницията (define (f x) (* 2 x)).
(define f1
  (λ (x) (* 2 x)))
(f1 4)


;Зад. 5. Да се дефинира процедура от по-висок ред (derive f eps), която намира първа
;производна на едноаргументната реална функция f с точност eps.

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x )) eps)))
(define (g x)
  (* 2 x x))
(define g-p
  (derive g 0.001))
(g-p 1)




