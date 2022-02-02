#lang racket

;Зад. 5. Да се дефинира функция (newton-sqrt x), която пресмята корен квадратен по метода на Нютон с
;итеративно подобряване на приближението (https://en.wikipedia.org/wiki/Newton%27s_method).

;x^2 = a
(define (newton-sqrt x)
  (define (iter n sqrt-x)
    (if (= n 0)
        sqrt-x
        (iter (- n 1)
              (- sqrt-x (/ (- (* sqrt-x sqrt-x) x)
                           (* 2 sqrt-x)))))) ;f(x) = x^2 - a -> f(x)' = 2*x
  (iter 10 (/ x 2)))
(newton-sqrt 625)
  

;Зад. 6.
;а) Да се реализира фунцкия (sum-digit-divisors n), която намира сумата на
;положителните цифри на дадено естествено число, които са му делители.
; polojitelni == toest ako ima 0, preskachame zashtoto na 0 ne se deli
(define (sum-digit-divisors n)
  (define (helper k)
    (cond [(= k 0)                              0]
          [(= (remainder k 10) 0)               (helper (quotient k 10))]
          [(= 0 (remainder n (remainder k 10))) (+ (remainder k 10)
                                                   (helper (quotient k 10)))]
          [else                                 (helper (quotient k 10))]))
  (helper n))

(sum-digit-divisors 2524)
           
;б) Да се реализира фунцкия (same-sum a b), която намира броя на
;двойките числа (m, n), за които a <= m < n <= b и функцията
;sum-digit-divisors връща един и същ резултат.
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



(define (any? pred? xs)
        (if (null? xs)
            #f
            (or (pred? (car xs))
                (any? pred? (cdr xs)))))

(map (λ (x) (* x 2)) '(1 2 3 4 5 6)) 
(filter (λ (x) (= 0 (remainder x 2))) '(1 2 3 4 5 6))
(range 1 10) ;1 to 9
(range 20) ; 0 to 19
(range 1 20 3) ; 1 to 20 with step 3
(range 20 1 -3)

;Задача 1. Функция (removeDuplicates xs), която премахва всички повторни
;срещания на елементи от списъка.

(define (remove-duplicates xs)
(if (null? xs)
    '()
    (cons (car xs)
          (remove-duplicates
           (filter (λ (x) (not (= x (car xs))))
                   (cdr xs))))))

(remove-duplicates '(1 2 2 2 4 5 6 2))




;Задача 2. Функция (sublistBetween start end xs), която взима подсписъка
;на xs между позициите start и end.


(take (range 20) 5) ;0 1 2 3 4 -takes the first n elements
(drop (range 20) 5) ; 5 - 20 -> skips the first n elements

(take-right (range 20) 5) ; 15-19 ->takes the last n elements
(drop-right (range 20) 5); 0-14 -> drops the last n els

(drop-right '(1 2 30) 1) ; 1 2


(define (sublistBetween start end xs)
  (take (drop xs start) (- end start)))
(sublistBetween 3 10 (range 20)) ; 3-9

(define (sublstBetween1 start end xs)
  (take (drop xs start) (+ 1 (- end start))))
(sublstBetween1 3 10 (range 20))



(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (sublist-between start end xs)
    (drop-right (drop xs start) (- (length xs) (+ end 1))))


(sublist-between 2 9 '(1 2 3 4 5 6 7 8 9 10))



;Задача 3. Функция (countOcccurrences subxs xs), която връща броя
;срещания на subxs в xs.

;(count-occurences '(1 2) '(1 2 2 1 2)) -> 2

(define (countOccurrences subxs xs)
  (define (helper xs k)
    (cond [(> k (length xs)) 0]
          [(equal? subxs (take xs k))
             (+ 1 (helper (cdr xs) k))] 
          [else (helper (cdr xs) k)]))
  (helper xs (length subxs)))

(countOccurrences '(1 2) '(1 2 2 1 2))
(countOccurrences '(1 1) '(1 2 2 1 1 1 2)) 



;Задача 4. Функция (ordered? xs pred), която проверява дали списък
;е сортиран възходящо/низходящо според подадена функция за сравнение.

;(oredered? '(1 2 3 6 7) (λ (a b) (< a b))) -> #t
;(oredered? '(1 2 3 6 7) (λ (a b) (> a b))) -> #f

(define (ordered? xs pred)
  (cond [(null? (cdr xs)) #t]
        [(pred (car xs) (car (cdr xs)))
           (ordered? (cdr xs) pred)]
        [else #f]))

(ordered? '(1 2 3 6 7) (λ (a b) (< a b))) ; -> #t
(ordered? '(1 2 3 6 7) (λ (a b) (> a b))) ; -> #f



(define (ordered-2? xs pred)
  (map cons (drop-right xs 1) (cdr xs)))

(ordered-2? '(1 2 3 6 7) (λ (a b) (< a b)))
;'((1 . 2) (2 . 3) (3 . 6) (6 . 7))
(ordered-2? '(1 2 3 6 7) (λ (a b) (> a b)))
;'((1 . 2) (2 . 3) (3 . 6) (6 . 7))


(define (ordered-3? xs pred)
  (map (λ (p) (pred (car p) (cdr p)))
       (map cons (drop-right xs 1) (cdr xs))))
(ordered-3? '(1 2 3 6 7) (λ (a b) (< a b)))
;'(#t #t #t #t)
(ordered-3? '(1 2 3 6 7) (λ (a b) (> a b)))
;'(#f #f #f #f)
(ordered-3? '(1 2 3 6 7 1) (λ (a b) (> a b)))
;b/n 7 and 1 is true
;'(#f #f #f #f #t)


(define (ordered-4? xs pred)
  (andmap (λ (p) (pred (car p) (cdr p)))
       (map cons (drop-right xs 1) (cdr xs))))
(ordered-4? '(1 2 3 6 7) (λ (a b) (< a b))) ;#t
(ordered-4? '(1 2 3 6 7) (λ (a b) (> a b))) ; #f
(ordered-4? '(1 2 3 6 7 1) (λ (a b) (> a b))) ;#f



(define (ordered-5? xs pred) 
  (andmap pred (drop-right xs 1) (cdr xs)))

(ordered-5? '(1 2 3 6 7) <) ;#t