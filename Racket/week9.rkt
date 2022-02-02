#lang racket

;Задача 2. "Ниво на влагане" на атом в дълбок списък наричаме броя пъти, който трябва да се приложи операцията car за достигане до атома.
;Да се реализира функция deep-delete, която в даден дълбок списък изтрива всички атоми, които са по-малки от нивото им на влагане.
;Пример: (deep-delete `(1 (2 (2 4) 1) 0 (3 (1)))) ; -> (1 (2 (4)) (3 ())))

(define (deep-delete ls)
  (define (helper level lscur)
    (cond [(null? lscur) '()] 
          [(list? (car lscur)) (cons (helper (+ level 1)
                                             (car lscur))
                                     (helper level (cdr lscur)))]
          [(< (car lscur) level)  (helper level (cdr lscur))]
          [else (cons (car lscur) (helper level (cdr lscur)))]))
  (helper 1 ls))

(deep-delete '(1 (2 (2 4) 1) 0 (3 (1))))


;Задача 3. Да се дефинира предикат (hasMatchingLengths l1 l2). l1 и l2 са непразни
;списъци от списъци от числа. Ако l1 = '(a1 a2 … ak), а l2 = '(b1 b2 … bk),
;предикатът да връща истина, когато разликите в дължините на всички двойки
;съответните списъци ai и bi са еднакви.

(define (hasMatchingLengths l1 l2)
  (define (helper l1 l2 d)
    (cond [(null? l1) #t]
          [(not (= (- (length (car l2)) (length (car l1))) d)) #f]
          [else (helper (cdr l1) (cdr l2) d)]))
  (helper (cdr l1)
          (cdr l2)
          (- (length (car l2)) (length (car l1)))))


(hasMatchingLengths '((1 2) (1 2) (1 3)) '((4) (5) (6))) ; t
(hasMatchingLengths '((1) (2) (3)) '((4 4) (6) (7))) ; f


(define (hasMatchingLength l1 l2)
  (define (helper diff l1 l2)
    (cond [(null? (cdr l1)) #t]
          [(= diff (- (length (cadr l1)) (length (cadr l2))))
           (helper diff (cdr l1) (cdr l2))]
          [else #f]))
  (helper (- (length (car l1)) (length (car l2))) l1 l2))
(hasMatchingLength '((1 2) (1 2) (1 3)) '((4) (5) (6))) ; t
(hasMatchingLength '((1) (2) (3)) '((4 4) (6) (7))) ; f


(define (has-matching-lengths-2? l1 l2)
  (define d (- (length (car l2)) (length (car l1))))
  (andmap (λ (ai bi) (= d (- (length bi) (length ai))))
          (cdr l1)
          (cdr l2)))

(has-matching-lengths-2? '((1 (2 3 4)) (1 2) (1 3)) '((4) (5) (6)))
(has-matching-lengths-2? '((1) (2) (3)) '((4 4) (6) (7)))


(define (hasMatchingLengths-1 l1 l2)
  (apply = (map - (map length l1) (map length l2))))

(hasMatchingLengths-1 '((1 (2 3 4)) (1 2) (1 3)) '((4) (5) (6)))
(hasMatchingLengths-1 '((1) (2) (3)) '((4 4) (6) (7)))



(define (f x)
  (if (< x 10)
      (error "message")
      (* x 2)))
;(f 0)
(f 20)


(define xs '(2 2 2 2 2 2 ))
(andmap (λ (x) (= x (car xs))) (cdr xs)) ;t
(map + '(1 2 3) '(2 2 2)) ; 3 4 5
