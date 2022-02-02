#lang racket

;Задача 1. Да се дефинира функция reverse-number n, която връща число, съставено от цифрите на n, но подредени в обратен ред.
(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (reverse-number-rec n)
  (define (helper k count)
    (if (< k 10)
        k
        (+ (* (remainder k 10) (expt 10 (- count 1)))
           (helper (quotient k 10) (- count 1)))))
  (helper n (count-digits n)))
(reverse-number-rec 123)

(define (reverse-number n)
  (define (helper res k)
    (if (< k 10)
        (+ k (* res 10))
        (helper (+ (remainder k 10) (* res 10)) (quotient k 10))))
  (helper 0 n))
(reverse-number 54)


;Задача 2. Да се дефинира функция, която намира броя на палиндромите в интервала [a, b], където a и b са цели неотрицателни числа и a<b.
(define (palindrome? n)
  (= n (reverse-number n)))
(palindrome? 122222)

(define (count-palindromes a b)
  (cond [(> a b) 0]
        [(palindrome? a) (+ 1 (count-palindromes (+ a 1) b))]
        [else            (count-palindromes (+ a 1) b)]))
(count-palindromes 1 100)
 


;Задача 3. Да се дефинира функция, която чрез линейно итеративен процес намира броя на естествените делители на едно естествено число.
(define (count-divisors n)
  (define (helper count d)
    (cond [(> d n) count]
          [(= 0 (remainder n d)) (helper (+ 1 count) (+ d 1))]
          [else                   (helper count (+ d 1))]))
  (helper 0 1))
(count-divisors 6)


;Задача 4. Да се дефинира функцията (perfect-number? n), която проверява дали числото n e съвършено, т.е. дали е равно на сбора на делителите си.
(define (perfect-number? n)
  (define (helper-sum-div sum d)
    (cond [(= d n) sum]
          [(= 0 (remainder n d)) (helper-sum-div (+ d sum) (+ d 1))]
          [else                  (helper-sum-div sum (+ d 1))]))
   (= n (helper-sum-div 0 1))) ;function helper returns the sum of the divisors, we see that because in the cond we return sum
(perfect-number? 8127)
(perfect-number? 8128)
  


;Задача 5. Да се дефинира функцията (inc-digits? n), която проверява дали цифрите на числто n са подредени в нарастващ ред.
(define (inc-digits? n)
  (or (< n 10)
      (and (< (remainder (quotient n 10) 10) (remainder n 10))
           (inc-digits? (quotient n 10)))))
(inc-digits? 123)
(inc-digits? 222)

;Задача 6. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
(define (sum x n)
  (if (= n 0)
      1
      (+ (expt x n) (sum x (- n 1)))))
(sum 2 10)


;Задача 7. Да се реши задача 6, чрез използване на не повече от n умножения.
(define (sum2 x n)
  (define (helper prev i sum)
    (if (> i n)
        sum
        (helper (* prev x) (+ i 1) (+ sum prev))))
  (helper 1 0 0))
(sum 2 10)
