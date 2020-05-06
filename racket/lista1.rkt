#lang racket

;;cw 4
(define (procedure x y z)
  (if [>= x y]
      ;; x jest jedna z wiekszych (lub rownych)
      (if [>= z y]
          ;;druga jest z
          (+ (* z z) (* x x))
          ;;druga jest y
          (+ (* y y) (* x x)))
      ;; y jest jedna z wiekszych
      (if [>= z x]
          ;;druga jest z
          (+ (* z z) (* y y))
          ;;druga jest y
          (+ (* x x) (* y y)))))

;;cw 7

(define (p) (p))

(define (test x y)
   (if (= x 0)
       (p)
       y))

(test 9 7) ;; oblicza leiwie, poniewaz nie oblicz przypadku gdy x jest zero (gdy x zerem nie jest) i wyrzuca tutaj 7
           ;; a gdyby obliczalo gorliwie, obliczenia bylyby nieskonczone, wiec nie bylaby wyrzucona zadna odpowiedz

;;cw 8
(define (power-close-to b n)
  (define (iter e)
    (if [> (expt b e) n]
        e
        (iter (+ e 1))))
  (iter 0))

(power-close-to 3 27) ;; odpowiedz 4, bo 3^3 = 27, a 3^4 > 27

