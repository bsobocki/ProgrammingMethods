#lang racket

;;cw 2
(define (compose f g)
  (λ (x)
    (f (g x))))

((compose (λ (x) (+ x 1)) (λ (x) (* x x))) 0) ;; wykona (+ (* 0 0) 1)
((compose (λ (x) (* x x)) (λ (x) (+ x 1))) 3) ;; wykona (* (+ 3 1) (+ 3 1))

;;cw 3
(define (identity x) x)

(define (repeated p n)
  (λ (x)
    (if [= n 0]
        (p x)
        (p ((repeated p (- n 1)) x)))))

((repeated (λ (x) (+ x 1)) 3) 6) ;; funkcja : (inc 6)
                                 ;; 1 zlozenie : (inc (inc 6))
                                 ;; 2 zlozenia : (inc (inc (inc 6)))
                                 ;; 3 zlozenia : (inc (inc (inc (inc 6))))
                                 ;; wynik = 10

;;cw 4
(define r-product
  (lambda (term next s e)
    (if (> s e)
        1
        (* (term s) (r-product term next (next s) e)))))

(define (i-product term next s e)
  (define (iter term next s e acc)
    (if (> s e)
        acc
        (iter term next (next s) e (* (term s) acc))))
  (iter term next s e 1))

(define PI (* 4(/
            (r-product (λ(x) (* x (+ x 2))) (λ (x) (+ 2 x)) 2 70000)
            (r-product (λ(x) (* x x)) (λ(x) (+ 2 x)) 3 70001))))
(+ 0.0 PI)

(define PI2 (* 4 (/
                  (i-product (λ(x) (* x (+ x 2))) (λ (x) (+ 2 x)) 2 70000)
                  (i-product (λ(x) (* x x)) (λ(x) (+ 2 x)) 3 70001))))
(+ 0.0 PI2)

;;cw 5
;;zostanie dokonczone