#lang racket

;; procedures

;  x^y
(define (power x y)
  (define (iter y)
    (if (= 1 y)
        x
        (* x (iter (- y 1)))))
  (iter y))


;;--- arithmetic expression ---;;

;; predicats

(define (operator1? o)
  (member o '(++ --)))

(define (operator2? o)
  (member o '(+ - * / ^ %  == =$ && || -= +=)))

(define (good-ops? e)
  (and [not (null? e)]
       [operator2? (second e)]
       (if (= 1 (length (cddr e)))
           true
           [good-ops? (cddr e)])))

(define (arith-expr? e)
  (and [list? e]
       [> (length e) 2]
       [= 1 (modulo (length e) 2)]
       [good-ops? e]))

(define (inc/dec? e)
  (and [list? e]
       [= (length e) 2]
       [operator1? (inc/dec-op e)]))

;; selectors

(define (op->proc c)
  (cond ;; operators 1-arg
    [(eq? c '++) (λ (x) (+ x 1))]
    [(eq? c '--) (λ (x) (- x 1))]
    ;; operator 2-args
    [(eq? c '+) +]
    [(eq? c '-) -]
    [(eq? c '/) /]
    [(eq? c '*) *]
    [(eq? c '^) (λ (x y) (power x y))]
    [(eq? c '%) (λ (x y) (modulo x y))]
    [(eq? c '==) (λ (x y) (= x y))]
    [(eq? c '=$) eq?]
    [(eq? c '&&) (λ (x y) (and x y))]
    [(eq? c '||) (λ (x y) (or x y))]
    [(eq? c '-=) (λ (x y) (- x y))]
    [(eq? c '+=) (λ (x y) (+ x y))]
    [else (error "undefined operator!")]))

(define (arith-expr-left e)
  (first e))

(define (arith-expr-op e)
  (second e))

(define (arith-expr-right e)
  (third e))

(define (arith-expr-next e)
  (cddr e))

(define (inc/dec-op e)
  (first e))

(define (inc/dec-expr e)
  (cadr e))

;; constructors

(define (arith-cons left op right)
  (list left op right))

;; evaluator

;procedura oblicza "od lewej do prawej" wartosc wyrazenia arytmetycznego
; ((((3 + 3) / 2) + 1) * 4)
(define (val-of-arith-expr e)
  (define (recursion e val)
    ;czy doszlismy do konca (tylko jeden element, liczba
    (if [= 1 (length e)]
        val
        ;jesli nie to liczymy dalej dla kolejnej czesci
        (recursion (arith-expr-next e)
                   ((op->proc (arith-expr-op e)) val (eval (arith-expr-right e))))))
  (recursion (arith-expr-next e) ((op->proc (arith-expr-op e)) (eval (arith-expr-left e)) (eval (arith-expr-right e)))))

(define (val-of-inc/dec e)
  ((op->proc (inc/dec-op e)) (eval (inc/dec-expr e))))

(define (eval e)
  (cond [(null? e ) e]
        [(number? e) e]
        [(arith-expr? e)
         (val-of-arith-expr e)]
        [(inc/dec? e)
         (val-of-inc/dec e)]
        [else (error "bad syntax!")]))

(eval '(((2 ^ (++ 2)) * 3) + (4 * 5) - 2))
(eval '(4 + 3 ^ 2))
(eval '(++ 3))
(eval '(++ (++ 4)))
(eval '(3 + (2 * 9) - (10 / 100)))