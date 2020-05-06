#lang racket

;;cw 5
(define (insert l xs)
  (if [null? xs]
      (list l)
      (if [> (car xs) l]
          (cons l xs)
          (cons (car xs)
                (insert l (cdr xs))))))

(insert 42 '(1 2 4 33 40 41  44 45))
(insert 1 '(0 0 0 0 0 0 0))
(insert 1 '(2 2 2 2 2 2 2))

;;cw 6
(define (permi xs)
  (define (iter w res)
    (if [null? res]
        w
        (iter (append w (map (λ(i) (cons (car res) i)) (cdr res)))
              (cdr res))))
  (iter '() xs))


(displayln "\nPERMI:")
(permi '(1 2 3 4))