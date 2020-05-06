#lang racket

;; foldr

(define (foldr op nval xs)
	(if (null? xs)
		nval
		(op (car xs) (foldr op nval (cdr xs)))))

;; foldl

(define (foldl op nval xs)
	(if (null? xs)
		nval
		(foldl op (op (car xs) nval) (cdr xs))))