#lang racket

(provide last-element)
(provide list-of-integers-to-list-of-strings)
(provide remove-dup)
(provide reverse)
(provide append-element)

(define (last-element l)
  (cond ((null? (cdr l)) (car l))
        (else (last-element (cdr l)))))

(define (list-of-integers-to-list-of-strings list-of-integers)
  (map (lambda (int) (number->string int))
      list-of-integers))

;verwijder dubbels
(define (remove-dup lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (car lst))
        ((equal? (car lst) (car (cdr lst))) (remove-dup (cdr lst)))
        (else (cons (car lst) (remove-dup (cdr lst))))))

;keer een lijst om
(define (reverse1 l)
  (if (null? l)
     null
     (append (reverse1 (cdr l)) (list (car l)))))

;voeg element toe aan einde van een lijst
(define (append-element lst elem)
  (append lst (list elem)))


