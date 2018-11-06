#lang racket

(require "utility_functions.rkt")

(provide add-to-waitlist)
(provide recheck-waitlist)

(define waitlist '())

;this adt will keep all part tracks that are put on hold togetehr
(define (add-to-waitlist parttraject totalparts)
  (set! waitlist (append-element waitlist (cons parttraject totalparts))))

(define (recheck-waitlist)
  (let ((temp '()))
    (begin
      ;store waitlist in temp variable
      (set! temp waitlist)
      ;clear out waitlist
      (set! waitlist '())
      ;return temp
      temp)))