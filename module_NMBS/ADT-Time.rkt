#lang racket

(provide make-timestamp)

;dit adt was bedoeld voor de planner die uiteindelijk niet geimplementeerd werd
(define (make-timestamp hour minute sec)

  (define hourtime hour)
  (define minutetime minute)
  (define secondtime sec)

  (define time-as-string (string-append (number->string hourtime) " : " (number->string minutetime) " : " (number->string secondtime)))

  (define (get-hour)
    hourtime)

  (define (get-minute)
    minutetime)

  (define (get-second)
    secondtime)

  (define (dispatch msg)
    (cond ((eq? msg 'get-hour) get-hour)
         ((eq? msg 'get-minute) get-minute)
         ((eq? msg 'get-second) get-second)
         ((eq? msg 'time-as-string) time-as-string)))
  dispatch)