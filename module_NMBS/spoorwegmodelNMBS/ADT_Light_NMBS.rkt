#lang racket

(provide make-ADT-light-nmbs)

(define (make-ADT-light-nmbs color)
  (let ((current_color color))
    
    (define  (get-color)
      current_color)

    (define (set-color color)
      (set! current_color color))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-color) get-color)
           ((eq? msg 'set-color) set-color)))
    
    dispatch))