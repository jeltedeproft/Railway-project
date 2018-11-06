#lang racket

(provide make-ADT-switch-nmbs)
(require "../utility_functions.rkt")
(require "../ADT_NMBS--INFRABEL.rkt")

(define (make-ADT-switch-nmbs switch)
  (let* ((switch-symbol (car switch))
         (name  (symbol->string(car (cdr switch))))
         (always-connected-node (symbol->string (car (cdr (cdr switch)))))
         (variable-connected-nodes (car (cdr (cdr (cdr switch))))))
    
    (define  (get-name)
      name)
    
    (define  (get-type)
      switch-symbol)
    
    (define  (get-lock)
      (send-get-switch-lock name))
    
    (define  (get-always-connected-node)
      always-connected-node)
    
    (define  (get-state)
      (send-get-switch-position (string->symbol name)))

    (define (get-number-from-node node)
      (cond ((string=? node (symbol->string (car variable-connected-nodes)))
               1)
           ((string=? node (symbol->string (car (cdr variable-connected-nodes))))
               2)
           (else #f)))
             
    
    
    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-name) get-name)
           ((eq? msg 'get-type) get-type)
           ((eq? msg 'get-always-connected-node) get-always-connected-node)
           ((eq? msg 'get-state) get-state)
           ((eq? msg 'get-lock) get-lock)
           ((eq? msg 'get-number-from-node) get-number-from-node)
           ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    
    dispatch))