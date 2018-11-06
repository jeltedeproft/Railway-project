#lang racket

(provide make-ADT-switch-mivb)
(require "../utility_functions.rkt")
(require "../adt_INFR-Z21.rkt")

(define (make-ADT-switch-mivb switch)
  (let* ((switch-symbol (car switch))
         (name  (symbol->string(car (cdr switch))))
         (locked? #f)
         (always-connected-node (symbol->string (car (cdr (cdr switch)))))
         (variable-connected-nodes (car (cdr (cdr (cdr switch)))))
         (variable-connected-node (car variable-connected-nodes)))
    
    (define  (get-name)
      name)

        (define  (get-state)
      (get-switch-position-z21 name))
    
    (define  (get-lock)
      locked?)
    
    (define  (set-lock! boolean)
      (set! locked? boolean))
    
    (define  (get-always-connected-node)
      always-connected-node)
    
    (define  (get-variable-connected-nodes)
      (list-of-integers-to-list-of-strings variable-connected-nodes))
    
    (define (set-variably-connected-node nodeid)
      (set! variable-connected-node nodeid))
    
    
    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-name) get-name)
           ((eq? msg 'get-always-connected-node) get-always-connected-node)
           ((eq? msg 'get-variable-connected-nodes) get-variable-connected-nodes)
           ((eq? msg 'set-variably-connected-node) set-variably-connected-node)
           ((eq? msg 'get-lock) get-lock)
           ((eq? msg 'set-lock!) set-lock!)
           ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    
    dispatch))