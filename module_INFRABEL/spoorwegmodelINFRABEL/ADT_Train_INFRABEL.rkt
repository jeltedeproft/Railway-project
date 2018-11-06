#lang racket

(require "../adt_INFR-Z21.rkt")

(provide make-ADT-train-mivb)

(define (make-ADT-train-mivb train)
  (let* ((trainsymbol (car train))
        (name (symbol->string (car (cdr train))))
        (bussy #f)
        (last-passed-node #f)
        (direction #t)
        (last-seen-detection-block (get-loco-detection-block-z21 (string->symbol name))))

    ;update the last seen detection block if necesary
   (define  (get-last-seen-detection-block)
     (let ((pos (get-loco-detection-block-z21  (string->symbol name))))
       (if (eq? pos #f)
          last-seen-detection-block
          (if (not (eq? pos last-seen-detection-block))
            (begin
              (set! last-seen-detection-block pos)
              pos)
            pos))))

    (define (get-current-detection-block)
      (get-loco-detection-block-z21 name))
    
    (define (get-name)
      name)

    (define (get-status)
      bussy)

    (define (set-status! boolean)
      (set! bussy boolean))

    (define (get-last-passed-node)
      last-passed-node)

    (define (get-direction)
      direction)

    ;true = forward, false = backward
    (define (set-direction! boolean)
      (set! direction boolean))

    (define (set-last-passed-node! nodeid)
      (set! last-passed-node nodeid))

    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))

    (define (dispatch msg)
      (cond ((eq? msg 'get-last-seen-detection-block) get-last-seen-detection-block)
           ((eq? msg 'get-name) get-name)
           ((eq? msg 'get-status) get-status)
           ((eq? msg 'get-current-detection-block) get-current-detection-block)
           ((eq? msg 'set-status!) set-status!)
           ((eq? msg 'get-last-passed-node) get-last-passed-node)
           ((eq? msg 'get-direction) get-direction)
           ((eq? msg 'set-direction!) set-direction!)
           ((eq? msg 'set-last-passed-node!) set-last-passed-node!)
           ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    dispatch))
