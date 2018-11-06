#lang racket

(provide make-planner)
;niet gebruikt

(define (make-planner)
  (let ((trajectories '()))

    (define (add-trajectory trajectory)
      (set! trajectories (cons trajectory trajectories))
      (execute-trajectory trajectory))

    (define (execute-trajectory trajectory)
      (displayln "executing trajectory")
      ((trajectory 'execute)))

    (define (dispatch msg)
      (cond ((eq? msg 'add-trajectory) add-trajectory)))

    dispatch))