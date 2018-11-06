#lang racket

(require "../module_INFRABEL/adt_interface.rkt")

(provide
         send-execute-deeltraject
         send-get-loco-detection-block
         send-get-switch-position
         send-get-switch-lock
         send-get-loco-state
         send-set-loco-state
         send-start-model
         send-stop-model
         send-get-loco-last-passed-node
         send-get-loco-direction)

(define (send-execute-deeltraject deeltraject amount-of-parts)
  (execute-deeltraject deeltraject amount-of-parts))

(define (send-get-loco-detection-block lid)
  (get-loco-detection-block-infrabel lid))

(define (send-get-switch-position snode)
  (get-switch-position-infrabel snode))

(define (send-get-switch-lock snode)
  (get-switch-lock-infrabel snode))

(define (send-get-loco-state lid)
  (get-loco-state-infrabel lid))

(define (send-set-loco-state lid boolean)
  (set-loco-state-infrabel lid boolean))

(define (send-start-model)
  (display "1")
  (start-model-z21))

(define (send-stop-model)
  (stop-model-z21))

(define (send-get-loco-last-passed-node lid)
  (get-loco-last-passed-node lid))

;true = forward, false = backward
(define (send-get-loco-direction lid)
  (get-loco-direction lid))