#lang racket

(require "../simulator_treinstation/interface.rkt")


(provide
         get-loco-detection-block-z21
         get-switch-position-z21
         start-simulation
         stop-simulation
         set-switch-position-z21!
         set-loco-speed-z21!
         get-loco-speed-z21)
(start-simulator)

(define (get-loco-detection-block-z21 lid)
  (get-loco-detection-block lid))

(define (get-switch-position-z21 snode)
  (get-switch-position snode))

(define (start-simulation)
  (start-simulator))

(define (stop-simulation)
  (stop-simulator))

(define (set-switch-position-z21! snode position)
  (set-switch-position! snode position))

(define (set-loco-speed-z21! lid speed)
  (set-loco-speed! lid speed))

(define (get-loco-speed-z21 lid)
  (get-loco-speed lid))


      