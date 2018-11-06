#lang racket

(require "../simulator_treinstation/interface.rkt")
(require "../z21/z21-scheme/FullAPI/Z21Socket.rkt")
(require "../z21/z21-scheme/FullAPI/Z21MessageLocation.rkt")
(require "../z21/z21-scheme/FullAPI/Z21MessageUtil.rkt")
(require "../z21/z21-scheme/FullAPI/Z21MessageDriving.rkt")
(require "../z21/z21-scheme/FullAPI/Z21MessageSwitches.rkt")
(require "../z21/z21-scheme/APITesting/MessageHandler.rkt")


(provide
         get-loco-detection-block-z21
         get-switch-position-z21
         start-simulation
         stop-simulation
         set-switch-position-z21!
         set-loco-speed-z21!
         get-loco-speed-z21)
;deze netwerk UDP versie van de communicator tussen infrabel en de z21
;is niet geimplementeerd.
;Wat hier moet gebeuren, is dat de 7 procedures hieronder moeten vertaald worden naar de hardware versie
;als we ervoor zorgen dat de naam en return waarden identiek blijven, hoeft er niets aan de andere code gewijzigd te worden


;maak socket verbinding met z21
(define socket (setup))

(listen socket handle-msg)

(define (get-loco-detection-block-z21 lid)
  (define get-location-message (make-rmbus-get-data-msg "00"))
  (send socket get-location-message))

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

(get-loco-detection-block-z21 1)


      