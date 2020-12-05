#lang racket

(struct state (width height))

(define (make-state)
  (state #f #f))

(provide
 state
 make-state)
