#lang racket

(struct state (n))

(define (make-state)
  (state 0))

(provide
 state
 make-state)
