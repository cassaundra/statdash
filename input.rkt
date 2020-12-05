#lang racket

(require "state.rkt")

(define (handle-input state evt)
  (case evt
    [("q") #f]
    [else state]))

(provide handle-input)
