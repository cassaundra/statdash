#lang racket

(require raart/lux-chaos
         "state.rkt")

(define (handle-input current-state evt)
  (match evt
    ["q" #f]
    [(struct screen-size-report (height width)) (state width height)]
    [_ current-state]))

(provide handle-input)
