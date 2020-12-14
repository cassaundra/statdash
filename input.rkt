#lang racket

(require raart/lux-chaos
         "state.rkt")

(define (handle-input evt)
  (let ([state (current-state)])
    (match evt
     [(struct screen-size-report (height width))
      (send state set-size (cons width height))]
     ["C-C" (send state kill)]
     [else
      (cond
        [(send state current-interaction)
         (send (send state current-interaction) handle-input evt)]
        [(send state get-focused-buffer)
         (send (send state get-focused-buffer) handle-input evt)]
        )]
     )))

(provide handle-input)
