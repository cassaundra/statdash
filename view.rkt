#lang racket

(require "clock.rkt"
         "input.rkt"
         "state.rkt"
         charterm
         lux
         raart
         raart/lux-chaos
         racket/gui/base)

(define (make-terminal-view)
  (terminal-view (make-state)))

(struct terminal-view (state)
  #:methods gen:word
  [(define (word-fps w)
     30.0)
   (define (word-label w ft)
     "statdash")
   (define (word-event w e)
     ;; (define new-state (handle-input (terminal-view-state w)))
     ;; (terminal-view new-state)
     (let* ([state (terminal-view-state w)]
            [new-state (handle-input state e)])
       (and new-state
            (terminal-view new-state)))
     )
   (define (word-tick w)
     w)
   (define (word-output w)
     (match-define (state width height) (terminal-view-state w))
     (if (not (eq? width #f))
       (without-cursor
        (matte width height
               (vappend #:halign 'center
                        (text (~a (timestamp))))))
       (blank))
     )
   (define (word-return w)
     w)])

(provide make-terminal-view)
;; (thread (thunk (system (format "espeak-ng --punct=\"<>()\" \"key: ~a\"" e))))
;; (send (new dialog% [label (format "Key: '~a'" e)]) show #t)
;; (if (equal? e "q") #f w)
;; w
