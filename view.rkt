#lang racket

(require "input.rkt"
         "module/clock.rkt"
         "state.rkt"
         lux
         raart)

(define clock (new clock%))

(define (make-terminal-view)
  (terminal-view (new state%)))

(struct terminal-view (state)
  #:methods gen:word
  [(define (word-fps w)
     60.0)
   (define (word-label w ft)
     "statdash")
   (define (word-event w e)
     (let ([state (terminal-view-state w)])
       (parameterize ([current-state state])
         (handle-input e))
       (and (send state is-alive) (terminal-view state)))
     )
   (define (word-tick w)
     ;; execute top coroutine if existent and not currently entering data
     w)
   (define (word-output w)
     ;; (match-define (state size _) (terminal-view-state w))
     (define state (terminal-view-state w))
     ;; (define size (send state get-size))

     (parameterize ([current-state state])
       (draw-home state))
       ;; (without-cursor
       ;;  (place-at (matte (car size) (cdr size)
       ;;                   (vappend #:halign 'center
       ;;                            (draw-clock)))
       ;;            0 0 (text "hi")))
       )
   (define (word-return w)
     w)])

(define-syntax (bind-draw stx)
  (syntax-case stx ()
    [(_ [value-name value] proc)
     #'(let ([value-name value])
         (if value-name
            ((lambda (value-name) proc) value-name)
            (blank)))]))

(define (draw-home state)
  (define size (xor (void? state) (send state get-size)))
  (bind-draw [size size]
             (without-cursor
              (place-at (matte (car size) (cdr size)
                               (vappend #:halign 'center
                                        (send clock render size)))
                        0 0 (draw-string-reader))))
  )

(define (draw-string-reader)
  (bind-draw [interaction (send (current-state) current-interaction)]
             (let ([reader (send interaction get-reader)])
               (text (format "~a: ~a"
                             (reader-prompt reader)
                             (reader-value reader))))
             
             ))
(provide make-terminal-view)
