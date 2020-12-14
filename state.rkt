#lang racket

(require "buffer/buffer.rkt"
         raart)

(define current-state (make-parameter #f))

(define interaction%
  (class object%
    (super-new)
    (field [proc #f])
    (define/public (complete [value #f])
      (when proc
        (proc value)))))

(define interaction/read-string%
  (class* interaction% (buffer<%>)
    (super-new)
    (init-field prompt [value ""])

    (define/public (tick)
      void)
    (define/public (handle-input evt)
      (cond
        [(string=? evt "C-M") (send this complete value)]
        [(= 1 (string-length evt)) (set! value (string-append value evt))]))
    (define/public (render size)
      (text (format "~a: ~a" prompt value)))
    ))

(define (read-string prompt)
  (let ([state (current-state)]
        [ia (new interaction/read-string% [prompt prompt])])
    (send state run-interaction ia)))

(define interaction/read-boolean%
  (class* interaction% (buffer<%>)
    (super-new)
    (init-field prompt)

    (define/public (tick)
      void)
    (define/public (handle-input evt)
      (case evt
        [("y" "Y") (send this complete #t)]
        [("n" "N") (send this complete #f)]))
    (define/public (render size)
      (text (format "~a (y or n)" prompt)))
    ))

(define (read-boolean prompt)
  (let ([state (current-state)]
        [ia (new interaction/read-boolean% [prompt prompt])])
    (send state run-interaction ia)))

(define interactive-prompt-tag
  (make-continuation-prompt-tag 'interactive))

(define (interactive proc)
  (send (current-state) call-interactively proc))

(define (abort-interactive)
  (call-with-composable-continuation
   (λ (k)
     (abort-current-continuation interactive-prompt-tag k))
   interactive-prompt-tag))

(define state%
  (class object%
    (super-new)

    ;; --- Life ---

    (define alive #t)

    (define/public (is-alive) alive)
    (define/public (kill) (set! alive #f))

    ;; --- Frame size ---

    (define size #f)

    (define/public (get-size)
      size)
    (define/public (set-size new-size)
      (set! size new-size))

    ;; --- Buffers ---

    (define buffers empty)

    (define/public (get-buffers)
      buffers)
    (define/public (get-focused-buffer)
      ;; TODO
      (car buffers))
    (define/public (push-buffer buffer)
      (set! buffers (cons buffer buffers)))

    ;; --- Interactions ---

    (define interactions empty)

    (define/public (call-interactively proc)
      (call-with-continuation-prompt
       (λ ()
         (proc))
       interactive-prompt-tag
       (λ (k)
         (set-field! proc (send this current-interaction) k))
       ))
    (define/public (run-interaction ia)
      ;; push interaction
      (set! interactions (cons ia interactions))
      ;; call
      (define result (abort-interactive))
      ;; pop interaction
      (set! interactions (cdr interactions))
      result)
    (define/public (current-interaction)
      (and (not (empty? interactions)) (car interactions)))
    ))

(provide state%
         interactive
         read-string
         read-boolean
         current-state)
