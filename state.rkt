#lang racket

(define current-state (make-parameter #f))
;; (define current-interaction (make-parameter #f))

(define interaction%
  (class object%
    (init proc)

    ;; TODO AS MODULE

    (super-new)

    (field [current-proc proc]
           [current-reader #f])

    (define/public (get-reader)
      current-reader)
    ;; (define/public (read-string prompt)
    ;; (set! current-reader (reader prompt ""))
    ;; (abort-interactive))
    (define/public (complete [success #t])
      (when success
        (current-proc (reader-value current-reader)))
      (set! current-reader #f))
    ))

(struct reader (prompt value) #:mutable #:transparent)

(define interactive-prompt-tag
  (make-continuation-prompt-tag 'interactive))

(define (interactive proc)
  (send (current-state) call-interactively proc))

(define (abort-interactive)
  (call-with-composable-continuation
   (λ (k)
     (abort-current-continuation interactive-prompt-tag k))
   interactive-prompt-tag))

(define (read-string prompt)
  (let* ([state (current-state)]
         [ia (and state (send state current-interaction))])
    (cond
      [ia
       (set-field! current-reader ia (reader prompt ""))
       (abort-interactive)]
      [else ""])))

(define (complete-interaction)
  (let* ([state (current-state)]
         [ia (and state (send state current-interaction))])
    (when ia
      (send ia complete))))

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

    ;; --- Modules ---

    ;; (define current-module (new clock%))

    ;; --- Interactions ---

    (define interactions empty)

    (define/public (call-interactively proc)
      (define interaction (new interaction% [proc proc]))
      (call-with-continuation-prompt
       (λ ()
         ;; push interaction
         (set! interactions (cons interaction interactions))
         ;; call
         (define result (proc))
         ;; pop interaction
         (set! interactions (cdr interactions))
         ;; result
         result)
       interactive-prompt-tag
       (λ (k)
         (set-field! current-proc interaction k))
       ))
    (define/public (current-interaction)
      (and (not (empty? interactions)) (car interactions)))
    ))

(provide state%
         (struct-out reader)
         interactive
         complete-interaction
         read-string
         current-state)
