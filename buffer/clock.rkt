#lang racket

(require "buffer.rkt"
         "../state.rkt"
         gregor
         gregor/time
         raart
         rsound)

(define clock%
  (class* object% (buffer<%>)
    (super-new)

    (define timer/timestamp #f)
    (define timer/status 'done)

    (define finish-sound (rs-read "resources/timer_finish.wav"))

    (define/public (tick)
      (cond
        [(and (eq? timer/status 'running)
              (negative? (timer/remaining)))
         (timer/ring)]
        [(and (eq? timer/status 'ringing)
              (< (timer/remaining) -2))
         (timer/stop)]))

    (define/public (render size)
      (vappend* #:halign 'center
                (filter raart?
                        (list
                         (text (~t (current-time) "HH:mm:ss"))
                         (text (time-of-day/emoji))
                         (case timer/status
                           [(running)
                            (text (format "Timer: ~as"
                                          (~r #:precision '(= 1) (timer/remaining))))]
                           [(ringing) (style 'bold (text "TIMER DONE"))]
                           [(done) (blank)])
                         )
                 )))

    (define/public (handle-input evt)
      (case evt
        [("t") (if timer/timestamp
                   (timer/stop-interactive)
                   (timer/start-interactive))]))

    (define (time-of-day)
      (let ([current-time (current-time)])
        (cond
          [(time<? current-time (time 06 00)) 'night]
          [(time<? current-time (time 12 00)) 'morning]
          [(time<? current-time (time 18 00)) 'afternoon]
          [(time<? current-time (time 20 00)) 'evening]
          [else 'night])))

    (define (time-of-day/emoji)
      (case (time-of-day)
        ['night ""]
        ['morning ""]
        ['afternoon ""]
        ['evening ""]))

    (define (timer/start-interactive)
      (interactive
       (λ ()
         (let* ([value (read-string "Duration (in minutes)")]
                [minutes (max 0(min (sub1 1400) (string->number value)))])
           (timer/set minutes)))))

    (define (timer/stop-interactive)
      (interactive
       (λ ()
         (when (read-boolean "Stop the currently running timer?")
           (timer/stop)))))

    (define (timer/set minutes)
      (set! timer/status 'running)
      (set! timer/timestamp (+ minutes (current-posix-seconds))))

    (define (timer/ring)
      (play finish-sound)
      (set! timer/status 'ringing))

    (define (timer/stop)
      (set! timer/status 'done)
      (set! timer/timestamp #f))

    (define (timer/remaining)
      (and timer/timestamp
           (- timer/timestamp (current-posix-seconds))))

    ))

(provide clock%)
