; 局所状態をもつ線
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (call-each procedures)
      (if (null? procedures)
          'done
          (begin ((car procedures))
                 (call-each (cdr procedures)))))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE:" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))

(define w (make-wire))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time  the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (prove name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;-----------------------------------------------------
; 3.31
;-----------------------------------------------------
; 初期化を行わないで以下のように、accept-action-procedure!を実装すると、
; どのような問題が生じるか？
;(define (accept-action-procedure! proc)
;  (set! action-procedures (cons proc action-procedures)))

; andゲートの例
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) ( get-signal a2 ))))
      (after-delay
        and-gate-delay
        (lambda () ( set-signal! output new-value))))))

; after-delayの定義
; agendaが登録されるのはこのタイミング
(define (after-delay delay action)
  (add-to-agenda! (+ delay ( current-time the-agenda))
                  action the-agenda))

; なので初期化で実行しないとafter-delayされないので予定表にも登録されない。
