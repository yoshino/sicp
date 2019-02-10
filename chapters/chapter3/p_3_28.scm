(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

; 半加算器
(define (half-adapter a b s c)
  (let ((d (make-wire))
        (e make-wire))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; 全加算器
(define (full-adapter a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adapter b c-in s c1)
    (half-adapter a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; インバーター
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; Andゲート
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invarid signal" s1 s2))))

;----------------------------------------------------------------
; 3.28: Or ゲート
;----------------------------------------------------------------
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
        or-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invarid signal" s1 s2))))

;------------------------------------------------------------
; 3.29: OrゲートをANDゲートとInverterで書き換える
;------------------------------------------------------------
; OR
; A: B: RESULT:
; 1  0   1
; 0  1   1
; 1  1   1
; 0  0   0

;この結果がANDの結果を反転させたものなので、

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-not (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        (+ and-gate-delay inverter-delay)
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; こういう答え方が正解らしい。。。
(define (or-gate a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gete c d e)
    (inverter e output)
    'ok))

; 遅延時間は、2*inverter-delay + and-gate-delay
