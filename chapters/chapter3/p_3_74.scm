(use util.stream)
(use srfi-27)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (scale-random-stream stream)
  (stream-map (lambda (x)
                 (* x (random-integer 100)))
                  stream))

(define (stream-head s n)
  (define counter 0)
  (define (iter s)
    (if (<= n counter)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (set! counter (+ counter 1))
        (iter (stream-cdr s)))))
  (iter s))

(define ones (stream-cons 1 ones))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;----------------------------------------------
; 3.74:ゼロ交差
;----------------------------------------------
(define sense-data
  (stream-map (lambda (x) (sin x)) integers))

(define (make-zero-crossings input-stream last-value)
  (stream-cons
    (sign-change-detector ; 0, 1, -1を適切に返す手続き
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define (sign-change-detector s2 s1)
  (cond ((and (<= 0 s2) (> 0 s1)) 1)
        ((and (> 0 s2) (<= 0 s1)) -1)
        (else 0)))

(define zero-crossings (make-zero-crossings sense-data 0))

; ２番めの値にsense-dataの１つ遅れた値を入れている
(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))

;----------------------------------------------
; 3.75: ノイズの平滑化
;----------------------------------------------
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value )
                 2)))
    (stream-cons
      (sign-change-detector avpt last-value)
      (make-zero-crossings
        (stream-cdr input-stream ) avpt))))

; 平滑化の平均がおかしいので
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value) 2)))
    (stream-cons
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings
        (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

;-------------------------------------------------
;3.76: 平滑化演算をモジュール化する
;-------------------------------------------------
(define (make-zero-crossings smooth last-smooth)
    (stream-cons
      (sign-change-detector (stream-car smooth) last-smooth)
      (make-zero-crossings
        (stream-cdr smooth) (stream-car smooth))))

(define (smooth s1 s2) (scale-stream (add-streams s1 s2) 0.5))

(define smooth-integers (smooth integers (stream-cons 0 integers)))

(define zero-crossings (make-zero-crossings (smooth sense-data (stream-cons 0 sense-data)) 0))
