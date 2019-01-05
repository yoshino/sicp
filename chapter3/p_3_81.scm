(use util.stream)

; 代入を使用しないでも、ストリームを利用すればモジュール性を確保できる

; 文学的に言えば相対主義(代入)に陥らないでも、歴史主義（過去からの連なり）を利用すれば、
; 自己を認識できる

(define random-init 12345)

(define (rand-update x)
  (modulo (+ (* 214013 x) 253011) 32767))

(define random-numbers
  (stream-cons
    random-init
    (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (stream-cons
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f ( stream-cdr ( stream-cdr s )))))

(define cesaro-stream
  (map-successive-pairs
    (lambda (r1 r2) (= (gcd r1 r2) 1))
    random-numbers))

; 任意のexperiment-streamをモジュール化できている
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
    (lambda (p) (sqrt (/ 6 p)))
    (monte-carlo cesaro-stream 1 1))) ; passed failedを0にすると０割れないエラー出るのでとりあえず1

;gosh> (stream-ref pi 100)
;3.3829638550307397

;-----------------------------------------------------------
; 乱数生成器を ストリームを使ってかけ
;-----------------------------------------------------------
; 代入を利用した乱数生成器
(use srfi-27)

(define (rand internal-value range)
  (define generate
    (begin (set! internal-value (+ internal-value (random-integer 10)))
           internal-value))
  (define reset
    (begin (set! internal-value 100)
           internal-value))
  (define (dispatch m)
    (cond ((eq? m 'generate) generate)
          ((eq? m 'reset) reset)))
  dispatch)

; generateとresetは先に評価されてしまい、
; 以降、generateを呼び出しても最初に評価した同じ値が返ってきてしまう
(define rand100 (rand 100 10))
;(rand100 'generate)

(define rand
  (let ((internal-value 0))
    (define (generate)
      (begin (set! internal-value (+ internal-value (random-integer 10)))
             internal-value))
    (define (reset new-value)
      (begin (set! internal-value new-value)
             internal-value))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)))
    dispatch))

(define (rand-update x)
  (remainder (+ x 1812433253) 4294967296))

; ここから
(define (add-streams s1 s2) (stream-map + s1 s2))

(define (rand initial-value range)
  (define rand-stream
    (stream-cons
      (rand-update range)
      rand-stream))

  (define generate
    (stream-cons
      initial-value
      (add-streams generate rand-stream)))

  (define reset
    (stream-cons
      (stream-car generate)
      initial-value))

  (define (dispatch m)
    (cond ((eq? m 'generate) generate)
          ((eq? m 'reset) reset)))
  dispatch)

(define rand-seed (rand 10 100))
(define rs (rand-seed 'generate))

;gosh> (stream-ref rs 0)
;10
;gosh> (stream-ref rs 1)
;1812433363
;gosh> (stream-ref rs 2)
;3624866716
