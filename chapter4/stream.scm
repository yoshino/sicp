(use util.stream)
; https://practical-scheme.net/gauche/man/gauche-refj/sutorimuraiburari.html
; stream-null?
; stream-null
; stream-car
; stream-cdr
; stream-cons(cons-streamではないことに注意)
; stream-map
; stream-filter
; stream-for-each
; delay
; force

;;------------------------------------------------------------
;; Chapter3
;;------------------------------------------------------------
(define the-empty-stream stream-null)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

;streamの結合
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (div-streams s1 s2) (stream-map / s1 s2))
(define (integrate-series s) (div-streams s integers))

;streamを交互に結合
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

;delayとforce: sample
; (define d-ones-car (delay (stream-car ones)))
; d-ones-car : #<promise 0x56025e5ca980>
; (force d-ones-car)
; 1

;;------------------------------------------------------------
;; sample stream
;;------------------------------------------------------------
; ones: 1 1 1 1 1 .....
(define ones (stream-cons 1 ones))

; integers: 1 2 3 4 5 .....
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers
  (integers-starting-from 1))

; /integers: 1 1/2 1/3 1/4.....
(define /integers (integrate-series ones))

;;------------------------------------------------------------
;; Chapter4
;;------------------------------------------------------------
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
        (stream-car stream)
        (delay (flatten-stream (stream-cdr stream))))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (stream-cons
        (stream-car s1)
        (stream-append-delayed
          (stream-cdr s1)
          delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (stream-cons
        (stream-car s1)
        (interleave-delayed
          (force delayed-s2)
          (delay (stream-cdr s1))))))

(define (singleton-stream x)
  (stream-cons x the-empty-stream))

;テーブル内のストリームを検索
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
   (if s s the-empty-stream)))
