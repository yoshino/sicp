;;4.74: Alyssa P. Hacker は、negate, lisp-value, find-assertions の中で簡単なほうの
;stream-flatmap を使うことを提案した。彼女は、これらの場合にフレームのストリームにマップ
;される手続きが作るのは常に空ストリームか単一要素のストリームであるため、
;これらのストリームを組み合わせるのに互い違いに挟み込んでいく必要はないと気がついていた。

;a. Alyssa のプログラムに欠けている式を埋めよ。
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (null? s))) stream)))

;-------------------------------------------------------------
;複雑な(これまでの)stream-flatmap
;-------------------------------------------------------------
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
