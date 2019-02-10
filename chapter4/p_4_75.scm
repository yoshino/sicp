;; uniqueを実装せよ
; (unique (job ?x ( computer wizard)))

;; フィルタ(NOT)を参考にする
;; (put 'not 'qeval negate)
(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))

;;---------------------------------------------------------
;; unique
;;---------------------------------------------------------
(define (unique operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null? (stream-cdr (qeval (unique-query operands) (singleton-stream frame))))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))
