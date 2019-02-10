;----------------------------------------------------------------
;;4.73: なぜflatten-stream は明示的に delay を使っているのだろうか。
;----------------------------------------------------------------

;4.71と同じ理由

; 例えば以下の様に明示的なdelayをなくしてしまうと
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
        (stream-car stream)
        (flatten-stream (stream-cdr stream)))))

; stream-cdrがstream-carの評価後でないと無限ループにハマることがあるため
