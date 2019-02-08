;Louis Reasoner は、 simple-query と disjoin の手続き (4.4.4.2 節)
;がなぜ以下のような定義ではなく、明示的な delay 演算を用いて
;実装されているのだろうかと不思議に思っている。
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append
        (find-assertions query-pattern frame)
        (apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
        (qeval (first-disjunct disjuncts)
               frame-stream)
        (disjoin (rest-disjuncts disjuncts)
                 frame-stream))))

;;---------------------------------------------------------------------------------
;; 単純クエリ
;;---------------------------------------------------------------------------------
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed                         ; AとBを組み合わせた大きなストリームを生成する
        (find-assertions query-pattern frame)        ; A:拡張フレームのストリームを生成
        (delay (apply-rules query-pattern frame))))  ; B:別の拡張フレームのストリームを生成
    frame-stream))

; Louis Reasonerの方法だとAのストリームの結果が反映されない。
; 正しい動作としては、Aのフレームの拡張が終わったら、その後に（delay!)
; Bのフレームの拡張を行いたい
; そうしないと、無限ループにはまる
