; この式について考える
(define (f x)
  (define b (+ a x))
  (define a 5)
  (+ a b))

;gaucheのデフォルトの評価
;gosh> (f 20)
;30

;aとbが同時に評価されるなら25が正しいはず
(define (f x)
  (define b (+ a x))
  (define a 5)
  (+ a b))

; 本文中の以下のような方法は
; 上から順に評価していくのでbを評価する時点でaのundefinedエラーが生じる
;(lambda ⟨vars⟩
;  (let ((u '*unassigned* )
;        (v '*unassigned* ))
;    (set! u ⟨e1⟩)
;    (set! v ⟨e2⟩)
;    ⟨e3⟩))

; *unassigned*が式の評価に現れたらとりあえずスキップして評価順序を変更する
; そしたら既存の枠組みをそのまま使える
;(define (f x)
;  (define a 5)
;  (define b (+ a x))
;  (+ a b))

