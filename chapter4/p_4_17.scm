; 有効な環境はlambda(vars)の定義するものだけ
;(lambda ⟨vars⟩
;  (define u ⟨e1⟩)
;  (define v ⟨e2⟩)
;  ⟨e3⟩)

;以下のように変形されます。

; 有効な環境はlambda(vars)の定義する中に、
; letが定義する環境が有効

;(lambda ⟨vars⟩
;(let ((u '*unassigned* )
;      (v '*unassigned* ))
;  (set! u ⟨e1⟩)
;  (set! v ⟨e2⟩)
;  ⟨e3⟩))


; 同時スコープルール??
; wat-aroさん参照
; lambdaを評価すると新しくフレームが作られます．
; これを防ぐためにletでunassignmentを束縛するのではなくdefineで内部定義します．
; define-variable!はフレームに新たな変数を追加する手続きなので余計なフレームは作られません．

; だから余計なlambdaを入れなければ良いので
;(lambda <vars>
;  (define u '*unassigned)
;  (define v '*unassigned)
;  (set! u <e1>)
;  (set! v <e2>)
;  <e3>)


