;(lambda ⟨vars⟩
;  (define u ⟨e1⟩)
;  (define v ⟨e2⟩)
;  ⟨e3⟩)

; CASE1
;(lambda ⟨vars⟩
; (let ((u '*unassigned*)
;       (v '*unassigned* ))
;    (set! u ⟨e1⟩)
;    (set! v ⟨e2⟩)
;    ⟨e3⟩))

; CASE2
;(lambda ⟨vars⟩
;  (let ((u '*unassigned* ) (v '*unassigned* ))
;    (let ((a ⟨e1⟩) (b ⟨e2⟩))
;      (set! u a)
;      (set! v b))
;    ⟨e3⟩))

;----------------------------------------------------
; 変形する対象
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; CASE1
; delayされるのでdy問題はOK
(lambda ⟨f y0 dt⟩
 (let ((f '*unassigned*)
       (y0 '*unassigned* )
       (dt '*unassigned* ))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    ⟨e3⟩))

; let=lambda=環境を作製する

; letはlambdaの変形でありevalの評価をうける
; (apply (eval (append (cons 'lambda (list keys)) (list proc)) (interaction-environment)) vals))

(lambda ⟨f y0 dt⟩
  (let ((f '*unassigned* ) (y0 '*unassigned* ) (dt '*unassigned* ))
    (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y))) ; ここでdelay
      (set! y  a)
      (set! dy b)) ; ここでundefined error!
    ⟨e3⟩))
