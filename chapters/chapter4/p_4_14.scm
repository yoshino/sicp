;gosh> (map car '((a 1) (b 2) (c 3)))
;(a b c)

(define (analyze-map exp)
  (let ((proc (eval (cadr exp) (interaction-environment)))
        (args (eval (caddr exp) (interaction-environment))))
    (define (iter as)
      (if (null? as)
          '()
          (cons (proc (car as)) (iter (cdr as)))))
    (iter args)))

(define e '(map car '((a 1) (b 2) (c 3))))
(define map-res (analyze-map e))
;gosh> map-res
;(a b c)

; メタ循環器の基本手続きとしてmapは定義可能？
; 基本手続きの定義自体にmapを使っているので不可


