(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))
;(set! count (+ count 1) (id 10))

;count
;=> 1

; w
; 10

;count
;=>2


;遅延評価系の解釈系を作っておくと良い
;gosh> (load "./driver-loop_delayed")
;#t
;gosh> (driver-loop)

;;; M-Eval input:
;(+ 1 3)

;;; M-Eval value:
;4

;;; M-Eval input:
;(define (try a b) (if (= a 0) 1 b))

;;; M-Eval value:
;ok

;;; M-Eval input:
;(try 0 (/ 1 0))

;;; M-Eval value:
;1
