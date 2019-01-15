;ref: https://wat-aro.hatenablog.com/entry/2016/01/10/204424

;gosh> (load "./eval_stream") 
;#t
;gosh> (driver-loop)
;

;;; M-Eval input:
;ones
;
;;; M-Eval value:
;(1 1 1 1 1 1 1 1 1 1 ...)
;
;;; M-Eval input:
;(cons 'a (cons 'b (cons 'c '())))
;
;;; M-Eval value:
;(a b c)


