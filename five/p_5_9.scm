(load "./machine")

(define machine
  (make-machine
   '(a)  ; register
   (list (list '+ +) (list 'eq? eq?))   ; operations
   '(start
      (goto (label here))
     here
      (assign a (op eq?) (label start) (label start))
      (goto (label there))
     there)))

;(start machine)
;gosh> done
;(get-register-contents machine 'a)
;gosh> #t

;; labelを禁止
(define (make-operation-exp exp machine labels operations)
  (display '-----------------make-operation-exp------------------------------)
  (newline)
  (let ((op (lookup-prim (operation-exp-op exp)
                          operations))
        (aprocs
          (map (lambda (e)
                 (if (label-exp? e)
                     (error "Label can't be applied for operation!!!"))
                 (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs))))
