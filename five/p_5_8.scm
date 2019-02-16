(load "./machine")

(define here-machine
  (make-machine
   '(a)  ; register
   '()   ; operations
   '(start
      (goto (label here))
     here
      (assign a (const 3))
      (goto (label there))
     here
      (assign a (const 4))
      (goto (label there))
     there)))

;(start here-machine)
;gosh> done
;(get-register-contents here-machine 'a)
;gosh> 3

;make-goto
;一番初めに出てくる(label here)
;pcに以下のように登録していくので3をassignしたあとthereにgotoする。
;(((assign a (const 3))) ((goto (label there))) ((assign a (const 4))) ((goto (label there))))


;;-------------------------------------------------------
;; 重複したラベルを禁じる
;;-------------------------------------------------------
(define (extract-labels text receive)
  (display '-------------extract-labels------------------)
  (newline)
  (if (null? text)
      (receive '() '())
      (extract-labels
        (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
           (if (assoc next-inst labels)  ;;重複したlabelはエラー
               (error "Duplicated labels" labels)
             (if (symbol? next-inst)
                 (receive insts
                          (cons (make-label-entry next-inst
                                                  insts)
                                labels))
                 (receive (cons (make-instruction next-inst)
                                insts)
                          labels))))))))

