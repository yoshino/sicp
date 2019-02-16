(load "./machine")

;a
(define expt-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label expt-done))
     expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label return))
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
     after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
     return
      (assign val (const 1))
      (goto (reg continue))
   expt-done)))

;以下のようにして実行する
;(set-register-contents! expt-machine 'b 2)
;gosh> done
;(set-register-contents! expt-machine 'n 8)
;gosh> done
;(start expt-machine)
;gosh> done
;(get-register-contents expt-machine 'val)
;gosh> 256

;b
(define expt-machine-r
  (make-machine
   '(product counter n continue)
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label expt-done))
     (assign product (const 1))
     (assign counter (reg n))
     expt-loop
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign product (op *) (reg product) (reg n))
      (assign n (op -) (reg n) (const 1))
      (assign counter (op -) (reg counter) (const 1))
      (goto (label expt-loop))
   expt-done)))

;(set-register-contents! expt-machine-r 'n 8)
;gosh> done
;(start expt-machine-r)
;gosh> done
;(get-register-contents expt-machine-r 'product)
;gosh> 40320

;----------------------------------------------
;1: make-machineでマシンに必要な要素を作成する
;----------------------------------------------
;構成要素
;---------------------------------
;pc:    program counter   : register ; registerの名前空間に束縛されたcontentsという変数にsetする
;flag:  flag              : register
;stack: stack             : stack    ; 先入後出 (FILO): push,pop,initialize
;the-instruction-sequence : 空配列

;マシンに束縛された変数
;----------------------------------
; the-ops        : stackを登録する
; register-table : pcとflagを登録する

;マシン変数へのセット
;----------------------------------
;install-operations
;the-ops(stack)に(list (list '= =) (list '* *) (list '- -))を代入

;install-instruction-sequence
;the-instruction-sequenceにcontroller-textをlist(insts, labels)にconsしていく

;-----------------
;insts ; assign,test,branch,save,goto,restoreは"make-execution-procedure"で定義されている
;------------------
;(
 ;((assign continue (label expt-done)))
 ;((test (op =) (reg n) (const 0)))
 ;((branch (label base-case)))
 ;((save continue))
 ;((save n))
 ;((assign n (op -) (reg n) (const 1)))
 ;((assign continue (label after-expt)))
 ;((goto (label expt-loop))) ((restore n))
 ;((restore continue))
 ;((assign val (op *) (reg b) (reg val)))
 ;((goto (reg continue)))
 ;((assign val (const 1)))
 ;((goto (reg continue)))
;)

;------------------
;labels; expt-loop, after-expt, base-case, base-caseが関連付けされたlst
;------------------
;(
 ;(expt-loop
 ;  ((test (op =) (reg n) (const 0)))
 ;  ((branch (label base-case)))
 ;  ((save continue))
 ;  ((save n))
 ;  ((assign n (op -) (reg n) (const 1)))
 ;  ((assign continue (label after-expt)))
 ;  ((goto (label expt-loop)))
 ;  ((restore n))
 ;  ((restore continue))
 ;  ((assign val (op *) (reg b) (reg val)))
 ;  ((goto (reg continue)))
 ;  ((assign val (const 1)))
 ;  ((goto (reg continue))))

 ;(after-expt
 ;  ((restore n))
 ;  ((restore continue))
 ;  ((assign val (op *) (reg b) (reg val)))
 ;  ((goto (reg continue)))
 ;  ((assign val (const 1)))
 ;  ((goto (reg continue))))

 ;(base-case
 ;  ((assign val (const 1)))
 ;  ((goto (reg continue))))

 ;(expt-done)
;)


;命令
;---------------------------------
;start                         ; PCにthe-instruction-sequenceを代入し実行(PCが空になったら終了
;install-instruction-sequence
;allocate-register
;get-register
;install-operations
;stack
;operations
