;;--------------------------------------------------------
;; 別の章からの共通要素
;;--------------------------------------------------------
(define (tagged-list? x tag)
  (and (list? x) (eq? (car x) tag)))

;;--------------------------------------------------------
;;5.2.1: machine-model
;;--------------------------------------------------------
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
   (for-each ; 新しいマシンの中にレジスタを割り当てる
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
   ((machine 'install-operations) ops)
   ((machine 'install-instruction-sequence) ;マシンの命令に変換
    (assemble controller-text machine))
   machine))

;;レジスタ
(define (make-register name)
  (let ((contents '*unassigned*)
        (stack (make-stack)))
   (define (dispatch message)
     (cond ((eq? message 'get) contents)
           ((eq? message 'set)
            (lambda (value) (set! contents value)))
           ((eq? message 'push) (stack 'push))
           ((eq? message 'pop) (stack 'pop))
           ((eq? message 'initialize) (stack 'initialize))
           (else
             (error " Unknown request : REGISTER " message))))
   dispatch))
(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

;;スタック
(define (make-stack)
  (let ((s '()))
   (define (push x) (set! s (cons x s)))
   (define (pop)
     (if (null? s)
         (error "Empty stack : POP")
         (let ((top (car s)))
          (set! s (cdr s))
          top)))
   (define (initialize)
     (set! s '())
     'done)
   (define (dispatch message)
     (cond ((eq? message 'push) push)
           ((eq? message 'pop) (pop))
           ((eq? message 'initialize) (initialize))
           (else (error " Unknown request : STACK " message))))
   dispatch))
(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;;基本マシン
(define (make-new-machine)
  (let ((pc (make-register 'pc))     ;Program Counter
        (flag (make-register 'flag)) ;シミュレート対象のマシンの分岐を制御するために使う
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply define register: " name)
            (begin
              (let ((reg (make-register name)))
                (set! register-table
                      (cons (list name reg)
                            register-table))
                (reg 'initialize))))
        'register-allocated)
      (define (lookup-register name)
        (let (( val (assoc name register-table)))
         (if val
             (cadr val)
             (error " Unknown register :" name ))))
      (define (execute)
        (let ((insts (get-contents pc)))
         (if (null? insts)
             'done
             (begin
               ((instruction-execution-proc (car insts)))    ; 直列に命令を実行している
               (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error " Unknown request : MACHINE "
                           message))))
      dispatch)))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;--------------------------------------------------------
;;5.2.2: アセンブラ
;;--------------------------------------------------------
(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

;extract-labels は、text の要素を順に走査し、insts と labels を集積(cons)していく
(define (extract-labels text receive)
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

;命令リストは初期状態では命令のテキストしか持っていませんが、
;それに対応する実行手続きを追加
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops ( machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine pc flag stack ops)))
      insts)))

(define (make-instruction text) (cons text '()))

(define (instruction-text inst) (car inst))

(define (instruction-execution-proc inst) (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
   (if val
       (cdr val)
       (error " Undefined label : ASSEMBLE "
              label-name))))

;;--------------------------------------------------------
;;5.2.3: 命令の実行手続きの生成: 機械語 => scheme
;;--------------------------------------------------------
(define (make-execution-procedure
          inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
          (error " Unknown instruction type: ASSEMBLE "
                 inst))))

;assign
; (assign ⟨register-name⟩ (reg ⟨register-name⟩))
(define (make-assign inst machine labels operations pc)
  ;register-name
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    ;別のregisterから値を取り出す
    (let ((value-proc
            (if (operation-exp? value-exp)
                (make-operation-exp
                  value-exp machine labels operations)
                (make-primitive-exp
                  (car value-exp) machine labels))))
      (lambda ()
        ; assign に対する実行手続き
        (set-contents! target (value-proc)) ; registerに値を入れる
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc) ;pcを進める:次の命令をpcにいれる
  (set-contents! pc (cdr (get-contents pc))))

;test
;ex)
;(test (op =) (reg b) (const 0))
;(branch (label gcd-done))
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
   (if (operation-exp? condition)
       (let ((condition-proc
               (make-operation-exp
                 condition machine labels operations)))
         (lambda ()
           (set-contents! flag (condition-proc)) ; 結果がflagレジスタへ記憶される
           (advance-pc pc)))
       (error "Bad TEST instruction : ASSEMBLE " inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;branch
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
   (if (label-exp? dest)
       (let ((insts
               (lookup-label
                 labels
                 (label-exp-label dest))))
         (lambda ()
           (if (get-contents flag)
               (set-contents! pc insts)
               (advance-pc pc))))
       (error "Bad BRANCH instruction : ASSEMBLE " inst ))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;goto
;命令は branch に似ていますが、目的地がラベルまたはレジスタに規定されるという点と
;チェックするべき条件がないというところが異なる
;pc は常に新しい目的地に設定される
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
   (cond ((label-exp? dest)     ;ラベルに規定
          (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
            (lambda () (set-contents! pc insts))))
         ((register-exp? dest)  ;レジスタに規定
          (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
            (lambda ()
              (set-contents! pc (get-contents reg)))))
         (else (error "Bad GOTO instruction : ASSEMBLE " inst)))))

(define (goto-dest goto-instruction) (cadr goto-instruction))

;その他の命令
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
   (lambda ()
     (push reg (get-contents reg))
     (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop reg))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
   (if (operation-exp? action)
       (let ((action-proc
               (make-operation-exp
                 action machine labels operations)))
         (lambda () (action-proc) (advance-pc pc)))
       (error "Bad PERFORM instruction : ASSEMBLE " inst))))

(define (perform-action inst) (cdr inst))

;部分式の実行手続き
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c ( constant-exp-value exp)))
          (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                        labels
                        (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
          (lambda () (get-contents r))))
        (else (error " Unknown expression type: ASSEMBLE " exp ))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                          operations))
        (aprocs
          (map (lambda (e)
                 (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op )))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
   (if val
       (cadr val)
       (error " Unknown operation : ASSEMBLE "
              symbol))))
