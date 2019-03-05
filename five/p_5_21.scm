(load "./machine")

;;a): 再帰的
;;レジスタマシン
;;pairを使っているところが今までと違うところ
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;(define count-leaves-machine-a
;  (make-machine
;    '(tree val) ; treeはlist構造, valは答えを想定
;    (list (list '+ +) (list 'null? null?)  (list 'not not) (list 'pair pair) (list 'car car) (list 'cdr cdr))
;    '(controller
;       (assign continue (label leave-done))
;      leave-loop
;        ;; ((null? tree) 0)
;        (test (op null?) (reg tree) (const 0))
;        (branch (label zero-case))
;        ;; ((not (pair? tree)) 1)
;        (test (op null?) (reg tree) (const 1))
;        (assign val (op +) (reg val) (const 1))
;        ;; 再帰
;        ;; (count-leaves (car tree))
;        ;; 以下の方法だとtreeを記憶できない(すべての枝を走破できない)
;        (assign tree (op car) (reg tree))
;        (goto (label leave-loop))
;        ;; (count-leaves (cdr tree))
;        (assign tree (op cdr) (reg tree))
;        (goto (label leave-loop))
;      zero-case
;        (goto (reg continue))
;      leave-done)))

;; 以下REF
(define count-leaves-machine-a
  (make-machine
    '(continue tree val val-tmp tmp)
    (list (list '+ +) (list 'pair? pair?) (list 'null? null?) (list 'not not) (list 'car car) (list 'cdr cdr))
    '(start
       (assign continue (label count-leaves-done))
       (assign val (const 0))
    count-leaves-loop
       ;;((null? tree) 0)
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       ;; ((not (pair? tree)) 1)
       ;; ２段階に分けることがポイント
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       ;; (count-leaves (car tree)) を実行するように設定
       (save continue)
       (assign continue (label count-leaves-with-car))
       (save tree) ;; tree を退避
       (assign tree (op car) (reg tree)) ;; tree を (car tree) に変える
       (goto (label count-leaves-loop))  ;; 再帰呼び出しを実行
    null
       (assign val (const 0))
       (goto (reg continue))
    not-pair
       (assign val (const 1))
       (goto (reg continue))
    count-leaves-with-car
       (restore tree)
       (restore continue)
       ;; (count-leaves (cdr tree)) を実行するように設定
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label count-leaves-with-cdr))
       (save val) ;; (count-leaves (car tree)) を退避
       (goto (label count-leaves-loop))
    count-leaves-with-cdr
       (assign val-tmp (reg val))
       (restore val)
       (restore continue)
       (assign val (op +) (reg val) (reg val-tmp))
       (goto (reg continue))
    count-leaves-done)))

;gosh> (list x x)
;(((1 2) 3 4) ((1 2) 3 4))

;gosh>  (load "./p_5_21")
;gosh> (define x (cons (list 1 2) (list 3 4)))
;gosh> (set-register-contents! count-leaves-machine-a 'tree (list x x))
;done
;gosh> (start count-leaves-machine-a)
;done
;gosh> (get-register-contents count-leaves-machine-a 'val)
;8


  ' start
     (assign continue (label count-leaves-done))
     (assign val (const 0))

continue = count-leaves-done
val = 0

    count-leaves-loop
       ;;((null? tree) 0)
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       ;; ((not (pair? tree)) 1)
       ;; ２段階に分けることがポイント
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       ;; (count-leaves (car tree)) を実行するように設定
       (save continue)
       (assign continue (label count-leaves-with-car))
       (save tree) ;; tree を退避
       (assign tree (op car) (reg tree)) ;; tree を (car tree) に変える
       (goto (label count-leaves-loop))  ;; 再帰呼び出しを実行

CONTINUE: count-leaves-done
TREE: (((1 2) 3 4) ((1 2) 3 4))

val = 0
continue = count-leaves-with-car
tree = ((1 2) 3 4)

    count-leaves-loop
       ;;((null? tree) 0)
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       ;; ((not (pair? tree)) 1)
       ;; ２段階に分けることがポイント
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       ;; (count-leaves (car tree)) を実行するように設定
       (save continue)
       (assign continue (label count-leaves-with-car))
       (save tree) ;; tree を退避
       (assign tree (op car) (reg tree)) ;; tree を (car tree) に変える
       (goto (label count-leaves-loop))  ;; 再帰呼び出しを実行

CONTINUE: count-leaves-done count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4)

val = 0
continue = count-leaves-with-car
tree = (1 2)

    count-leaves-loop
       ;;((null? tree) 0)
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       ;; ((not (pair? tree)) 1)
       ;; ２段階に分けることがポイント
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       ;; (count-leaves (car tree)) を実行するように設定
       (save continue)
       (assign continue (label count-leaves-with-car))
       (save tree) ;; tree を退避
       (assign tree (op car) (reg tree)) ;; tree を (car tree) に変える
       (goto (label count-leaves-loop))  ;; 再帰呼び出しを実行

CONTINUE: count-leaves-done count-leaves-with-car count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4) (1 2)

val = 0
continue = count-leaves-with-car
tree = 1

    count-leaves-loop
       ;;((null? tree) 0)
       (test (op null?) (reg tree)) ;; tree が null
       (branch (label null))
       ;; ((not (pair? tree)) 1)
       ;; ２段階に分けることがポイント
       (assign tmp (op pair?) (reg tree))
       (test (op not) (reg tmp)) ;; tree がペアでない
       (branch (label not-pair))
       ;; (count-leaves (car tree)) を実行するように設定
       (save continue)
       (assign continue (label count-leaves-with-car))
       (save tree) ;; tree を退避
       (assign tree (op car) (reg tree)) ;; tree を (car tree) に変える
       (goto (label count-leaves-loop))  ;; 再帰呼び出しを実行

CONTINUE: count-leaves-done count-leaves-with-car count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4) (1 2)

val = 0
continue = count-leaves-with-car
tree = 1

    not-pair
       (assign val (const 1))
       (goto (reg continue))

CONTINUE: count-leaves-done count-leaves-with-car count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4) (1 2)

val = 1
continue = count-leaves-with-car
tree = 1

    count-leaves-with-car
       (restore tree)
       (restore continue)
       ;; (count-leaves (cdr tree)) を実行するように設定
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label count-leaves-with-cdr))
       (save val) ;; (count-leaves (car tree)) を退避
       (goto (label count-leaves-loop))

CONTINUE: count-leaves-done count-leaves-with-car count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4)
VAL: 1

val = 1
continue = count-leaves-with-cdr
tree = 2

    not-pair
       (assign val (const 1))
       (goto (reg continue))

CONTINUE: count-leaves-done count-leaves-with-car count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4)
VAL: 1

val = 1
continue = count-leaves-with-cdr
tree = 2

    count-leaves-with-cdr
       (assign val-tmp (reg val))
       (restore val)
       (restore continue)
       (assign val (op +) (reg val) (reg val-tmp))
       (goto (reg continue))

CONTINUE: count-leaves-done count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4)) ((1 2) 3 4)
VAL:

val-tmp = 1
val = 2
continue = count-leaves-with-car
tree = 2

    count-leaves-with-car
       (restore tree)
       (restore continue)
       ;; (count-leaves (cdr tree)) を実行するように設定
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label count-leaves-with-cdr))
       (save val) ;; (count-leaves (car tree)) を退避
       (goto (label count-leaves-loop))

CONTINUE: count-leaves-done count-leaves-with-car
TREE: (((1 2) 3 4) ((1 2) 3 4))
VAL: 2

val-tmp = 1
val = 2
continue = count-leaves-with-cdr
tree =  (3 4)


