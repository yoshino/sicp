(load "./stream.scm")
(load "./table.scm")

;;---------------------------------------------------------------------------------
;; 4.4.4.2: 評価器
;;---------------------------------------------------------------------------------
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; 単純クエリ
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed                         ; AとBを組み合わせた大きなストリームを生成する
        (find-assertions query-pattern frame)        ; A:拡張フレームのストリームを生成
        (delay (apply-rules query-pattern frame))))  ; B:別の拡張フレームのストリームを生成
    frame-stream))

;; 複合クエリ(AND)
;;(put 'and 'qeval conjoin)
;; クエリを適用したものを再帰的にconjoinに渡していく
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts) (qeval (first-conjunct conjuncts) frame-stream))))

;; 複合クエリ(OR)
;; (put 'or 'qeval disjoin)
;; クエリは別々に計算されinterleave-delayed 手続きによって結合される
(define (disjoin disjuncts frame-stream )
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed (qeval (first-disjunct disjuncts) frame-stream)
                          (delay (disjoin (rest-disjuncts disjuncts) frame-stream )))))

;; フィルタ(NOT)
;; (put 'not 'qeval negate )
(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))

;; フィルタ(lisp-value): whereみたいなもの
;; (put 'lisp-value 'qeval lisp-value )
(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate ; パラメータ変数を具体化
              call
              frame
              (lambda (v f)
                (error " Unknown pat var: LISP-VALUE " v))))
          (singleton-stream frame) ; 条件を満たす場合
          the-empty-stream))       ; 満たさない場合は除去
    frame-stream))

;; execute
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

;; always-true
;; (put 'always-true 'qeval always-true)
(define (always-true ignore frame-stream) frame-stream)

;;---------------------------------------------------------------------------------
;; 4.4.4.3: パターンマッチングによる表明の検索
;;---------------------------------------------------------------------------------
(define (find-assertions pattern frame)
  (stream-flatmap
    (lambda (datum) (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

;パターン、データオブジェクト (表明)、フレームをとる
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
          (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

;マッチャの基本的な考え方は、データに対して要素ごとにパターンをチェックし、
;パターン変数に対する束縛を集積していくというもの
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed ) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
           (cdr pat)
           (cdr dat)
           (pattern-match (car pat) (car dat) frame )))
        (else 'failed)))

;フレームにすでにある束縛と矛盾しない範囲で新しい束縛を追加してフレームを拡張する手続き
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
   (if binding
       (pattern-match (binding-value binding) dat frame)
       (extend var dat frame))))

;;---------------------------------------------------------------------------------
;; 4.4.4.4: 規則とユニフィケーション
;;---------------------------------------------------------------------------------
;パターンマッチングを一般化したもの—ユニフィケーション (unification) と呼ばれます
;ユニフィケーションを行うユニファイアは、二つのパターンを引数として取ります。
;パターンはどちらも定数や変数を含みます。
;そして、二つのパターンが等しいものとなるような変数への値の割り当てが可能かどうかを決定します。
;もし可能であれば、それらの束縛を含むフレームを返します。
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

;ユニークな変数名を作る際には、規則適用ごとにユニークな識別子 (例えば数字) を関連づけ、
;その識別子と元の変数名を組み合わせることにします。
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

;pattern-matchと似ているけど***の文だけ違う
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed ) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) ( pair? p2 ))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
            (unify-match (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                   var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) ; ある変数を、その変数を含むパターンに束縛は失敗させる
           'failed)
          (else (extend var val frame )))))

; ターン変数の値として提案された式がその変数に依存するかどうかテストする述語です
; tree-walkは再帰探索の意味
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;;---------------------------------------------------------------------------------
;; 4.4.4.5: データベースの保守
;;---------------------------------------------------------------------------------
;ここでのシステムでは、すべての表明をひとつの大きなストリームに格納するのに加えて、
;car が定数記号であるすべての表明を、その記号によって索引づけされたテーブルの中に別々の
;ストリームとして格納します。
;パターンにマッチするかもしれない表明を取り出すときは、まずパターンの car が定数記号かどうか
;チェックします。定数記号であれば、格納された表明の中で同じ car を持つものを返します。

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern) ;定数が合致するものからstreamを得る
  (get-stream (index-key-of pattern) 'assertion-stream))

(define THE-RULES the-empty-stream)
(define ( fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules ) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
    (get-stream ( index-key-of pattern ) 'rule-stream )
    (get-stream '? 'rule-stream ))) ;結論が変数で始まるすべての規則をテーブル内の独立したストリームに格納


(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
   (set! THE-ASSERTIONS ;データベース内のすべての表明のストリームに格納される
     (stream-cons assertion old-assertions))
   'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
   (set! THE-RULES (stream-cons rule old-rules))
   'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
       (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
         (put key
              'assertion-stream
              (stream-cons
                assertion
                current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
   (if (indexable? pattern)
       (let ((key (index-key-of pattern)))
        (let (( current-rule-stream
                (get-stream key 'rule-stream)))
          (put key
               'rule-stream
               (stream-cons rule
                            current-rule-stream )))))))

;先頭が変数または定数記号であればテーブルに格納されます。
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

;パターンが格納されるテーブルのキーは、?(パターンの先頭が変数である場合)またはパターン先頭の定数記号
(define (index-key-of pat)
  (let ((key (car pat)))
   (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))


;;---------------------------------------------------------------------------------
;; 4.4.4.7: クエリ手続き: セレクタ
;;---------------------------------------------------------------------------------
;;
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error " Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error " Unknown expression CONTENTS " exp)))

;; query-driver-loopによって使われるもの
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

;; and, or, not, lisp-valueのための構文定義
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) ( null? exps ))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

;; 規則の構文
(define (rule? statement)
  (tagged-list? statement 'rule ))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

;?symbol という形を持つ式のパターン変数を (? symbol) という内部形式に変形
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
   (if (string=? (substring chars 0 1) "?") ; 文字列の切り取り
       (list '?
             (string->symbol
               (substring chars 1 (string-length chars))))
       symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

;規則適用のユニーク識別子は数値で、規則適用のたびにインクリメントされる
(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

;query-driver-loop が答えを表示するためにクエリを具体化する際には、以下
;の手続きを使って未束縛のパターン変数をすべて表示用の形式に戻します。
(define (contract-question-mark variable)
  (string->symbol
    (string-append "?"
                   (if (number? (cadr variable))
                       (string-append (symbol->string (caddr variable))
                                      "-"
                                      (number->string (cadr variable)))
                       (symbol->string (cadr variable))))))

;;---------------------------------------------------------------------------------
;; 4.4.4.8: フレームと束縛
;;---------------------------------------------------------------------------------
(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame )
  (cons (make-binding variable value) frame))

;; Instance
;変数が具体化できない場合に取るべき行動は、instantiate手続きの引数として渡される
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;---------------------------------------------------------------------------------
;; クエリの節で定義されなかった手続き
;;---------------------------------------------------------------------------------
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;---------------------------------------------------------------------------------
;; Driver Loop
;;---------------------------------------------------------------------------------
(define input-prompt ";;; Query input :")
(define output-prompt ";;; Query results :")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display " Assertion added to data base.")
           (query-driver-loop))
          (else
            (newline)
            (display output-prompt)
            (display-stream
              (stream-map
                (lambda (frame)
                  (instantiate
                    q
                    frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q ( singleton-stream ' ()))))
            (query-driver-loop)))))

(query-driver-loop)
