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

;; Driver Loop
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


