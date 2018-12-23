;; 変数が2つ以上ある場合について考える

; 変数が１つであり疎な多項式の場合和は以下のように定義できた
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond ((> (order t1) (order t2)))
                  (adjoin-term
                    (make-term (order t1)
                               (add coeff t1) (coeff t2))) ; 一般的なaddを使用している
                  (add-terms (rest-terms L1)
                             (rest-terms L2)))))))

;------------------------------------------------------
; 定義
;------------------------------------------------------

; 与えられた次数と係数から項を構築するコンストラクタ

; 5xy**2 について考える
; (((symbolA, 次数) (symbolB、次数))、係数) のように表現できる
(define (make-term var-list coeff) (list var-list coeff))

; 項の係数
(define (coeff term) (cadr term))

; xy**2の部分
(define (var-list (car term)))
(define (var symbol order) (list symbol order))

; 変数のシンボル
(define (symbol var) (car var))

; 変数の次数
(define (order var) (cdr var))

; 同じ変数の組み合わせを持っているか?
; ((x, 3) (y,2) (z,3)) == ((x, 3) (y,2) (p,3))

(define (same-var-list? vl1 vl2)
  (cond
    ((and (= (null? vl1) (null? vl2))) #t)
    ((or  (= (null? vl1) (null? vl2))) #f)
    ((if (include-var? (car vl1) vl2)
       (same-var-list? (cdr vl1) vl2)
       #f))))
(define (include-var? var var-list)
  (cond
    ((null? var-list) #f)
    ((if (and (= (symbol var) (symbol (car var-list))) (= (order var) (order (car var-list))))
       #t
       (include-var? var (cdr var-list))))))

; 項の追加
(define (adjoin-term term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))

; 空の項のリストを返すコンストラクタ
(define (the-empty-termlist) '())

; 与えられた項リストが空かどうか？
(define (empty-termlist? term-list) (null? term-list))

(define (add-terms L1 L2)
    ((cond
       ((empty-termlist? L1) L2)
       ((empty-termlist? L2) L1)
       (else (cons (add-term (car L1) L2)
                   (add-terms (cdr L1) L2))))))

(define (add-term term terms)
  ((cond
     ((null? terms) term)
     (if (= (same-var-list? (var-list term) (var-list (car terms))))
       (make-term (var-list term) (+ (coeff term) (coeff (car terms))))
       (add-term term (cdr terms)))

; 5xy**2 について考える
; (((symbolA, 次数) (symbolB、次数))、係数) のように表現した
; 確かにこのように定義すれば汎用的（データ構造に対する知識を必要としない)だけど、
; そのぶん、ロジックが難しくなる。

; 別解を見てみるとデータ構造を上手く利用している。

; 上手く利用しているというか、あらかじめどの変数が使われるのかを既知のものとしている。

;例
;(polynomial light '(変数1 変数2 ...)
;            '(((変数1の次数 変数2の次数 ...) 係数)
;              ((変数1の次数 変数2の次数 ...) 係数)
;              ...))

;上述の多項式の具体例の場合
(polynomial light '(x y)
            '(((6 3) 2)
              ((3 3) -4)
              ((3 2) 1)
              ((3 1) 7)
              ((3 -1) 2)
              ((2 0) 1)
              ((1 0) 2)
              ((0 0) 1)
              ((-1 0) 8)))
