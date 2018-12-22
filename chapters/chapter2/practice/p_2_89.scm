; 項リストは⾼次から低次の順に並べられた項のリストとして表現されると想定した場合

; 項リストに新しい項を追加する
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))

; 空の項のリストを返すコンストラクタ
(define (the-empty-termlist) '())

; 与えられた項リストが空かどうか？
(define (empty-termlist? term-list) (null? term-list))

; 項リストから最大次数の項を取り出す
(define (first-term term-list) (car term-list))

; 最大次数の高以外の全ての項を返す
(define (rest-terms term-list) (cdr term-list))

; 与えられた次数と係数から項を構築するコンストラクタ
(define (make-term order coeff) (list order coeff))

; 項の次数
(define (order term) (car term))

; 項の係数
(define (coeff term) (cadr term))

;----------------------------------------------------------------
; 疎な多項式の場合
;----------------------------------------------------------------
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

;----------------------------------------------------------------
; 密な多項式の場合
;----------------------------------------------------------------
; x^4 + 3*x^3 + 3*x + 9
; ( 1 3 0 3 9)

; 多項式のコンストラクタ
(define (make-term) term-list)
; ex)  (make-term '(1 3 0 3 9))

; この方針であらわすと個別の項を考えるのが難しい
; 例えば係数が同じ場合、その項を区別するのは全体からの位置関係でしか不可能

; 項リストに新しい項を追加する
(define (adjoin-term term term-list) (cons term term-list))
; ex) (add-terms 4 '(1 2 3)) => (4 1 2 3)]

; 同じ
(define (empty-termlist? term-list) (null? term-list))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (cond
            ((> (length L1) (length L2)) (adjoin-term (first-term L1) (add-terms (rest-terms L1) L2)))
            ((< (length L1) (length L2)) (adjoin-term (first-term L2) (add-terms L1 (rest-terms L2))))
            ((= (length L1) (length L2)) (adjoin-term (map + L1 L2)))))))

; 多項式の和に関しては、其の多項式で定義したものより、こちらのほうが効率よく計算できる.

; (1 2 3 4) (5 6 7)
; 多項式の乗算を行うのはこの定義だと難しいと思う。
; なので、和に関しては上記の定義。
; 乗算に関しては、項：(係数、次数) のようなペアで定義されたものを利用する。

; 上記のような答えはやはり正しくない

; 一番よろしくないのは、インターフェースが異なること（例えば、次数を求めるorderが定義されていない)
; インターフェースが定義できれば、ジェネリックな演算のロジックは既存のものを利用できる。
; それがジェネリックな強さ。

;orderは以下のように定義することができる
(define (order term) (length (rest-terms term)))

; 乗算は以下のように定義できる
(define (mul-terms L1 L2)
  (cond ((empty-termlist? L1) (the-empty-termlist))
        ((empty-termlist? L2) (the-empty-termlist))
        (else
         (add-terms
          (mul-term-by-all-terms
           (make-term (first-term L1)
                      (iota (length (rest-terms L1) 0 0)))
           L2)
          (mul-terms (rest-terms L1) L2)))))
