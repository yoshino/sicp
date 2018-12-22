(define (install-polynomial-package)
  ;; 内部表現
  (define (make-poly variable term-list) (cons variable term-list))

  (define (variable p) (car p))

  (define (term-list p) (cdr p))

  (define (same-variable?))

  (define (variable?))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  ;; システムと他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  'done)

; 空の項のリストを返すコンストラクタ
(define the-empty-termlist)

; 項リストに新しい項を追加する
(define adjoin-term)

; 与えられた項リストが空かどうか？
(define empty-termlist?)

; 項リストから最大次数の項を取り出す
(define first-term)

; 最大次数の高以外の全ての項を返す
(define rest-terms)

; 与えられた次数と係数から項を構築するコンストラクタ
(define make-term)

; 項の次数
(define order)

; 項の係数
(define coeff)

;２つの多項式の和となる項リストを作成する
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

;２つの多項式の乗算となる項リストを作成する
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t2 (first-term L)))
      (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2))) ; 一般的なmulを使用している
        (mul-term-by-all-terms t1 (rest-terms L))))))

; 項リストは⾼次から低次の順に並べられた項のリストとして表現されると想定します。

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

;------------------------------------------------
; 2.87
;------------------------------------------------
(put '=zero? '(scheme-number)
     (lambda (x) (= x 0)))

;; 有理数算術演算パッケージ equ? を追加
  (define (equ-zero? x)
    (= (denom x) 0))
  (put '=zero? '(rational)
       (lambda (x) (equ-zero?)))

;; 複素数算術演算パッケージに equ? を追加
  (define (equ-zero? z1 z2)
    (and (= (real-part z1) 0)
         (= (imag-part z1) 0))
  (put '=zero? '(complex)
       (lambda (z1) (equ-zero? z1)))

(define (=zero? x) (apply-generic '=zero? x))

















