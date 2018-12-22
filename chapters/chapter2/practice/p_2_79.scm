; 以下の様なタグ付けのジェネリックシステムがあるとして
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else "Bad tagged datum - CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

;---------------------------------------------------------------
; get put
;---------------------------------------------------------------
(define operation-table (make-hash-table))

; opとtypeが示すところにitemを入れる
(define (put op type item)
    (if (not (hash-table-exists? operation-table op))
        (hash-table-put! operation-table op (make-hash-table)))
    (let ((type-table (hash-table-get operation-table op)))
      (hash-table-put! type-table type item)))

; tableからopとtypeの項目を探しそこで見つかった項目を返す
; 見つからなければfalse
(define (get op type)
    (if (hash-table-exists? operation-table op)
        (let ((type-table (hash-table-get operation-table op)))
          (hash-table-get type-table type))
        (error "Not exists" op type)))

; テーブルにアクセスする
; 手続きがあればそれを適用する
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))
;---------------------------------------------------------------
; 複素数の定義
;---------------------------------------------------------------
(define (install-rectangular-package)
  ; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-form-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; システムと他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-real-ang 'rectangular
       (lambda (r a) (tag (make-from-real-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;-------------------------------------------------------------
; 通常の数値、有理数、複素数に対して動作するようにする
; 通常(ordinary)の算術演算パッケージに equ? を追加
;-------------------------------------------------------------

(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

(put 'add '(scheme-number scheme-number)
     (lambda (x y) (+ x y)))

;; 有理数算術演算パッケージ equ? を追加
  (define (equ-rat? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))

;; 複素数算術演算パッケージに equ? を追加
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ-complex? z1 z2)))

(define (equ? x y) (apply-generic 'equ? x y))
