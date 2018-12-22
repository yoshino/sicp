; 多項式が疎の場合
; 多項式が密の場合
; ２つのデータの表し方をデータ主導の方針を利用して表現する

; 疎
(define (install-loosly-terms)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
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

  ; インターフェース
  (define (tag x) (atttach-tag 'loosely-terms x))
  (put 'add-terms '(loosely-terms loosely-terms) add-terms)
  (put 'empty-termlist? '(loosely-terms) empty-termlist?)
  (put 'first-term '(loosely-terms) first-term)
  (put 'rest-terms '(loosely-terms) rest-terms)
  (put 'adjoin-term (lambda (t l) (tag (adjoin-term t l))))
  'done)

; 密
(define (install-tight-terms)
  ; 内部手続き
  (define (make-term) term-list)
  (define (adjoin-term term term-list) (cons term term-list))
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

  ; インターフェース
  (define (tag x) (atttach-tag 'tight-terms x))
  (put 'add-terms '(tight-terms tight-terms) add-terms)
  (put 'empty-termlist? '(tight-terms) empty-termlist?)
  (put 'first-term '(tight-terms) first-term)
  (put 'rest-terms '(tight-terms) rest-terms)
  (put 'adjoin-term (lambda (t l) (tag (adjoin-term t l))))
  'done)

; さらに密＝＞疎、疎＝＞密への型変換を定義して、apply-genericに組み込む必要がある。

; ここで考えたんだけど(誰かの解いている答えを見て）、

; 強制型変換をどこに組み込むか？という問は、どこをパブリックなインターフェースにするのか？
; という問とかなり似ている。

; つまり、強制型変換をapply-genericに組み込んで、add-termsをそこに登録すると、
; 型：tight-termsとloosely-termesがむき出しになる

; これはおそらく望んでいるものではなくて、
; 強制型変換、ここでいえば、疎＜＝＞密のものは、
; polynomialなる汎用的な型の中で呼び出されるべきもののきがする。

; こうすることで、パブリックからは多項式である型は見ることができるけど、
; 疎か密かの型をみることはなくなる。
