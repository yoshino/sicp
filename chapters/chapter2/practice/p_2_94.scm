(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (remainder-terms a b))))

; remainder-termsを定義する

; 2_91より多項式の割り算は以下のように定義されている
; ここから余りを出したい
(define (div-terms L1 L2) ;L1:非除数 L2:除数
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1)) ; 係数の比較
        (list (the-empty-termlist) L1)
        (let ((new-c (div (coeff t1) (coeff t2))) ;係数
              (new-o (- (order t1) (order t2))))  ;次数
          (let ((rest-of-result
                  (add-terms (make-term (list new-c new-o))
                        ((let (new-L1 (sub-terms L1 (mul-terms (make-term (list new-c new-o)) L2))))
                         (cond
                           ((> (order t2) new-o) new-L1) ; 除数の次数が被除数の次数を上回った場合
                           ((= new-L1 0) 0)              ; あまりが0の場合
                           (else (div-terms new-L1 L2)))))))
            ; 完全な計算結果
            rest-of-result))))))

; div-termsの結果のうち除数の最高次数より小さい次数のものが余りである。

; こんな感じで多項式を想定いているので
( define p1 ( make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
( define p2 ( make-polynomial 'x '((3 1) (1 -1))))
( greatest-common-divisor p1 p2)

(define (member-if proc ls)
  (cond
   ((null? ls) #f)
   ((proc (car ls)) ls)
   (else (member-if proc (cdr ls)))))

(define (remainder-terms L1 L2)
  (let ((result-terms (div-terms L1 L2))
        (fast-order (order L2)))          ;除数の最高次数
    (member-if (lambda(x) (< fast-order (order x))) remainder-terms)))

; div-termsの結果のうち除数の最高次数より小さい次数のものが余りである。
; これは誤りで、div-termsの二番目の値が余りらし、、、、
(define (remainder-terms L1 L2) (cadr (div-terms L1 L2)))

; ジェネリック関数
(define (greatest-common-divisor p1 p2)
  (if (and (= (symbol p1) '(polynominal) (= (symbol p2)) '(polynominal)))
    (gcd-terms p1 p2)
    (gcd p1 p2)))

; 式の中で分岐を作るのではなくて、
(put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
(put 'greatest-common-divisor '(integer integer) gcd)

; 汎用手続きとして登録する
(define (greatest-common-divisor a b) (apply-generic 'greatest-common-divisor a b))


