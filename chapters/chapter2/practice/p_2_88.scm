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

;２つの多項式の引き算となる項リストを作成する
(define (sub-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond
              ((> (order t1) (order t2)) (adjoin-term t1 (sub-terms (rest-terms L1) L2)))
              ((< (order t1) (order t2)) (adjoin-term (* t2 (- 1)) (sub-terms L1 (rest-terms L2))))
              ((= (order t1) (order t2)) (adjoin-term  (make-term (- (coeff t1) (coeff t2)) (sub-terms (rest-terms L1 (rest-terms L2)))))))))))

; 上記のように、計算の都度、マイナスをかけてもOKだけれど、
; 感覚的には以下の様に符号を反転させる処理を付け加えたほうが良い

;; polynomial
(define (negative-term p)
  (mul-term (make-term 0 -1) p))
