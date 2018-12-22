; 多項式の割り算
; sub-terms と mul-termsで割り算を計算できている
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
