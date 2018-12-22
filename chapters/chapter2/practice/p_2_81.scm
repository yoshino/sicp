; 強制型変換
(define (scheme-number->complex n)
  (make-complex-form-real-imag (contents n) 0))

; get-coercionがあることを仮定
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contens args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))   ; １つめの引数の型
                (type2 (cadr type-tags))  ; ２つめの引数の型
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))  ; もう片方への型変換があればそれを適用する
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a1)))
                    (error "No method for these types" (list op type-tags))))
            (error "No method for these types" (list op type-tags))))))))

; a
(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt xy))))

; 自身への型変換を追加した場合、上記の場合を考える
; 複素数と複素数の場合

(cond (t1->t2
        (apply-generic op (t1->t2 a1) a2))
      (t2->t1
        (apply-generic op a1 (t2->t1 a1)))
      (t1->t1
        (apply-generic op a1 (t1->t1 a1)))
      (t2->t2
        (apply-generic op a1 (t2->t2 a1)))
; 無限ループに入る

; b
; 同じ型への型変換を加える必要はないと思う。
; 条件分岐としてそのままの型で計算するのを加えればよいだけ

; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contens args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))   ; １つめの引数の型
                (type2 (cadr type-tags))  ; ２つめの引数の型
                (a1 (car args))
                (a2 (cadr args)))
            (if (= type1 type2)
              ; 型が同じ場合
              (apply-generic op a1 a2)

              ; 型が異なる場合
              (let ((t1->t2 (get-coercion type1 type2))  ; もう片方への型変換があればそれを適用する
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a1)))
                      (error "No method for these types" (list op type-tags))))

            (error "No method for these types" (list op type-tags))))))))
