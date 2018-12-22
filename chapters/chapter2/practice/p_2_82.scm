; 引数が３つ以上の場合を考える

; step1
; 全ての引数をtype1の方に変換する

; step2
; step1が上手く行かない場合、全ての引数をtype2に変換する

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contens args))
          (let ((type1 (car type-tags))   ; １つめの引数の型
                (type2 (cadr type-tags))  ; ２つめの引数の型
                (a1 (car args))
                (a2 (cadr args)))
            (if (= type1 type2)
              ; 型が同じ場合
              (apply-generic op a1 a2)

              ; 型が異なる場合
              (let ((t1->t2_all (unify-type args type1 type2))
                    (t2->t1_all (unify-type args type2 type1))
                (cond (t1->t2_all
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1_all
                        (apply-generic op a1 (t2->t1 a1)))
                      (error "No method for these types" (list op type-tags))))

            (error "No method for these types" (list op type-tags))))))))

; argsの全ての方をt2に変換する
; 変換できなければ、#f
; このメソッドがflaseを返すのは以下の場合が考えられる

; 1: t1->t2 が定義されていない
; 2: t1, t2 以外の型がある場合
(define (unify-type args t1 t2)
