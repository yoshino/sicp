;----------------------------
; 2.83
;----------------------------
; 整数 -> 有理数 -> 実数 -> 複素数
; 複素数は型のタワーの頂上なのでraiseを持たない

; integer
(define (riaes n)
  (chage_to_fraction (contents n) 0))
(get 'integer raise)

; fraction
(define (riaes n)
  (chage_to_real (contents n) 0))
(get 'fraction raise)

; real
(define (riaes n)
  (chage_to_complex (contents n) 0))
(get 'real raise)

;----------------------------
; 2.84
;----------------------------
; raiseが同じ型をもつまでraiseするにはどうすれば良いか？
; 考え方として、どちらがタワーの高い位置にあるか？を知る方法を考える

; 何回raiseできるか?を知れれば良い。
(define (count-raise n count)
  ((let typed_n (get (tyoe-tag n) raise))
   (if typed_n
     (count-raise typed_n (+ count 1))
     count)))

; count-raiseを使って、n1とn2を同じ型に変換する
(define (unit-type n1 n2)
  ((let
     (n1_count (count-raise n1))
     (n2_count (count_raise n2)))
   ((cond
      ((= (n1_count n2_count) (list n1 n2)))
      ((> (n1_count n2_count) (unit-type n1 (get n2 raise))))
      ((< (n1_count n2_count) (unit-type (get n1 raise) n2 raise)))))))


; 別解としてtowerの構造を基地のものとして考えて良いなら以下のようにして、
; どちらが高いか？を把握できる
(define (higher-type x y)
  (let ((tower '(complex real rational scheme-number))) ;; 型の塔
       (define (iter twr)
         (if (null? twr)
             #f
             (cond ((eq? x (car twr)) x)
                   ((eq? y (car twr)) y)
                   (else (iter (cdr twr))))))
       (iter tower)))



;----------------------------
; 2.85
;----------------------------
; タワー型を降りて単純化するfunction:dropを実装せよ
; アイデアとしては
; A:複素数 => project(射影) => B:実数 => raose => C:複素数
; とした時に、AとCが等しければ、A=>Cへはdropできる

; drop
; 整数 -> 有理数 -> 実数 -> 複素数
; 逆方向にくだっていく
; 整数 <- 有理数 <- 実数 <- 複素数

; dropを各型に対して定義する
; fraction
(define (drop n)
  (drop_to_integer (contents n)))
(get 'fraction drop)

; real
(define (drop n)
  (drop_to_fraction (contents n))
(get 'real drop)

; complex
(define (drop n)
  (drop_to_rael (contents n))
(get 'complex drop)

; dor

; 上記のアイデアを元にdrop可能化をprojectを使って判断する
(define (can_drop? n)
  (= (contents (project n)) (contens n)))

(define (project n)
  ((let drop_n (get (type-tag n) drop)
     (get (type-tag n) raise))))

; dropできるだけdropして単純化する
(define (to_bottom_type n)
  (if (can_drop? n)
    (to_bottom_type (get (type-tag n) drop))
    n))

; dropを使って答えを単純化する
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
                        (to_bottom_type (apply-generic op (t1->t2 a1) a2)))
                      (t2->t1
                        (to_bottom_type (apply-generic op a1 (t2->t1 a1))))
                      (error "No method for these types" (list op type-tags))))
