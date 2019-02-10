(define (last-pair? x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 一度、チェックしたリストを覚えておきたかったので、
; internal変数を用意した。
(define (check-circle l)
  (let ((internal '()))
    (define (iter l)
      (cond
        ((not (pair? l)) #f)
        ((if (memq (car l) internal)
           #t
           (begin
             (set! internal (cons (car l) internal))
             (iter (cdr l)))))))
    (iter l)))

(define c-list (make-cycle (list 'a 'b 'c)))
(define s-list (list 'a 'b 'c))

;(check-circle c-list)
;#t

;(check-circle s-list)
;#f

;-----------------------------------------------------------
; 3.19
; 定数量に空間しか使わないアルゴリズムによって3.18をやり直す
;-----------------------------------------------------------

;問題の意味を読み砕くとメモリを使わない方法でやるべし。
;ということらしい。

;結城さんの解答
;xとyの追っかけっこ。
;xが先行。xが2歩進むとき、yは1歩進む。つまり差は1歩ずつ開いていく。
;ループに入ったら、先行していたはずのxがyに1歩ずつ近づいていく。yが一周する前には必ずxが追いつく。

(define (cyclic? x)
  (define (loop? x y)
    (cond ((not (pair? x)) #f)
          ((eq? x y) #t)
          ((not (pair? (cdr x))) #f)
          (else
            (loop? (cddr x) (cdr y)))))
  (cond ((not (pair? x)) #f)
        (else
          (loop? (cdr x) x))))
