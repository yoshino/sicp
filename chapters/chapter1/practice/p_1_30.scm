; 再帰
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; 線形再帰に変換する
;(define (sum term a next b)
;  (define (iter a result)
;    (if (??)
;        (??)
;        (iter (??) (??))))
;  (iter (??) (??)))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b) 
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1_29.scmで定義した時と答えが同じになるか？
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc n) (+ n 1))
  (define (y k)
    (f (+ a (* k h)))) 
  (define (term k)
    (* (cond ((odd? k) 4)
         ((or (= k 0) (= k n)) 1)
         ((even? k) 2))
   (y k)))
  (/ (* h (sum term 0 inc n)) 3))

(define (cube n) (* n n n))
        


