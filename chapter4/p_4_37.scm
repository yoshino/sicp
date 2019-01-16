; 4.35
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high )))
    (let ((j (an-integer-between i high )))
      (let ((k (an-integer-between j high )))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k )))))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))

; 4.37:Ben Bitdiddle
(define (a-pythagorean-triple-between low high)
         (let ((i (an-integer-between low high))
               (hsq (* high high )))
           (let ((j (an-integer-between i high)))
             (let ((ksq (+ (* i i) (* j j))))
               (require (>= hsq ksq ))
               (let ((k (sqrt ksq )))
                 ( require ( integer? k))
                 (list i j k ))))))

; Ben Bitdiddleは正しい
; 例えば有限のambを考える
: i: 10 , j: 10, k: 10

; 4.35の場合は試行回数は10*10*10=1000

; 4.37はiとjの組み合わせをチェックすることで100以下になるので、
; 100以下 * 10になる

; http://sioramen.sub.jp/blog/2008/02/sicp-431.html
; が非常にイメージしやすい
