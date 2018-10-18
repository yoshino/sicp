; パスカルの三角形
; row, colともに0から始まっている
(define (pascal row col)
  (cond ((< row col) #f)
        ((or (= 0 col) (= row col)) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

