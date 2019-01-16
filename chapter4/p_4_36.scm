;-----------------------------------------------------------
; 4.35 an-integer-between
;-----------------------------------------------------------
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high )))
    (let ((j (an-integer-between i high )))
      (let ((k (an-integer-between j high )))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k )))))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))

;-----------------------------------------------------
; 単純なピタゴラス
;-----------------------------------------------------
; an-integer-form: 連続する整数のamdを返す
(define (a-pythagorean-triple-between if1 if2 jf3)
  (let ((i (an-integer-form if1)))
    (let ((j (an-integer-form if2)))
      (let ((k (an-integer-form if3)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k )))))

; i j k はランダムに出てくる
; 例えば i > kなどは条件を満たさない

(define (a-pythagorean-triple-between low)
  (let ((i (an-integer-form low)))
    (let ((j (an-integer-form i)))
      (let ((k (an-integer-form j)))
        (if (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k )
            try-again)))))
