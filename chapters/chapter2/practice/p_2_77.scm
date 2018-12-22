; polar
; magnitude だけに送れば良い
(define (magnitude z) (car z))

; rectangular
(define (magnitude z)
  (sqrt (+ (square (real-part z) (square (imag-part z))))))

; magnitudeからreal-part, imag-partが呼ばれている
