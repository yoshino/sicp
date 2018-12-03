; right-split
(define (right-split painter n)
  (if (= n 0)
    (painter)
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    (painter)
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

; ここから抽象化
(define (split painter n direction split-direction)
  (if (= n 0)
    (painter)
    (let ((smaller (split painter (- n 1) direction split-direction)))
      (direction painter (split-direction smaller smaller)))))
  (
