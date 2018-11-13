; last-pair
; (last-pair (list 23 72 149 34))
; => (34)

(define (last-pair items)
  (list-ref items (- (length items) 1)))

