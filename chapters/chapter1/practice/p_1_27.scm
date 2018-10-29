(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n count)
  (cond 
    ((= count 1) #t)
    (else 
      (if (= (expmod (- count 1) n n) (- count 1)) 
        (fermat-test n (- count 1))
        #f))))

; カーマイケル数である、561, 1105を試した
; gosh> (fermat-test 561 561)
; #t
; gosh> (fermat-test 1105 1105)
; #t
