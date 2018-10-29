(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (square-check (expmod base (/ exp 2) m) m))
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

(define (square-check x m)
  (if (and (not (or (= x 1) (= x (- m 1))))
           (= (remainder (* x x) m) 1))
      0
      (remainder (* x x) m)))

; 素数である場合、１の自明な平方根に関して。
; 例えば563である場合、1と(563-1)の２つである。
;gosh> (expmod 1 2 563)
;1
;gosh> (expmod 562 2 563)
;1

; しかし、カーマイケル数のような数は、
; １の自明な素数以外にも１をとる。
;gosh> (expmod 67 2 561)
;1

; 上記のようにsquare-checkをいれて書き直すと、
; カーマイケル数も騙せなくなる。
; gosh> (fermat-test 13 13)
; #t
; gosh> (fermat-test 561 561)
; #f
; gosh> (fermat-test 1105 1105)
; #f

