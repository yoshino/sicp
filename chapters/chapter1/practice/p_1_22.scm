(define (smallest-divisor n)
   (find-divisor n 2))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
   (= (remainder b a) 0))

(define (square x)
   (* x x))

(define (prime? n)
   (= n (smallest-divisor n)))

; gaucheにはtime数が定義されているのでそれを使う
; https://practical-scheme.net/gauche/man/gauche-refj/Shi-Jian-noJi-Ce-.html
(define (timed-prime-test n)
  (time (prime? n)))

(define (search-three-primes n count)
  (cond ((= count 3) "finish")
        ((prime? n)
           (print n)
           (search-three-primes (+ n 1) (+ count 1)))
        (else (search-three-primes (+ n 1) count))))



; 結果
; (time (search-three-primes n count))
; nを10倍ずつしていたものの比率は、
; √10=3.162
; に近似している。

; timeはシステムコールをgoshから直接呼び出すことを前提としているので、
; vim経由で呼び出すとすごく時間がかかってしまう（適切に時間を計測できない)

;irb(main):007:0> 0.283 / 0.117
;=> 2.4188034188034186
;irb(main):008:0> 0.996 / 0.283
;=> 3.519434628975265
;irb(main):009:0> 3.147 / 0.996
;=> 3.1596385542168672
;irb(main):010:0> 9.207 / 3.147
;=> 2.925643469971402

