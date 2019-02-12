; GCDのお手本
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

(controller
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (reg a))
  ; remainderをloopで表現
  ; (assign t (op rem) (reg a) (reg b))
  rem-loop
    (test (op <) (reg t) (reg b))
    (branch (label rem-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
  rem-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done)

; Newton法
(define (sqrt x)
  (define (good-enough? guess )
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; good-enough? と improve 演算が基本命令として使えると仮定
(controller
  (assign i-guess (const 1))
  (test-newton (op good-enough) (reg i-guess))
  (branch (label newton-done))
  (assign guess (reg i-guess))
  (assign i-guess (op improve) (reg guess))
  (go-to (lanel test-newton))
  newton-done)

; good-enough? と improve 演算を算術命令として展開する方法
(controller
  initialize
    (assign x (op read))
    (assign i-guess (const 1))
  test-newton
    (assign s-guess (op square) (reg i-guess))
    (assign ss-guess (op sub) (reg s-guess) (reg x))
    (assign abs-ss-guess (op abs) (reg ss-guess))
    (test (op <) (reg abs-ss-guess) 0.001)
    (branch (label newon-done))
  newton-improve
    (assign div-guess (op div) (reg guess) (reg x))
    (assign i-guess (op average) (reg div-guess) (reg guess))
  newton
    (assign guess (reg i-guess))
    (goto (label test-newton))
  newton-done
    (perform (op print) (reg i-guess)) ; 標準出力のようなもの？
    (goto (label sqrt-loop)))          ; driver-loop風に答えを表示し続ける
  )

