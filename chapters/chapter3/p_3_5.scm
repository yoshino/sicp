; 数学的に乱数っぽいのを算出している。
; 実際は全くランダムではない
; moduloは余りを求めているだけ
(define (rand-update x)
  (modulo (+ (* 214013 x) 253011) 32767))

(define random-init 100)
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

;gosh> (rand)
;28091
;gosh> (rand)
;3034

; ランダムに選んだ二つの整数が共通因子を持たない、
; つまり最大公約数が 1 であるという確率が 6 / パイ**2 であるという事実
; をモンテカルロシュミレーションで試す

; 試行を増やせば予想した確率に漸近していくはず
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1)) ;randを手続きの中にカプセル化している

;gosh> (cesaro-test)
;#t
;gosh> (cesaro-test)
;#t
;gosh> (cesaro-test)
;#t
;gosh> (cesaro-test)
;#f

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials)) ; 結果を表示
          ((experiment)              ; ここにテストしたい関数をわたす: cesaro-test
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

; 試行を増やすとパイに漸近していく
;gosh> (monte-carlo 50 cesaro-test)
;31/50
;gosh> (monte-carlo 1000 cesaro-test)
;599/1000
;gosh> (estimate-pi 50)
;3.1108550841912765
;gosh> (estimate-pi 1000)
;3.164916190172819

; randの代わりにrand-updateを使ってみる
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1) ; モジュール性が失われている
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

;gosh> (estimate-pi 50)
;3.1108550841912765
;gosh> (estimate-pi 1000)
;3.1596457182557427

;-------------------------------------------------
; 3.5:  長方形に内包される円を利用したパイの推定方法
;-------------------------------------------------
(use srfi-27)

; モンテカルロは定義したものをそのまま使う
; モジュール性がポイントなので
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials)) ; 結果を表示
          ((experiment)              ; ここにテストしたい関数をわたす: cesaro-test
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (+ (- high low))))
    (+ low (random-integer range))))

;gosh> (random-in-range 2 8)
;7
;gosh> (random-in-range 2 8)
;2
;gosh> (random-in-range 2 8)
;4
;gosh> (random-in-range 2 8)
;6

(define (estimate-integral p x1 x2 y1 y2 trial)
  (define (rectangle-area x1 x2 y1 y2)
    (* (- x2 x1) (- y2 y1)))
  (* (monte-carlo trial (p x1 x2 y1 y2)) (rectangle-area x1 x2 y1 y2)))

; 領域内の点 (x, y) に対しては真となり、領域外の点に対しては偽となるような述語 P (x, y)
(define (p x1 x2 y1 y2)
  (define p1 (random-in-range x1 x2))
  (define p2 (random-in-range y1 y2))
  (define r1 (/ (+ x1 x2) 2.0))
  (define r2 (/ (+ y1 y2) 2.0))
  (define r  (- r1 x1))
  (define d (+ (square (- p1 r1)) (square (- p2 r2))))
  (or (< d (expt r 2)) (= d (expt r 2))))

;gosh> (p 2 4 8 10)
;#t
;gosh> (p 2 4 8 10)
;#f
;gosh> (p 2 4 8 10)
;#t
;gosh> (p 2 4 8 10)
;#t

; (rectangle-area x1 x2 y1 y2) を渡すと#t or #f を渡すことになってしまうので修正
(define (estimate-integral p x1 x2 y1 y2 trial)
  (define rectangle-area
    (* (- x2 x1) (- y2 y1) 1.0))
  (define (circle-in-rectangle?) (p x1 x2 y1 y2))
  (* (monte-carlo trial circle-in-rectangle?) rectangle-area))

;gosh> (estimate-integral p 2 8 4 10 10000)
;27.0288
