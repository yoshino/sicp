(load "./machine-debug")

;;方針
;; executeの実行前に対称のlabelとlabelからのカウンタを合致するかを確認。
;; 合致するなら、そこでdisplayとかexecute以外のコマンドを実行すれば止まる。
;; それまでの計算結果は全てマシンに束縛されている。
;; 計算を進めたければ再びexecuteを実行すればよい

;;再帰階乗マシン: デバッグ用
(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
    gcd-done)))

;gosh> (set-breakpoint gcd-machine 'test-b 4)
;set-breakpoint-done
;gosh> (set-register-contents! gcd-machine 'a 206)
;done
;gosh> (set-register-contents! gcd-machine 'b 40)
;done
;gosh> (start gcd-machine)
;DEBUG#<undef>
;gosh> (get-register-contents gcd-machine 'a)
;206
;gosh> (proceed-machine gcd-machine)
;done
;gosh> (get-register-contents gcd-machine 'a)
;2
