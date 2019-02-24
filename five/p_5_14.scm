(load "./machine_monitor")

;;再帰階乗マシン
(define fact-machine
  (make-machine
    '(n continue val)                            ;; register
     (list (list '= =) (list '- -) (list '* *))   ;; operations
     '(controller
       (assign continue (label fact-done))
        ;set up final return address
        fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        ;; Set up for the recursive call by saving n and continue.
        ;; Set up continue so that the computation will continue
        ;; at after-fact when the subroutine returns.
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
        after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
        (goto (reg continue))
        ;return to caller
        base-case
        (assign val (const 1))
        ;base case: 1! = 1
        (goto (reg continue))
        ;return to caller
        fact-done)))

(set-register-contents! fact-machine 'n 6)
(start fact-machine)
(get-register-contents fact-machine 'val)
(monitor-stack fact-machine)

;n=1
;(total-pushes = 0 max-depth = 0)

;n=2
;(total-pushes = 2 max-depth = 2)

;n=3
;(total-pushes = 4 max-depth = 4)

;n=4
;(total-pushes = 6 max-depth = 6)

;n=5
;(total-pushes = 8 max-depth = 8)

;n=6
;(total-pushes = 10 max-depth = 10)

;n=k, total-pushes(k), max-depth(k)とすると、
;total-pushes(k) = max-depth(k) = k + (k-2)
