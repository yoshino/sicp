(load "./machine_stack-register")

;; Check
(define test-machine
  (make-machine
    '(a b)
    '()
    '(start
       (assign a (const 1))
       (assign b (const 2))
       (save a)
       (save b)
       (restore a)
       (restore b)
       (goto (label done))
      done)))

(start test-machine)
(print "a is " (get-register-contents test-machine 'a))
(print "b is " (get-register-contents test-machine 'b))
