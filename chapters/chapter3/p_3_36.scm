; make-connectorより抜粋
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval )
             (set! informant setter )
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))

(define ( for-each-except exception procedure list)
  (define (loop items)
    (cond (( null? items ) 'done)
          ((eq? (car items ) exception) (loop (cdr items)))
          (else ( procedure (car items))
                (loop (cdr items)))))
  (loop list))

; 3.36
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)


