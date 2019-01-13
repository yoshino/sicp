; analyze-sequenceはbeginやlambdaなど複数のprocが絡む時に使われる手続き
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

; CASE1
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
         (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
         (if (null? rest-procs)
             first-proc
             (loop (sequentially first-proc (car rest-procs))
                   (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error " Empty sequence : ANALYZE "))
    (loop (car procs) (cdr procs))))

; 式が2つ
;((+ 1 2) (+ 3 4))
;(analyze-sequence ((+ 1 2) (+ 3 4)) env)
;(loop (+ 1 2) (+ 3 4))
;(define (sequentially (+ 1 2) (+ 3 4))
;=> (lambda (env) (+ 1 2) env) (+ 3 4) env)))

; CASE2
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
            ((car procs) env)
            (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error " Empty sequence : ANALYZE "))
    (lambda (env)
      (execute-sequence procs env))))

; 式が2つ
;((+ 1 2) (+ 3 4))
;(analyze-sequence '((+ 1 2) (+ 3 4)) env)
;(execute-sequence '((+ 1 2) (+ 3 4)) env)
;=> (lambda (env) (+ 1 1) ...
; 解析された値を返してしまう
