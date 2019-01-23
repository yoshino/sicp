;a. Ben Bitdiddle に監督されるすべての人と、その住所
(and (supervisor ?ben-person (Bitdiddle Ben))
     (address ?ben-person ?where))

;b. Ben Bitdiddle よりも給料が少ないすべての人と、その給料と、Ben Bitdiddle の給料
(and (salary (Ben Bitdiddle) ?ben-salary)
     (salary ?person ?amount)
     (lisp-value > ?amount ?ben-salary))

;c. コンピュータ部門以外の人に監督されているすべての人と、その上司の名前と職務
(and (not (job? ?not-computer-persons (computer . ?type)))
     (supervisor ?not-computer-persons ?subordinates)
     (job? ?not-computer-persons ?job))
