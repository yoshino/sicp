;人 1 が人 2 と同じ職務を持っているか、人 1 の職務を持つ人には
;人 2 の職務もできるという場合で、人 1 と人 2 が同一人物でない場合、
;人 1 は人 2 を代替 (replace) できるとする規則を定義せよ。
;この規則を使って、以下の検索を行うクエリを示せ。

;a. Cy D. Fect を代替できるすべての人
(rule (replace ?person1 ?person2)
      (and (not (same ?person1 ?person2))
           (or
             (and (job ?person1 ?job)
                  (job ?person2 ?job))
             (can-do-job
               (job ?person1 ?job1)
               (job ?person2 ?job2)))))

;b. 自分より給料の高い誰かを代替できるすべての人と、その二つの給料
(rule (replace ?i ?you)
      (and (replace ?i ?you)
           (salary ?you ?your-salary)
           (lisp-value > ?your-salary (salary ?i ?my-salary))))

