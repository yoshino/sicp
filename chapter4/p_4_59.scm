;4.59: Ben Bitdiddle はある会議を何度も欠席してしまった。
;この会議を忘れる癖を何とかしないと首になると思って、Benは対策を取ることを決めた。
;彼は、以下のような表明として会社の週次ミーティングを Microshaft のデータベースにすべて追加した。

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))

;a. 金曜の朝、Ben はその日にあるすべての会議についてデータ
;ベースに問い合わせようと思った。彼はどのようなクエリを
;使うべきだろうか。

(meeting ?kind (Friday ?time))

;b. Alyssa P. Hacker は、このやり方はあまりよくないと思った。
;彼女は、自分の名前を指定して自分の会議について聞くこと
;ができたほうがずっと便利だろうと考えた。そこで、彼女は
;規則を設計することにした。その規則は、ある人の会議は全
;社 (whole-company) 会議とその人の部門会議をすべて含むと
;いうものだ。Alyssa の規則に本体を補え

(rule (meeting-time ?person ?day-and-time)
      (or (and (job ?person (?devision . ?type))
               (meeting ?devision ?day-and-time))
          (meeting whole-company ?day-and-time)))

;c. Alyssa は水曜の朝に職場に着き、その日にどんな会議がある
;か考えた。上記の規則を定義してあるとして、この検索を行
;うには、彼女はどのようなクエリを作るべきだろうか。

(meeting-time (c. Alyssa) (Wednesday ?time))

