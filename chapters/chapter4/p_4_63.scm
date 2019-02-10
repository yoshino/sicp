;練習問題 4.63: 次のデータベース (創世記第 4 章参照) は、Ada の
;子孫から家系をたどり、 Cain を経て Adam に至るまで遡っている。
;(son Adam Cain)
;(son Cain Enoch )
;(son Enoch Irad)
;(son Irad Mehujael )
;(son Mehujael Methushael )
;(son Methushael Lamech )
;(wife Lamech Ada)
;(son Ada Jabal )
;(son Ada Jubal )
;“もし S が f の息子であり、かつ、f が G の息子ならば、S は G
;の孫である”、“もし W が M の妻であり、かつ、S が W の息子な
;らば、S は M の息子である” といった (聖書時代には現代よりも
;当てはまりやすかったと思われる) 規則を定式化せよ。これらによ
;って、クエリシステムが Cain の孫、Lamech の息子、Methushael
;の孫を求めることができるようになる (より複雑な関係を推論す
;る規則については、練習問題 4.69参照)。

;“もし S が f の息子であり、かつ、f が G の息子ならば、S は Gの孫である”
(rule (?s grand-child-to ?g)
      (and (g? son ?f)
           (?f son ?s)))

;“もし W が M の妻であり、かつ、S が W の息子ならば、S は M の息子である”
(rule (?m born ?s)      ;?motherは?sを産んだ？
      (and (?w wife ?m)
           (?w son ?s)))

