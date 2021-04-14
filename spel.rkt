
(load "positie-adt.rkt")
(load "score-adt.rkt")
(load "raket-adt.rkt")
(load "alienschip-adt.rkt")
(load "alienvloot-adt.rkt")
(load "teken-adt.rkt")
(load "spel-adt.rkt")
(load "constanten.rkt")
(load "level-adt.rkt")
(load "kogel-adt.rkt")
(load "kogels-adt.rkt")

; instantie maken van spel-adt om het spel op te starten
(define spel (maak-spel))
((spel 'start))  