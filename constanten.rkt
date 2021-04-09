(define cel-breedte-px 20)
(define cel-hoogte-px 20)

(define spel-breedte 31)
(define spel-hoogte 25)

(define venster-breedte-px (* cel-breedte-px spel-breedte))
(define venster-hoogte-px (* cel-hoogte-px spel-hoogte))

(define aantal-rijen-aliens 5)
(define aantal-aliens-per-rij 11)
(define aantal-levens-raket 3)

(define snelheid-vloot 1000)
(define snelheid-kogel 50)

(define raket-start-x 5)
(define raket-start-y (- spel-hoogte 4))

(define score-bord-y 5)
(define score-pos (- (/ venster-breedte-px 2) 230))
(define record-pos (+ (/ venster-breedte-px 2) 70))

(define score-label-tekst-x score-pos)
(define score-label-tekst-y score-bord-y)
(define punten-tekst-x (+ score-pos 100))
(define punten-tekst-y score-bord-y)

(define record-label-tekst-x record-pos)
(define record-label-tekst-y score-bord-y)
(define record-punten-tekst-x (+ record-pos 105))
(define record-punten-tekst-y score-bord-y)

(define y-buiten-speelveld 25)

(define delay-alienschot 3000)