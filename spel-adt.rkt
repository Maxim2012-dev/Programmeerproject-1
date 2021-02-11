;;          ----------
;; --------  Spel ADT  ---------
;;          ----------

; Initieel bij het aanmaken van een spel maken we al twee instanties:
; eentje van het Level ADT en eentje van het Teken ADT.
(define (maak-spel)
  (let ((level-adt (maak-level spel-breedte spel-hoogte))
        (teken-adt (maak-teken-adt venster-breedte-px venster-hoogte-px)))


    ;; toets-functie : symbol, any -> /
    ; Deze functie zorgt ervoor dat de gebruikersinvoer correct wordt afgehandeld.
    ; De functie gaat een ingedrukte toets versturen naar het level-adt die dan op zijn beurt de nieuwe
    ; spelsituatie zal tekenen.
    (define (toets-functie status toets)

      (if (eq? status 'pressed)
          ((level-adt 'toets!) toets)))

    ;; spel-lus-functie : number -> /
    ; Om de zoveel tijd de spelsituatie automatisch bijwerken en alles tekenen.
    ; We geven de dispatch functie mee zodat de teken-functies kunnen aangeroepen worden.
    (define (spel-lus-functie tijdsverschil)
      ((level-adt 'update!) tijdsverschil)
      ((teken-adt 'teken-spel!) dispatch-spel))
    
    
    ; Om een effectief spel te kunnen starten voorzien we een start functie die de
    ; callbacks gaat zetten via het Teken ADT. Daarna kan het spel zijn lus beginnen
    ; en kan er 'geluisterd' worden naar gebruikersinvoer.
    (define (start)
      ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
      ((teken-adt 'set-toets-functie!) toets-functie))


    ;; Dispatch
    (define (dispatch-spel msg)
      (cond ((eq? msg 'start) start)
            ((eq? msg 'level) level-adt)))
      
    dispatch-spel))