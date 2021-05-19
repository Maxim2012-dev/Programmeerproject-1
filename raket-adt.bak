;;            -----------
;; ----------  Raket ADT  ----------
;;            -----------


; constructor om raket aan te maken
; de raket begint altijd op een gegeven positie
(define (maak-raket positie)
  (let ((levens aantal-levens-raket)
        (schild? #f)
        (upgrade? #f))


  ; de beweeg! functie die de beweeg van positie gaat aanroepen
  ; om zo de raket naar links of rechts te laten bewegen afhankelijk van het richtingsargument
  (define (beweeg! richting)
    (if (not (rand-geraakt?))
        ((positie 'beweeg!) richting)
        (let ((x (positie 'x)))
                 ; linkerrand geraakt (dan mag je alleen naar rechts)
          (cond ((and (= x 0) (eq? richting 'rechts))
                 ((positie 'beweeg!) richting))
                 ; rechterrand geraakt (dan mag je alleen naar links)
                ((and (= x (- spel-breedte 1)) (eq? richting 'links))
                 ((positie 'beweeg!) richting))))))

  
  ; rand-geraakt? : / -> boolean
  (define (rand-geraakt?)
    ((positie 'rand-horizontaal?)))

  ; verminder-levens! : / -> /
  (define (verminder-levens!)
    (set! levens (- levens 1)))

  ; voeg-leven-toe! : / -> /
  (define (voeg-leven-toe!)
    (set! levens (+ levens 1)))

  ; toggle-schild! : / -> /
  (define (toggle-schild!)
    (if schild?
        (set! schild? #f)
        (set! schild? #t)))

  ; toggle-3-kogels! : / -> /
  (define (toggle-upgrade!)
    (if upgrade?
        (set! upgrade? #f)
        (set! upgrade? #t)))

  ; reset-levens! : /> /
  (define (reset-levens!)
    (set! levens aantal-levens-raket))

  ; de dispatch functie
  (define (dispatch-raket msg)
    (cond((eq? msg 'beweeg!) beweeg!)
         ((eq? msg 'positie) positie)
         ((eq? msg 'positie!) positie!)
         ((eq? msg 'schiet!) schiet!)
         ((eq? msg 'levens) levens)
         ((eq? msg 'schild?) schild?)
         ((eq? msg 'upgrade?) upgrade?)
         ((eq? msg 'verminder-levens!) verminder-levens!)
         ((eq? msg 'voeg-leven-toe!) voeg-leven-toe!)
         ((eq? msg 'toggle-schild!) toggle-schild!)
         ((eq? msg 'toggle-upgrade!) toggle-upgrade!)
         ((eq? msg 'reset-levens!) reset-levens!)
         (else (display "ongeldige boodschap - raket"))))
  dispatch-raket))

  

  

  
  