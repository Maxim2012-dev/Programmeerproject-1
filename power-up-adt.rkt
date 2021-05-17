;;            --------------
;; ----------  Power-Up ADT  ----------
;;            --------------


(define (maak-power-up positie)
  (let ((tijdsgebonden? #f)
        (looptijd #f)
        (actief? #f)
        (type 'default))

    (let* ((vec-size (vector-length power-up-types))
          (nieuw-type (vector-ref power-up-types (random-integer vec-size))))
      (set! type nieuw-type)
      (cond ((= type 3) (set! tijdsgebonden? #t)
                        (set! looptijd 8000))
            ((= type 4) (set! tijdsgebonden? #t)
                        (set! looptijd 3000))))
          

    ; Een power-up beweegt in elk geval omlaag richting de raket
    ; beweeg! : / -> /
    (define (beweeg!)
      ((positie 'beweeg!) 'omlaag))

    ; zet-actief! : / -> /
    (define (toggle-actief!)
      (if actief?
          (set! actief? #f)
          (set! actief? #t)))

    (define (dispatch-power-up msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'looptijd) looptijd)
            ((eq? msg 'tijdsgebonden?) tijdsgebonden?)
            ((eq? msg 'actief?) actief?)
            ((eq? msg 'toggle-actief!) toggle-actief!)
            ((eq? msg 'type) type)
            (else (display "wrong-message-power-up"))))

    dispatch-power-up))

    