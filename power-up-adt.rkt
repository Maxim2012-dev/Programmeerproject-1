;;            --------------
;; ----------  Power-Up ADT  ----------
;;            --------------


(define (maak-power-up positie)
  (let ((looptijd 10000)
        (actief? #f)
        (type 'default))

    (let ((vec-size (vector-length power-up-types))
          (nieuw-type (vector-ref power-up-types (random-integer vec-size))))
      (set! type nieuw-type))
          

    ; Een power-up beweegt in elk geval omlaag richting de raket
    ; beweeg! : / -> /
    (define (beweeg!)
      ((positie 'beweeg!) 'omlaag))

    (define (dispatch-power-up msg)
      (cond ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'type) type)
            (else (display "wrong-message-power-up"))))

    dispatch-power-up))

    