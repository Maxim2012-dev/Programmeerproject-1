;;            --------------
;; ----------  Power-Up ADT  ----------
;;            --------------


(define (maak-power-up)
  (let ((looptijd #f)
        (actief? #f))

    (define (beweeg!)
      ((positie 'beweeg!) 'omlaag))

    (define (dispatch-power-up msg)
      (cond ((eq? msg 'beweeg!) beweeg!)
            (else (display "wrong-message-power-up"))))

    dispatch-power-up))

    