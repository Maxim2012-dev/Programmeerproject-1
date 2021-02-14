;;            -----------
;; ----------  Kogel ADT  ----------
;;            -----------

(define (maak-kogel positie)
  (let ((afgeschoten? #f))

    ;; beweeg! : / -> /
    (define (beweeg!)
      (display (positie 'y)) (newline)
      ((positie 'beweeg!) 'omhoog))


    ;; dispatch functie
    (define (dispatch-kogel msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'beweeg!) beweeg!)
            (else (display "ongeldige boodschap - kogel"))))

    dispatch-kogel))