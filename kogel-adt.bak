;;            -----------
;; ----------  Kogel ADT  ----------
;;            -----------

(define (maak-kogel positie)
  (let ((afgeschoten? #f))

    ;; beweeg! : / -> /
    (define (beweeg!)
      ((positie 'beweeg!) 'omhoog))


    (define (vuur-af!)
      (set! afgeschoten? #t))


    ;; dispatch functie
    (define (dispatch-kogel msg)
      (cond ((eq? msg 'positie) positie)
            (else (display "ongeldige boodschap"))))

    dispatch-kogel))