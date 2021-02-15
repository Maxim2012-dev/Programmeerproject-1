;;            ------------
;; ----------  Matrix ADT  ----------
;;            ------------

(define (maak-matrix)
  (let ((vector (make-vector aantal-rijen-aliens)))

    ; Deze procedure zal de vector in de 'let' initialiseren met
    ; vectoren van gelijke grootte
    (define (vul-vector)
      (let loop
        ((huidige-idx 0))
        (if (< huidige-idx aantal-rijen-aliens)
            (begin
              (vector-set! vector huidige-idx (make-vector aantal-aliens-per-rij))
              (loop (+ huidige-idx 1))))))

    (vul-vector)
    vector))


    