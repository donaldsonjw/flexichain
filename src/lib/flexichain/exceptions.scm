(module flexichain.exceptions
   (export 
      (class &flexi-initialization-error::&error)
      (class &flexi-incompatible-type-error::&error)
      (raise-flexi-initialization-error #!key proc (msg "initialization error") cause)
      (raise-flexi-incompatible-type-error #!key proc element chain (msg "incompatible type"))))

;; Initialization error - problems during flexichain construction
(define (raise-flexi-initialization-error #!key proc (msg "initialization error") cause)
   (raise (instantiate::&flexi-initialization-error 
             (proc proc)
             (msg (if cause (string-append msg ": " cause) msg))
             (obj cause))))

;; Type compatibility error - element doesn't match expected type
(define (raise-flexi-incompatible-type-error #!key proc element chain (msg "incompatible type"))
   (raise (instantiate::&flexi-incompatible-type-error 
             (proc proc)
             (msg (string-append msg ": " (if element (format "~a" element) "unknown")))
             (obj element))))
