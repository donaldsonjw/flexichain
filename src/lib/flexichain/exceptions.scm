(module flexichain.exceptions
   (export 
      (class &flexi-initialization-error::&error)
      (class &flexi-incompatible-type-error::&error)
      (class &flexi-cursor-error::&error)
      (class &flexi-navigation-error::&error)
      (raise-flexi-initialization-error #!key proc (msg "initialization error") cause)
      (raise-flexi-incompatible-type-error #!key proc element chain (msg "incompatible type"))
      (raise-flexi-cursor-error #!key proc cursor (msg "cursor error"))
      (raise-flexi-navigation-error #!key proc element (msg "navigation error"))))

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

;; Cursor error - cursor operation failed
(define (raise-flexi-cursor-error #!key proc cursor (msg "cursor error"))
   (raise (instantiate::&flexi-cursor-error 
             (proc proc)
             (msg msg)
             (obj cursor))))

;; Navigation error - element navigation failed
(define (raise-flexi-navigation-error #!key proc element (msg "navigation error"))
   (raise (instantiate::&flexi-navigation-error 
             (proc proc)
             (msg msg)
             (obj element))))
