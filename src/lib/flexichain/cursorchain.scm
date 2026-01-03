(module flexichain.cursorchain
   (import flexichain.flexichain
           flexichain.flexicursor
           flexichain.gapbuffer)
   (export (class <cursorchain>::<gapbuffer>
               (cursors::pair-nil (default '())))
           (make-cursorchain #!key (initial-capacity 0) (fill #\_)
              (min-capacity 5) (expand-factor 1.5))
           (make-cursorchain-left-cursor chain::<cursorchain> #!key (position::long 0))
           (make-cursorchain-right-cursor chain::<cursorchain> #!key (position::long 0))))

;; Construct a new cursorchain 
(define (make-cursorchain #!key (initial-capacity 0) (fill #\_)
           (min-capacity 5) (expand-factor 1.5))
   (let* ((capacity (calculate-space expand-factor min-capacity initial-capacity))
          (buffer (make-vector capacity fill)))
      (instantiate::<cursorchain> (fill-element fill)
                                  (expand-factor expand-factor)
                                  (min-capacity min-capacity)
                                  (buffer buffer)
                                  (gap-start 2)
                                  (gap-end 0)
                                  (data-start 1))))

(define-method (move-elements chain::<cursorchain> to::vector from::vector
                  start1::long start2::long end2::long)
   (set! (-> chain cursors) (adjust-cursors! (-> chain cursors) start2 (- end2 1) (- start1 start2))))

;; Create cursors with weak pointer management
(define (make-cursorchain-left-cursor chain::<cursorchain> #!key (position::long 0))
   (let ((cursor (make-left-sticky-flexicursor chain :position position)))
      ;; Add cursor to cursorchain's weak pointer list
      (set! (-> chain cursors) (cons (make-weakptr cursor) (-> chain cursors)))
      cursor))

(define (make-cursorchain-right-cursor chain::<cursorchain> #!key (position::long 0))
   (let ((cursor (make-right-sticky-flexicursor chain :position position)))
      ;; Add cursor to cursorchain's weak pointer list
      (set! (-> chain cursors) (cons (make-weakptr cursor) (-> chain cursors)))
      cursor))


