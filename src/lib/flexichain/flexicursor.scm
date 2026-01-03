(module flexichain.flexicursor
   (import flexichain.cursor
           flexichain.flexichain
           flexichain.gapbuffer
           flexichain.exceptions)
   (static
      (class <flexicursor>::<cursor>
         chain::<flexichain>
         index::long)
      (class <left-sticky-flexicursor>::<flexicursor>)
      (class <right-sticky-flexicursor>::<flexicursor>))
   (export (make-left-sticky-flexicursor chain::<flexichain> #!key (position::long 0))
           (make-right-sticky-flexicursor chain::<flexichain> #!key (position::long 0))
           (adjust-cursors! cursors::pair-nil start::long end::long increment::long)))


(define (make-left-sticky-flexicursor chain::<flexichain> #!key (position::long 0))
   (let ((index (flexi-position-index chain (- position 1))))
      (instantiate::<left-sticky-flexicursor> (chain chain)
                                              (index index))))

(define (make-right-sticky-flexicursor chain::<flexichain> #!key (position::long 0))
   (let ((index (flexi-position-index chain position)))
      (instantiate::<right-sticky-flexicursor> (chain chain)
                                               (index index))))

;; Adjust cursor positions when elements are moved
(define (adjust-cursors! cursors::pair-nil start::long end::long increment::long)
   (let ((acc '()))
      (let loop ((cursors cursors))
         (if (null? cursors)
             acc
             (let ((cursor (car cursors)))
                (cond ((and (<= start (with-access::<flexicursor> cursor (index) index))
                           (<= (with-access::<flexicursor> cursor (index) index) end))
                       ;; Cursor is in the affected range, adjust its index
                       (with-access::<flexicursor> cursor (index)
                          (set! index (+ index increment)))
                       (let ((rest (cdr cursors)))
                          (set-cdr! cursors acc)
                          (set! acc cursors)
                          (loop rest)))
                      (else
                       ;; Cursor is not affected, keep it
                       (let ((rest (cdr cursors)))
                          (set-cdr! cursors acc)
                          (set! acc cursors)
                          (loop rest)))))))))

;; Cursor method implementations
(define-method (cursor-pos cursor::<left-sticky-flexicursor>)
   (+ (index-position (-> cursor chain) (-> cursor index)) 1))

(define-method (cursor-pos cursor::<right-sticky-flexicursor>)
   (index-position (-> cursor chain) (-> cursor index)))

(define-method (cursor-pos-set! cursor::<left-sticky-flexicursor> pos::long)
   (set! (-> cursor index) (flexi-position-index (-> cursor chain) (- pos 1))))

(define-method (cursor-pos-set! cursor::<right-sticky-flexicursor> pos::long)
   (set! (-> cursor index) (flexi-position-index (-> cursor chain) pos)))

(define-method (cursor-at-beginning? cursor::<flexicursor>)
   (= (cursor-pos cursor) 0))

(define-method (cursor-at-end? cursor::<flexicursor>)
   (= (cursor-pos cursor) (flexi-length (-> cursor chain))))

(define-method (cursor-move> cursor::<flexicursor> n::long)
   (let ((steps (if (= n 0) 1 n)))
      (cursor-pos-set! cursor (+ (cursor-pos cursor) steps))))

(define-method (cursor-move< cursor::<flexicursor> n::long)
   (let ((steps (if (= n 0) 1 n)))
      (cursor-pos-set! cursor (- (cursor-pos cursor) steps))))

(define-method (cursor-insert! cursor::<flexicursor> item::obj)
   (flexi-insert! (-> cursor chain) (cursor-pos cursor) item))

(define-method (cursor-delete<! cursor::<flexicursor> n::long)
   (let ((pos (cursor-pos cursor))
         (count (if (= n 0) 1 n)))
      (when (> pos 0)
         (flexi-delete-items! (-> cursor chain) (- pos count) count))))

(define-method (cursor-delete>! cursor::<flexicursor> n::long)
   (let ((pos (cursor-pos cursor))
         (count (if (= n 0) 1 n)))
      (when (< pos (flexi-length (-> cursor chain)))
         (flexi-delete-items! (-> cursor chain) pos count))))

(define-method (cursor-ref< cursor::<flexicursor>)
   (let ((pos (cursor-pos cursor)))
      (if (> pos 0)
          (flexi-ref (-> cursor chain) (- pos 1))
          (raise-flexi-cursor-error :proc "cursor-ref<" :cursor cursor :msg "cursor at beginning"))))

(define-method (cursor-set<! cursor::<flexicursor> item::obj)
   (let ((pos (cursor-pos cursor)))
      (if (> pos 0)
          (flexi-set! (-> cursor chain) (- pos 1) item)
          (raise-flexi-cursor-error :proc "cursor-set<!" :cursor cursor :msg "cursor at beginning"))))

(define-method (cursor-ref> cursor::<flexicursor>)
   (let ((pos (cursor-pos cursor)))
      (if (< pos (flexi-length (-> cursor chain)))
          (flexi-ref (-> cursor chain) pos)
          (raise-flexi-cursor-error :proc "cursor-ref>" :cursor cursor :msg "cursor at end"))))

(define-method (cursor-set>! cursor::<flexicursor> item::obj)
   (let ((pos (cursor-pos cursor)))
      (if (< pos (flexi-length (-> cursor chain)))
          (flexi-set! (-> cursor chain) pos item)
          (raise-flexi-cursor-error :proc "cursor-set>!" :cursor cursor :msg "cursor at end"))))

(define-method (cursor-clone cursor::<left-sticky-flexicursor>)
   (make-left-sticky-flexicursor (-> cursor chain) :position (cursor-pos cursor)))

(define-method (cursor-clone cursor::<right-sticky-flexicursor>)
   (make-right-sticky-flexicursor (-> cursor chain) :position (cursor-pos cursor)))


