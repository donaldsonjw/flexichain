(module flexichain.flexirank
   (import flexichain.flexichain
           flexichain.gapbuffer
           flexichain.exceptions)
   (export
      (abstract-class <element-rank-mixin>
         index::long
         chain::<flexichain>)
      (class <ranked-gapbuffer>::<gapbuffer>)
      (generic rank element::<element-rank-mixin>)
      (generic flexi-first? element::<element-rank-mixin>)
      (generic flexi-last? element::<element-rank-mixin>)
      (generic flexi-next element::<element-rank-mixin>)
      (generic flexi-prev element::<element-rank-mixin>)
      (make-ranked-gapbuffer #!key (initial-capacity 0) (fill #\_)
         (min-capacity 5) (expand-factor 1.5))))

;; Generic definitions
(define-generic (rank element::<element-rank-mixin>))
(define-generic (flexi-first? element::<element-rank-mixin>))
(define-generic (flexi-last? element::<element-rank-mixin>))
(define-generic (flexi-next element::<element-rank-mixin>))
(define-generic (flexi-prev element::<element-rank-mixin>))

;; Element methods
(define-method (rank element::<element-rank-mixin>)
   (index-position (-> element chain) (-> element index)))

(define-method (flexi-first? element::<element-rank-mixin>)
   (= (rank element) 0))

(define-method (flexi-last? element::<element-rank-mixin>)
   (= (rank element) (- (flexi-length (-> element chain)) 1)))

(define-method (flexi-next element::<element-rank-mixin>)
   (let ((pos (rank element)))
      (if (< pos (- (flexi-length (-> element chain)) 1))
          (flexi-ref (-> element chain) (+ pos 1))
          (raise-flexi-navigation-error :proc "flexi-next" :element element :msg "element is last"))))

(define-method (flexi-prev element::<element-rank-mixin>)
   (let ((pos (rank element)))
      (if (> pos 0)
          (flexi-ref (-> element chain) (- pos 1))
          (raise-flexi-navigation-error :proc "flexi-prev" :element element :msg "element is first"))))

;; Constructor
(define (make-ranked-gapbuffer #!key (initial-capacity 0) (fill #\_)
           (min-capacity 5) (expand-factor 1.5))
   (let* ((capacity (calculate-space expand-factor min-capacity initial-capacity))
          (buffer (make-vector capacity fill)))
      (instantiate::<ranked-gapbuffer> 
         (fill-element fill)
         (expand-factor expand-factor)
         (min-capacity min-capacity)
         (buffer buffer)
         (gap-start 2)
         (gap-end 0)
         (data-start 1))))

;; Override move-elements to update element positions
(define-method (move-elements chain::<ranked-gapbuffer> to::vector from::vector
                  start1::long start2::long end2::long)
   ;; Call parent method first
   (call-next-method)
   
   ;; Update element positions
   (let loop ((old start2) (new start1))
      (when (< old end2)
         (let ((element (vector-ref from old)))
            (when (isa? element <element-rank-mixin>)
               (with-access::<element-rank-mixin> element (index)
                  (set! index new))))
         (loop (+ old 1) (+ new 1)))))

;; Override insert to set element position
(define-method (flexi-insert! chain::<ranked-gapbuffer> position::long item::obj)
   (call-next-method)  ; Insert normally
   (when (isa? item <element-rank-mixin>)
      (with-access::<element-rank-mixin> item (index chain)
         (set! index (flexi-position-index chain position))
         (set! chain chain))))

;; Override set to update element position
(define-method (flexi-set! chain::<ranked-gapbuffer> position::long item::obj)
   (call-next-method)  ; Set normally
   (when (isa? item <element-rank-mixin>)
      (with-access::<element-rank-mixin> item (index chain)
         (set! index (flexi-position-index chain position))
         (set! chain chain))))
