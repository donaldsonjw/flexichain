;;; Flexichain protocol 
;;; Bigloo port
;;; Copyright (C) 2024 Joseph Donaldson (donaldsonjw@gmail.com)
;;;
;;; Original Common Lisp implementation 
;;; Flexichain -- https://github.com/robert-strandh/Flexichain
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(module flexichain.flexichain
   (export
      (class &flexi-position-error::&error
         chain::<flexichain>
         position::long)
      (abstract-class <flexichain>)
      (generic flexi-length chain::<flexichain>)
      (generic flexi-empty? chain::<flexichain>)
      (generic flexi-insert! chain::<flexichain> position::long item::obj)
      (generic flexi-insert-vector! chain::<flexichain> position::long vec::vector)
      (generic flexi-delete! chain::<flexichain> position::long)
      (generic flexi-delete-items! chain::<flexichain> position::long n::long)
      (generic flexi-ref::obj chain::<flexichain> position::long)
      (generic flexi-set! chain::<flexichain> position::long item::obj)
      (generic flexi-push-start! chain::<flexichain> item::obj)
      (generic flexi-push-end! chain::<flexichain> item::obj)
      (generic flexi-pop-start!::obj chain::<flexichain>)
      (generic flexi-pop-end!::obj chain::<flexichain>)
      (generic flexi-rotate! chain::<flexichain> #!optional (n 1))
      (flexi-position-error proc::bstring message::bstring chain::<flexichain> position::long)
      (flexichain? obj)
      (generic flexi-position-index chain::<flexichain> position::long)))

;; raise &flexi-position-error 
(define (flexi-position-error proc::bstring message::bstring chain::<flexichain> position::long)
   (raise (instantiate::&flexi-position-error
             (proc proc)
             (msg message)
             (obj chain)
             (chain chain)
             (position position))))


(define (flexichain? obj)
   (isa? obj <flexichain>))

;;;; flexichain protocol 
;;;; flexichain defines an editable sequence protocol 

;; Returns the number of elements in the flexichain 
(define-generic (flexi-length chain::<flexichain>))

;;Rreturns whether the flexichain is empty (i.e., 0 elements)
(define-generic (flexi-empty? chain::<flexichain>))

;; Inserts ITEM before the element at POSITION in chain.
;; If POSITION is out of range, a &flexi-position-error is thrown.
(define-generic (flexi-insert! chain::<flexichain> position::long item::obj))

;; Inserts the elements of VEC before the element at POSITION in chain.
;; If POSITION is out of range, a &flexi-position-error is thrown.
(define-generic (flexi-insert-vector! chain::<flexichain> position::long vec::vector))

;; Deletes an element at POSITION from the chain.
;; If POSITION is out of range, a &flexi-position-error is thrown.
(define-generic (flexi-delete! chain::<flexichain> position::long))

;; Deletes N items at POSITION of the chain.
;; N can be negative, resulting in items before POSITION being deleted.
;; If POSITION+N is out of range, a &flexi-position-error is thrown.
(define-generic (flexi-delete-items! chain::<flexichain> position::long n::long))

;; Returns the item at POSITION.
;; If POSITION is out of range, a &flexi-position-error is thrown.
(define-generic (flexi-ref::obj chain::<flexichain> position::long))

;; Replace the item at POSITION with ITEM.
;; If POSITION is out of range, a &flexi-position-error is thrown.
(define-generic (flexi-set! chain::<flexichain> position::long item::obj))

;; Inserts item at the beginning of chain.
(define-generic (flexi-push-start! chain::<flexichain> item::obj))

;; Inserts item at end of chain. 
(define-generic (flexi-push-end! chain::<flexichain> item::obj))

;; Pops and returns item at beginning of chain. 
(define-generic (flexi-pop-start!::obj chain::<flexichain>))

;; Pops and returns item at end of chain.
(define-generic (flexi-pop-end!::obj chain::<flexichain>))

;; Rotates the items of chain so that the item that used to be at position N
;; is now at position 0. with a negative value for N, rotates the items so that
;; the item used to be at position 0 is now at position N.
(define-generic (flexi-rotate! chain::<flexichain> #!optional (n 1)))

;; Returns the underlying index of the givien position 
(define-generic (flexi-position-index chain::<flexichain> position::long))