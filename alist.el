;;; alist.el --- A better alist interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: extensions, alist

;; Dependencies: `cl-lib'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some operations on alists are much more laborious than they should
;; be, most notably the process of updating a key that may or may not
;; be present.  In order to set `key' to `val' in a variable `alist',
;; you have to do something like this:
;;
;;     (let ((elt (assq key alist)))
;;       (if elt (setf (cdr elt) val)
;;         (setq alist (push `(,key . ,val) alist))))
;;
;; The `map' library for Emacs 25 neatly solves this
;; problem, turning the above code into this:
;;
;;     (map-put alist key val)
;;
;; Unfortunately, this library is not available for Emacs 24.  This
;; module is intended as a partial stopgap measure, providing
;; functions to solve the most egregious of these interface problems,
;; at least in alists.  Unlike the `map' module, no attempt is made to
;; support hash maps or arrays.

;;; Code:

(require 'cl-lib)


;;;; Updating
;;===========
(defmacro alist-delete (alist key &optional test)
  "Delete association in ALIST for KEY.  Return ALIST.

If the optional parameter TEST is supplied, it is used in place
of `eql' to compare elements.

Here, ALIST may be any generalized variable containing an
alist."
  (declare (debug (gv-place form &optional function-form)))
  (let ((test (or test '#'eql)))
    `(setf ,alist
           (cl-remove ,key ,alist :test ,test :key #'car))))

(defun alist--put (alist key value &optional test)
  "Subroutine used by `alist-put'.

As `alist-put', except back-assignment may be necessary, as with
`delete'."
  (let* ((test  (or test #'eql))
         (elt   (cl-assoc key alist :test test)))
    (if elt (setf (cdr elt) value)
      (setq alist (push (cons key value) alist)))
    alist))

(defmacro alist-put (alist key value &optional test)
  "Associate KEY with VALUE in ALIST.  Return ALIST.

If ALIST does not already contain an association for KEY, it is
added; otherwise, the existing association is updated.

If the optional parameter TEST is supplied, it is used in place
of `eql' to compare elements.

Here, ALIST may be any generalized variable containing an alist."
  (declare (debug (gv-place form form &optional function-form)))
  `(setf ,alist (alist--put ,alist ,key ,value ,test)))

(defun alist-insert--compare (k1 k2)
  "Default comparator for `alist-insert'.

Return t if K1 is equal to K2; a negative integer if K1 is less
than K2; and a positive integer if K1 is greater than K2.

For numbers, comparison is by `=' and `<'.  For strings and
symbols, comparison is by `compare-strings' and does not ignore
case.

If K1 and K2 are of different types (or a type other than
numbers, strings, or symbols), signal an error."
  (cond
   ((and (numberp k1) (numberp k2))
    (cond ((= k1 k2) t) ((< k1 k2) -1) ((> k1 k2) 1)))

   ((and (stringp k1) (stringp k2))
    (compare-strings k1 nil nil k2 nil nil))

   ((and (symbolp k1) (symbolp k2))
    (alist-insert--compare (symbol-name k1) (symbol-name k2)))

   (:else
    (error "`alist-insert--compare' cannot compare keys %s, %s" k1 k2))))

(defun alist-insert--compare-down (k1 k2)
  "Reverse of `alist-insert--compare'."
  (let ((result (alist-insert--compare k1 k2)))
    (if (eq result t) t (- result))))

(defun alist--insert (alist key value &optional compare)
  "Subroutine used by `alist-insert'.

As `alist-insert', except back-assignment may be necessary, as
with `delete'."
  (let* ((compare (cond
                   ((eq compare :down)  #'alist-insert--compare-down)
                   ((null compare)      #'alist-insert--compare)
                   (:else               compare)))
         (cursor  alist) 
         comparison last-cursor)
    (if (null alist) (list (cons key value))
      (while (and (not (null cursor))
                  (setq comparison (funcall compare (caar cursor) key))
                  (numberp comparison)
                  (< comparison 0)) 
        (setq last-cursor cursor
              cursor      (cdr cursor)))
      (cond
       ((eq comparison t)   (setf (cdar cursor) value))
       ((null last-cursor)  (push (cons key value) alist))
       (:else               (push (cons key value) (cdr last-cursor)))) 
      alist)))

(defmacro alist-insert (alist key value &optional compare)
  "As ALIST-PUT for an ALIST which is sorted by its keys.

If ALIST contains an association for KEY, it is updated to VALUE.
Otherwise, an association is added in such a way that ALIST
remains sorted.

If the optional parameter COMPARE is supplied, it should be a
comparator taking two keys and returning either t or an integer.
The value t means that the two keys should be considered equal; a
positive number indicates the first key is larger than the
second; and a negative number indicates the first key is smaller
than the second.

If COMPARE is omitted, keys are sorted according to their type:
numbers are sorted with `<', and strings and symbols are sorted
with `compare-strings'.  Keys of different types are not
supported.

Instead of passing a function to COMPARE, you can also pass the
keyword :down.  This just means to use the default comparator in
reverse order.

If ALIST is not initially sorted according to COMPARE, behavior
is undefined."
  (declare (debug (gv-place form form &optional &or function-form ":down")))
  `(setf ,alist (alist--insert ,alist ,key ,value ,compare)))


;;;; Equality Testing
;;===================
(defun alist-equal (alist1 alist2 &optional key-test value-test default)
  "Return non-nil if ALIST1 and ALIST2 are equal as alists.

Two alists are considered equal if the values for corresponding
keys (where equality is determined by KEY-TEST) are equal
according to VALUE-TEST.  Most notably, keys may appear in any
order.

If KEY-TEST is omitted, it defaults to `eq', while if VALUE-TEST
is omitted, it defaults to `equal'.  This is consistent with the
most common use of alists, in which keys are symbols but values
may be a wide range of types.

If a key appears in one list but not the other, then DEFAULT will
be used for its value in the list where it does not appear."
  (setq key-test   (or key-test   #'eq)
        value-test (or value-test #'equal))
  (catch 'fail
    (while (not (null alist1))
      (let* ((elt  (pop alist1))
             (k1   (car elt))
             (v1   (cdr elt))
             (v2   (or (cdr (cl-assoc k1 alist2 :test key-test))
                       default)))
        (if (funcall value-test v1 v2)
            (alist-delete alist2 k1 key-test)
          (throw 'fail nil))))
    (if (null alist2) t
      (alist-equal alist2 alist1 key-test value-test default))))

(provide 'alist)
;;; alist.el ends here
