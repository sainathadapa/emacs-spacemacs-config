;;; org-agenda-count.el --- Count Org agenda entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Aaron Harris

;; Author: Aaron Harris <meerwolf@gmail.com>
;; Keywords: calendar

;; Dependencies: `alist'
;; Advised functions from other packages:
;;   org: `org-agenda-finalize-entries' (temporary)

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

;; This module provides functionality for counting the number of
;; entries in a custom agenda block, and displaying that count in the
;; agenda.
;;
;; To use it, define a custom agenda command in the usual way, and set
;; the variable `org-agenda-overriding-header' to a value that
;; incorporates the return value of the function `org-agenda-count'.
;; You can think of this function as returning a string containing the
;; number of entries in that agenda block, but the function actually
;; returns a proxy string and sets up its eventual replacement with
;; the correct value; this is necessary because of the way various
;; parts of the agenda are constructed.
;;
;; The `org-agenda-count' function takes an optional argument (a
;; string) that identifies the block it appears in.  If multiple
;; blocks in a single agenda are being counted, all of them need
;; distinct identifiers; if only one block is being counted, its
;; identifier can be omitted.
;;
;; A typical use case is as follows:
;;
;; '((agenda
;;    ""
;;    ((org-agenda-overriding-header
;;      (format "Foo [%s]" (org-agenda-count "foo")))))
;;   (tags-todo
;;    "bar"
;;    ((org-agenda-overriding-header
;;      (format "Bar [%s]" (org-agenda-count "bar")))
;;     (org-agenda-max-entries 5))))
;;
;; This will associate counts with both blocks, but notice that the
;; "Bar" block sets `org-agenda-max-entries'.  Its count will not
;; display 5, but rather the number of entries that would be displayed
;; without this limitation.
;;
;; By default, the proxy value that `org-agenda-count' uses is of the
;; form "##<block>##", where "<block>" stands for the supplied block
;; identifier.  If the double hashes cause trouble for you, this can
;; be changed with the option `org-agenda-count-delimiter'.  Whatever
;; the value of this option, it should not appear as a substring of
;; your block identifiers.

;;; Code:

(load "~/alist.el")
;; (require 'alist)


;;;; User Options
;;===============

(defcustom org-agenda-count-delimiter "##"
  "String used to delimit the proxy for `org-agenda-count'."
  :group 'org-agenda
  :type  'string)


;;;; State Variables
;;==================
(defvar org-agenda-count--alist nil
  "Alist containing agenda block counts for `org-agenda-count'.")

(defvar org-agenda-count--block nil
  "Agenda block being counted for `org-agenda-count'.")


;;;; Implementation
;;=================
(defun org-agenda-count (&optional block)
  "Display a count of the number of items in this agenda block.

For use in custom agenda commands, specifically with
`org-agenda-overriding-header'.  To use this function, call it
while constructing the value for `org-agenda-overriding-header'
and use the return value as if it were the number of items in
this block (before filtering by `org-agenda-max-entries' and
similar).

The optional BLOCK parameter identifies the agenda block being
counted.  To count multiple blocks in the same agenda, each
invocation of `org-agenda-count' should have a different BLOCK
value.

The actual return value is a proxy string incorporating the BLOCK
parameter.  This string is replaced with the actual value during
agenda construction."
  ;; Set up replacement process
  (setq org-agenda-count--block (or block ""))
  (alist-put org-agenda-count--alist
             org-agenda-count--block
             0
             #'equal)
  (advice-add 'org-agenda-finalize-entries :before #'org-agenda--count)
  (add-hook 'org-agenda-finalize-hook #'org-agenda-count--replace)
  ;; Return proxy value
  (concat org-agenda-count-delimiter
          org-agenda-count--block
          org-agenda-count-delimiter))

(defun org-agenda--count (list &optional type)
  "Count the number of entries in this block for `org-agenda-count'.

Intended as (temporary) :before advice for the function
`org-agenda-finalize-hook'."
  (alist-put org-agenda-count--alist
             org-agenda-count--block
             (length list)
             #'equal))

(defun org-agenda-count--regexp ()
  "Return a regexp matching `org-agenda-count' proxy values."
  (let ((delim (regexp-quote org-agenda-count-delimiter)))
    (concat delim "\\([^\n]*?\\)" delim)))

(defun org-agenda-count--reset ()
  "Reset state variables used by `org-agenda-count'.

This includes removing hook functions and temporary advice
functions."
  (setq org-agenda-count--alist nil
        org-agenda-count--block  nil)
  (advice-remove 'org-agenda-finalize-entries #'org-agenda--count)
  (remove-hook 'org-agenda-finalize-hook #'org-agenda-count--replace))

(defun org-agenda-count--replace ()
  "Replace `org-agenda-count' proxy values with entry counts.

Intended for use in `org-agenda-finalize-hook'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (org-agenda-count--regexp) nil :noerror)
      (let* ((block  (match-string 1))
             (count  (cdr (assoc block org-agenda-count--alist))))
        (when count (replace-match (format "%d" count)))))
    (org-agenda-count--reset)))

(provide 'org-agenda-count)
;;; org-agenda-count.el ends here
