;;; reader-outline.el --- imenu and outline-mode integration for the emacs-reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Divya Ranjan Pattanaik

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'reader)
(require 'imenu)

;;; Imenu
;;;###autoload
(defun reader--make-imenu-entry (plist)
  "Convert one outline PLIST to an imenu entry using `reader-goto-page`."
  (let* ((title    (plist-get plist :title))
         (page     (plist-get plist :page))
         (children (plist-get plist :children)))
    (if children
        (cons title
              (mapcar #'reader--make-imenu-entry children))
      (cons title page))))

;;;###autoload
(defun reader--imenu-create-index ()
  "Turn `reader-current-doc-outline' into an imenu index."
  (if reader-current-doc-outline
      (mapcar #'reader--make-imenu-entry
	      reader-current-doc-outline)
    (error "Document lacks a proper outline!")))

;;;###autoload
(defun reader--imenu-goto (name page)
  "Wraps reader-goto-page for imenu compatibility,
ignores the name argument."
  (reader-goto-page (1+ page)))

(provide 'reader-outline)
;;; reader-outline.el ends here
