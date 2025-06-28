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
(require 'outline)

;;; Imenu

;;;###autoload
(defun reader--make-imenu-entry (plist)
  "Convert one outline PLIST to an imenu entry using `reader-goto-page'."
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
  "Switch to PAGE, ignores the NAME argument.
Wraps `reader-goto-page' for imenu compatibility."
  (reader-goto-page (1+ page)))

;;; Outline

;;;###autoload
(defun reader-show-outline ()
  "Show the document outline in a separate buffer."
  (interactive)
  (unless (derived-mode-p 'reader-mode)
    (user-error "Not in a reader-mode buffer"))
  (unless reader-current-doc-outline
    (user-error "This document has no outline"))
  (let ((outline-data reader-current-doc-outline)
        (source-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create "*Reader Outline*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (reader-outline-mode)
        (reader--insert-outline outline-data 1 source-buffer))
      (goto-char (point-min)))
    (pop-to-buffer "*Reader Outline*")))

;;;###autoload
(defun reader--insert-outline (outline level source-buffer)
  "Recursively insert OUTLINE entries at LEVEL.
Adds SOURCE-BUFFER and page as text properties."
  (dolist (entry outline)
    (let* ((title (plist-get entry :title))
           (page (plist-get entry :page))
           (children (plist-get entry :children))
           (line (concat (make-string level ?*) " " title "\n")))
      (let ((start (point)))
        (insert line)
        (when (numberp page)
          (put-text-property start (point) 'reader-page page)
          (put-text-property start (point) 'reader-buffer source-buffer)))
      (when children
        (reader--insert-outline children (1+ level) source-buffer)))))

(defvar-keymap reader-outline-mode-map
  "M-RET"    #'reader-outline-visit-page)

(define-derived-mode reader-outline-mode outline-mode "Reader Outline"
  "Major mode for navigating document outlines."
  (setq buffer-read-only t)
  (setq-local outline-regexp "^\\*+ ")
  (use-local-map reader-outline-mode-map))

;;;###autoload
(defun reader-outline-visit-page ()
  "Jump to the page at point using properties."
  (interactive)
  (let* ((page (get-text-property (point) 'reader-page))
         (src  (get-text-property (point) 'reader-buffer)))
    (unless (and (numberp page) src)
      (user-error "Missing page or source buffer info"))
    (select-window (display-buffer src))
    (reader-goto-page page)))

(provide 'reader-outline)
;;; reader-outline.el ends here
