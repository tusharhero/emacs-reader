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

(defvar-local reader-outline--source-buffer nil
  "Buffer from which the Reader outline was generated.")

(defvar-local reader-outline--data nil
  "Copy of the outline plist used in the Reader outline buffer.")

(defun reader-show-outline ()
  "Show the document outline in a separate buffer."
  (interactive)
  (unless (derived-mode-p 'reader-mode)
    (user-error "Not in a reader-mode buffer"))
  (unless reader-current-doc-outline
    (user-error "This document has no outline"))
  (let ((source-buffer (current-buffer))
        (outline-data reader-current-doc-outline))
    (with-current-buffer (get-buffer-create "*Reader Outline*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (reader-outline-buffer-mode)
        (setq reader-outline--source-buffer source-buffer)
        (setq reader-outline--data outline-data)
        (reader--insert-outline outline-data 1))
      (goto-char (point-min)))
    (pop-to-buffer "*Reader Outline*")))

(defun reader--insert-outline (outline level)
  "Recursively insert OUTLINE entries at LEVEL."
  (dolist (entry outline)
    (let ((title    (plist-get entry :title))
          (page     (plist-get entry :page))
          (children (plist-get entry :children)))
      (let ((start (point)))
        (insert (make-string level ?*) " " title "\n")
        (when (numberp page)
          (put-text-property start (point) 'reader-page page)))
      (when children
        (reader--insert-outline children (1+ level))))))

(define-derived-mode reader-outline-buffer-mode outline-mode "Reader-Outline"
  "Major mode for navigating document outlines."
  (setq buffer-read-only t)
  (setq-local outline-regexp "^\\*+ ")
  (use-local-map (copy-keymap outline-mode-map))
  (define-key reader-outline-buffer-mode-map (kbd "RET") #'reader-outline-visit-page))

(defun reader-outline-visit-page ()
  "Jump to the page at point in the associated reader buffer."
  (interactive)
  (let* ((page (get-text-property (point) 'reader-page))
         (src (or reader-outline--source-buffer
                  (user-error "No source buffer set"))))
    (unless (numberp page)
      (user-error "No page info at this line"))
    ;; Switch to source buffer and call `reader-goto-page`
    (select-window (display-buffer src))
    (reader-goto-page page)))

(provide 'reader-outline)
;;; reader-outline.el ends here
