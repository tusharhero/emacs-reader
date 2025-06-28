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

(defvar-local reader-outline--doc-buffer nil
  "The document buffer whose outline was generated.")

;;;###autoload
(defun reader-show-outline ()
  "Show the document outline in a separate buffer.
The outline buffer inherits it's name from the original
document it was created from."
  (interactive)
  (unless (derived-mode-p 'reader-mode)
    (user-error "Not in a reader-mode buffer"))
  (unless reader-current-doc-outline
    (user-error "This document has no outline"))
  (let* ((outline-data reader-current-doc-outline)
         (source-buffer (current-buffer))
         (bufname (format "*Outline of %s*" (buffer-name source-buffer))))
    (with-current-buffer (get-buffer-create bufname)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (reader-outline-mode)
	(setq reader-outline--doc-buffer source-buffer)
        (reader--insert-outline outline-data 1 source-buffer))
      (goto-char (point-min)))
    (pop-to-buffer bufname)))

;;;###autoload
(defun reader-toggle-outline ()
  "Toggle the Reader outline buffer for the current reader-mode buffer."
  (interactive)
  (let ((buf (get-buffer "*Emacs Reader Outline*")))
    (if (and buf (get-buffer-window buf))
        (quit-window nil (get-buffer-window buf))
      (reader-show-outline))))

;;;###autoload
(defun reader--insert-outline (outline level source-buffer)
  "Recursively insert OUTLINE entries at LEVEL.
Each heading title is its own clickable button."
  (dolist (entry outline)
    (let ((title    (plist-get entry :title))
          (page     (plist-get entry :page))
          (children (plist-get entry :children)))
      ;; Insert the outline stars and space
      (insert (make-string level ?*) " ")
      (insert-text-button
       title
       'reader-page page
       'reader-source-buffer source-buffer
       'action #'reader-outline--button-action
       'follow-link t
       'help-echo "Click to jump to this page in the document")
      (insert "\n")
      (when children
        (reader--insert-outline children (1+ level) source-buffer)))))

(defun reader-outline-select-doc-window ()
  "Display and switch to the original document's window."
  (interactive)
  (select-window (display-buffer reader-outline--doc-buffer)))

;;;###autoload
(defvar-keymap reader-outline-mode-map
  :doc "Keymap for `reader-outline-mode'"
  "o"        #'reader-outline-select-doc-window
  "q"        #'quit-window
  "M-RET"    #'reader-outline-visit-page)

;;;###autoload
(define-derived-mode reader-outline-mode outline-mode "Emacs Reader Outline"
  "Major mode for navigating document outlines."
  (setq buffer-read-only t)
  (setq-local outline-regexp "^\\*+ ")
  (use-local-map reader-outline-mode-map))

;;;###autoload
(defun reader-outline-goto-entry (button)
  "Shared logic to jump to an outline BUTTON."
  (let* ((page (button-get button 'reader-page))
         (src  (button-get button 'reader-source-buffer)))
    (unless (and (numberp page) (buffer-live-p src))
      (user-error "Invalid outline entry: no page or buffer info"))
    (select-window (display-buffer src))
    (reader-goto-page (1+ page))))

;;;###autoload
(defun reader-outline--button-action (button)
  "Jump to the page associated with BUTTON."
  (reader-outline-goto-entry button))

;;;###autoload
(defun reader-outline-visit-page ()
  "Jump to the page at point in the associated reader buffer."
  (interactive)
  (let ((button (button-at (point))))
    (unless button
      (user-error "No button at point"))
    (reader-outline-goto-entry button)))

(provide 'reader-outline)
;;; reader-outline.el ends here
