;;; reader.el --- Document Viewer for Emacs  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Divya Ranjan Pattanaik <divya@subvertising.org>

;; Author: Divya Ranjan Pattanaik <divya@subvertising.org>
;; Keywords: document reader, mupdf
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://codeberg.org/divyaranjan/emacs-reader

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;; In addition to conditions of the GNU General Public License, this program may
;; not be redistributed without the following acknowledgement:

;;; Commentary:

;;; Code:
(require 'image)
(require 'svg)
(module-load "reader.so")

(defgroup reader nil
  :group 'external
  :prefix "reader-")

;; Define the path to your compiled dynamic module.
;; You might need to adjust this path based on where you compile the module.
(defvar reader-module-path "./reader.so"
  "Path to the compiled render-pdf dynamic module.")

;; Load the dynamic module
(condition-case err
    (progn
      (unless (module-load render-pdf-module-path)
        (error "Could not load reader module from %s" render-pdf-module-path)))
  (file-error (warn "reader module not found at %s. Document rendering will not work. Error: %s"
                    render-pdf-module-path err)))

;; User-facing function to open a PDF
(defun read-pdf (pdf-file)
  "Open PDF-FILE for viewing.
This function calls the C function 'load-pdf' from the dynamic module
to render the first page and display it in a new buffer."
  (interactive "fFind PDF file: ")
  (unless (fboundp 'load-pdf)
    (error "The 'load-pdf' function from the dynamic module is not available. Was the module loaded correctly?"))
  (let ((absolute-pdf-path (expand-file-name pdf-file)))
    ;; Call the C function. It handles creating the buffer and inserting the image.
    (condition-case err
        (load-pdf absolute-pdf-path)
      (error (error "Error loading PDF '%s': %s" absolute-pdf-path err)))
    ;; Switch to the buffer created by the C code (assumed to be named *pdf-svg*)
    ;; and enable the major mode.
    (let ((pdf-buffer (get-buffer "*pdf-svg*")))
      (when pdf-buffer
        (with-current-buffer pdf-buffer
          (read-pdf-mode))))))

;; Internal function to go to the next page
(defun read-pdf--next-page ()
  "Go to the next page of the PDF."
  (interactive)
  (unless (fboundp 'next-pdf-page)
    (error "The 'next-pdf-page' function from the dynamic module is not available."))
  (condition-case err
      (unless (next-pdf-page)
        (message "Already at the last page."))
    (error (error "Error going to next page: %s" err)))
  ;; Ensure buffer remains read-only after potential updates
  (with-current-buffer (current-buffer)
    (setq buffer-read-only t)))

;; Internal function to go to the previous page
(defun read-pdf--previous-page ()
  "Go to the previous page of the PDF."
  (interactive)
  (unless (fboundp 'previous-pdf-page)
    (error "The 'previous-pdf-page' function from the dynamic module is not available."))
  (condition-case err
      (unless (previous-pdf-page)
        (message "Already at the first page."))
    (error (error "Error going to previous page: %s" err)))
  ;; Ensure buffer remains read-only after potential updates
  (with-current-buffer (current-buffer)
    (setq buffer-read-only t)))

;; Define the keymap for read-pdf-mode
(defvar read-pdf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'read-pdf--next-page)
    (define-key map (kbd "j") #'read-pdf--next-page)
    (define-key map (kbd "p") #'read-pdf--previous-page)
    (define-key map (kbd "k") #'read-pdf--previous-page)
    ;; Add other useful bindings if needed, e.g., quit
    (define-key map (kbd "q") #'kill-this-buffer)
    map)
  "Keymap for read-pdf-mode.")

;; Define the major mode
(define-derived-mode read-pdf-mode special-mode "ReadPDF"
  "Major mode for viewing PDFs rendered by render-pdf module.

Keybindings:
\\{read-pdf-mode-map}"
  ;; Make the buffer read-only to prevent editing the SVG image
  (setq buffer-read-only t)
  ;; You might want to hide the mode line lighter if it's distracting
  ;; (setq mode-line-format nil)
  )

(provide 'reader.el)
;;; reader.el ends here.
