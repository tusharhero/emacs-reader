;;; reader.el --- General-purpose Document Viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Divya Ranjan Pattanaik

;; Author: Divya Ranjan Pattanaik <divya@subvertising.org>
;; Keywords: lisp, files, tools
;; Version: 0.1.9
;; Package-Requires: ((emacs "26.1"))
;; URL: https://codeberg.org/divyaranjan/emacs-reader

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

;; This package provides a general purpose document reader within
;; Emacs by leveraging the use of dynamic modules.  It primarily
;; relies on MuPDF to do the rendering, and Emacs Lisp’s native SVG
;; capabilities to view the rendered images and manipulate them.

;;; Code:

(require 'image)
(require 'image-mode)
(require 'svg)
(require 'render-core)

(defgroup reader nil
  "Group for Reader’s customizations."
  :prefix "reader-"
  :group 'custom)

(defcustom reader-enlarge-factor 1.10
  "The fractional amount by which the page would be enlarged."
  :group 'reader
  :type 'number)

(defcustom reader-shrink-factor 0.90
  "The fractional amount by which the page would be shrinked."
  :group 'reader
  :type 'number)

(defvar reader-current-doc-scale 1)
(make-variable-buffer-local 'reader-current-doc-scale)

(defun reader-open-doc (file)
  "Open document FILE for viewing.
This function calls the C function `load-doc' from the dynamic module
to render the first page and displays it in a new buffer."
  (interactive "fOpen document: ")
  (switch-to-buffer (create-file-buffer file))
  (insert "\n")
  (load-doc (expand-file-name file))
  (reader-mode))

(defun reader-next-page ()
  "Go to the next page of the visiting document."
  (interactive)
  (next-doc-page)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-previous-page ()
  "Go to the previous page of the visiting document."
  (interactive)
  (previous-doc-page)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-first-page ()
  "Go to the first page of the visiting document."
  (interactive)
  (first-doc-page)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-last-page ()
  "Go to the last page of the visiting document."
  (interactive)
  (last-doc-page)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-goto-page (n)
  "Go to page number 'N' in the current document."
  (interactive "nPage: ")
  (goto-doc-page (- n 1)) ; MuPDF does 0-indexing
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-enlarge-size ()
  "Enlarge the size of the current page with respect to the `reader-enlarge-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale reader-enlarge-factor)))
    (doc-change-page-size scaling-factor)
    (setq reader-current-doc-scale scaling-factor))
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-shrink-size ()
  "Shrink the size of the current page with respect to the `reader-shrink-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale reader-shrink-factor)))
    (doc-change-page-size scaling-factor)
    (setq reader-current-doc-scale scaling-factor))
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-kill-buffer ()
  "Kill the current buffer and the document."
  (interactive)
  (kill-buffer (current-buffer)))

(defun get-current-doc-image-size ()
  "Get the size of the current page's doc image."
  (image-size (overlay-get current-svg-overlay 'display) t))

(defun reader-center-page (&optional window)
  "Centers the pages of the document with respect to the WINDOW in which the document is opened."
  (with-current-buffer (window-buffer window)
    (when (equal major-mode 'reader-mode)
      (let* ((window-width (window-width window))
	     (pixel-window-width (window-width window t))
	     (pixel-per-col (/ pixel-window-width
                               window-width))
	     (doc-image-width (car (get-current-doc-image-size)))
	     (doc-fits-p (> pixel-window-width doc-image-width))
	     (raw-offset (/ (- pixel-window-width doc-image-width) 2))
	     (overlay-offset
	      `(space :width (,(if doc-fits-p raw-offset 0)))))
        (overlay-put current-svg-overlay 'line-prefix overlay-offset)
	(when-let* (((not doc-fits-p)) ; scroll to the center of the doc
		    (scroll-offset
		     (/ (abs raw-offset) pixel-per-col)))
	  (set-window-hscroll window scroll-offset))))))

(defun reader-render-buffer ()
  "Render the document file this buffer is associated with.  It is to be called while a document’s buffer is already opened and the buffer is not in `reader-mode'."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
	  (load-doc file))
      (message "No file associated with buffer."))))

;; Define the keymap for reader-mode
(defvar reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'reader-next-page)
    (define-key map (kbd "j") #'reader-next-page)
    (define-key map (kbd "C-n") #'reader-next-page)
    (define-key map (kbd "p") #'reader-previous-page)
    (define-key map (kbd "k") #'reader-previous-page)
    (define-key map (kbd "C-p") #'reader-previous-page)
    (define-key map (kbd "gg") #'reader-first-page)
    (define-key map (kbd "M-<") #'reader-first-page)
    (define-key map (kbd "G") #'reader-last-page)
    (define-key map (kbd "M->") #'reader-last-page)
    (define-key map (kbd "M-g g") #'reader-goto-page)
    (define-key map (kbd "g n") #'reader-goto-page)
    (define-key map (kbd "=") #'reader-enlarge-size)
    (define-key map (kbd "-") #'reader-shrink-size)
    (define-key map (kbd "Q") #'reader-kill-buffer)
    map)
  "Keymap for reader-mode.")

;; Define the major mode
(defun reader-mode ()
  "Major mode for viewing documents in The Emacs Reader.

Keybindings:
\\{reader-mode-map}"
  (interactive)
  (setq-local buffer-read-only t
	      global-linum-mode nil
              display-line-numbers-mode nil)
  (set-buffer-modified-p nil)
  (blink-cursor-mode 0)
  ;; Only do this when document is not already rendered
  (when (not doc-render-status)
    (reader-render-buffer))

  (use-local-map reader-mode-map)
  (setq major-mode 'reader-mode)
  (setq mode-name "Emacs Reader")
  (run-hooks 'reader-mode-hook)
  ;; Invoke centering every time window's size changes only in reader-mode windows
  (add-hook 'window-size-change-functions #'reader-center-page nil t))

;; Modeline for the reader-mode
(defun reader-mode-line ()
  "Set custom mode-line interface when reading documents."
  (setq-local mode-line-format
	      (list
	       "Page: "
	       '(:eval (number-to-string (+ 1 (get-current-doc-pagenumber))))
	       "/"
	       '(:eval (number-to-string current-doc-pagecount))
	       "  "
	       mode-line-buffer-identification))
  (force-mode-line-update t))

(add-hook 'reader-mode-hook #'reader-mode-line)

;; Automatically load the mode for the supported document formats
(dolist (pattern '("\\.pdf\\'"
		   "\\.epub\\'"
		   "\\.odt\\'"
		   "\\.ods\\'"
		   "\\.odg\\'"
		   "\\.odp\\'"
		   "\\.docx\\'"
		   "\\.pptx\\'"
		   "\\.xlsx\\'"
		   "\\.fb2\\'"
		   "\\.xps\\'"
		   "\\.mobi\\'"
		   "\\.cbz\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'reader-mode)))

(provide 'reader)
;;; reader.el ends here.
