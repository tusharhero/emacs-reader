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

(defcustom reader-enlarge-factor 1.25
  "The fractional amount by which the page would be enlarged."
  :group 'reader
  :type 'number)

(defcustom reader-shrink-factor 0.75
  "The fractional amount by which the page would be shrinked."
  :group 'reader
  :type 'number)

(defvar reader-current-doc-scale 1)
(make-variable-buffer-local 'reader-current-doc-scale)

(defun reader-open-doc ()
  "Open a document for viewing.
This function calls the module function `render-core-load-doc' from the dynamic module
to render the first page and displays it in a new buffer.  The only files
that can be opened are of the following formats:
- PDF
- EPUB
- MOBI
- FB2
- XPS/OpenXPS
- CBZ
- DOCX/PPTX/XLSX
- ODT/ODS/ODP/ODG

Any other file format would simply not show up as a candidate."
  (interactive)
  (let* ((exts '("pdf" "epub" "mobi" "fb2" "xps" "cbz" "docx"
		 "pptx" "xlsx" "odt" "ods" "odp" "odg"))
	 (rgx (concat "\\." (regexp-opt exts t) "$"))
	 (files (directory-files default-directory nil rgx))
	 (file (read-file-name
		"Open document: "
		nil nil t nil
		(lambda (f)
                  (or (file-directory-p f)
                      (string-match-p rgx f))))))
    (switch-to-buffer (create-file-buffer file))
    (insert "\n")
    (render-core-load-doc (expand-file-name file))
    (reader-mode)))

(defun reader-next-page ()
  "Go to the next page of the visiting document."
  (interactive)
  (let ((status (render-core-next-page)))
    (when status
      (render-core-change-page-size reader-current-doc-scale)
      (reader-center-page)
      (force-mode-line-update t))
    status))

(defun reader-previous-page ()
  "Go to the previous page of the visiting document."
  (interactive)
  (let ((status (render-core-prev-page)))
    (render-core-change-page-size reader-current-doc-scale)
    (reader-center-page)
    (force-mode-line-update t)
    status))

(defun reader-first-page ()
  "Go to the first page of the visiting document."
  (interactive)
  (render-core-first-page)
  (render-core-change-page-size reader-current-doc-scale)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-last-page ()
  "Go to the last page of the visiting document."
  (interactive)
  (render-core-last-page)
  (render-core-change-page-size reader-current-doc-scale)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-goto-page (n)
  "Go to page number 'N' in the current document."
  (interactive "nPage: ")
  (render-core-goto-page (- n 1)) ; MuPDF does 0-indexing
  (render-core-change-page-size reader-current-doc-scale)
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-enlarge-size ()
  "Enlarge the size of the current page with respect to the `reader-enlarge-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale reader-enlarge-factor)))
    (render-core-change-page-size scaling-factor)
    (setq reader-current-doc-scale scaling-factor))
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-shrink-size ()
  "Shrink the size of the current page with respect to the `reader-shrink-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale reader-shrink-factor)))
    (render-core-change-page-size scaling-factor)
    (setq reader-current-doc-scale scaling-factor))
  (reader-center-page)
  (force-mode-line-update t))

(defun reader-scroll-up (&optional amount)
  "Scroll up the current page.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (let ((vscroll (- (window-vscroll) (if (not amount)
					 1 amount))))
    (set-window-vscroll nil vscroll)))

(defun reader-possible-scroll-down (&optional amount)
  "Return 1 (or AMOUNT) if that scroll is possible, otherwise return the max possible.
If none is possible return nil."
  (let* ((amount (if (not amount) 1 amount))
	 (image-height (cdr (get-current-doc-image-size)))
	 (window-height (window-body-height))
	 (pixel-window-height (window-body-height nil t))
	 (pixel-per-col (/ pixel-window-height
                           window-height))
         (pixel-amount (* pixel-per-col amount))
	 (pixel-current-scroll (window-vscroll nil t))
	 (pixel-predicted-scroll (+ pixel-current-scroll
				    pixel-amount))
         (win-bottom-pos (+ pixel-current-scroll
			    pixel-window-height))
	 (predicted-win-bottom-position (+ pixel-predicted-scroll
					   pixel-window-height)))
    (if (> predicted-win-bottom-position image-height)
	(let* ((max-scroll-amount
		(round (/ (- image-height win-bottom-pos) pixel-per-col))))
	  (when (< 0 max-scroll-amount)
	    max-scroll-amount))
      amount)))

(defun reader-scroll-down (&optional amount)
  "Scroll down the current page.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (when-let* ((amount (reader-possible-scroll-down (if (not amount)
						       1 amount)))
	      (vscroll (+ (window-vscroll) amount)))
    (set-window-vscroll nil vscroll)))

(defun reader-scroll-up-screenful (&optional amount)
  "Scroll up the current page by a screenful.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (when (not (reader-scroll-up scroll))
      (message "Beginning of page"))))

(defun reader-scroll-down-screenful (&optional amount)
  "Scroll down the current page by a screenful.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (when (not (reader-scroll-down scroll))
      (message "End of page"))))

(defun reader-scroll-left ()
  "Scroll to the left of the current page."
  (interactive)
  (set-window-hscroll nil
		      (1- (window-hscroll))))

(defun reader-scroll-right ()
  "Scroll to the left of the current page."
  (interactive)
  (set-window-hscroll nil
		      (1+ (window-hscroll))))

(defun reader-scroll-up-or-prev-page (&optional amount)
  "Scroll up the current page or go to the previous page if can't scroll.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (let* ((prev-scroll (window-vscroll)))
    (reader-scroll-up amount)
    (when-let* (((and (= prev-scroll (window-vscroll))
		      ;; if succeeds
		      (reader-previous-page)))
		(image-height (cdr (get-current-doc-image-size)))
		(pixel-window-height (window-body-height nil t))
		(bottom-most-scroll-pixel
		 (- image-height pixel-window-height)))
      (set-window-vscroll nil bottom-most-scroll-pixel t))))

(defun reader-scroll-down-or-next-page (&optional amount)
  "Scroll down the current page or go to the next page if can't scroll.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (let* ((prev-scroll (window-vscroll)))
    (reader-scroll-down amount)
    (when (and (= prev-scroll (window-vscroll))
	       ;; if succeeds
	       (reader-next-page))
      (set-window-vscroll nil 0))))

(defun reader-kill-buffer ()
  "Kill the current buffer and the document."
  (interactive)
  (kill-buffer (current-buffer)))

(defun get-current-doc-image-size ()
  "Get the size of the current page's doc image."
  (let* ((cdr-image (cdr (overlay-get current-svg-overlay 'display)))
	 (width (plist-get cdr-image :width))
	 (length (plist-get cdr-image :length)))
    (cons width length)))

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
		     (round (/ (abs raw-offset) pixel-per-col))))
	  (set-window-hscroll window scroll-offset))))))

(defun reader-render-buffer ()
  "Render the document file this buffer is associated with.  It is to be called while a document’s buffer is already opened and the buffer is not in `reader-mode'."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
	  (render-core-load-doc file))
      (message "No file associated with buffer."))))

;; Define the keymap for reader-mode
(defvar reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") #'reader-next-page)
    (define-key map (kbd "J") #'reader-next-page)
    (define-key map (kbd "j") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "n") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "C-n") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "SPC") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "<down>") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "<wheel-down>") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "<next>") #'reader-scroll-down-screenful)
    (define-key map (kbd "C-v") #'reader-scroll-down-screenful)

    (define-key map (kbd "P") #'reader-previous-page)
    (define-key map (kbd "K") #'reader-previous-page)
    (define-key map (kbd "p") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "k") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "C-p") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "DEL") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "S-SPC") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "<up>") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "<wheel-up>") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "<prior>") #'reader-scroll-up-screenful)
    (define-key map (kbd "M-v") #'reader-scroll-up-screenful)

    (define-key map (kbd "h") #'reader-scroll-left)
    (define-key map (kbd "l") #'reader-scroll-right)
    (define-key map (kbd "C-f") #'reader-scroll-right)
    (define-key map (kbd "C-b") #'reader-scroll-left)

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
	       '(:eval (number-to-string (+ 1 (render-core-get-current-pagenumber))))
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
