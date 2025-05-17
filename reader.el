;;; reader.el --- General-purpose Document Reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Divya Ranjan Pattanaik
;; Copyright (C) 2025  Tushar

;; Author: Divya Ranjan Pattanaik <divya@subvertising.org>
;; Keywords: lisp, files, tools
;; Version: 0.2.0
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

(defvar reader-current-doc-scale-value 1.0
  "The amount of scaling for the current document. Defaults to 1.0.")
(make-variable-buffer-local 'reader-current-doc-scale-value)

(defun reader-open-doc ()
  "Open a document for viewing.
This function calls the module function `reader-dyn--load-doc' from the dynamic module
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
    (reader-dyn--load-doc (expand-file-name file))
    (reader-mode)))

(defun reader-next-page ()
  "Go to the next page of the visiting document."
  (interactive)
  (let ((status (reader-dyn--next-page)))
    (when status
      (reader-doc-scale-page reader-current-doc-scale-value)
      (reader--center-page)
      (force-mode-line-update t))
    status))

(defun reader-previous-page ()
  "Go to the previous page of the visiting document."
  (interactive)
  (let ((status (reader-dyn--prev-page)))
    (when status
      (reader-doc-scale-page reader-current-doc-scale-value)
      (reader--center-page)
      (force-mode-line-update t))
    status))

(defun reader-first-page ()
  "Go to the first page of the visiting document."
  (interactive)
  (reader-dyn--first-page)
  (reader-doc-scale-page reader-current-doc-scale-value)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-last-page ()
  "Go to the last page of the visiting document."
  (interactive)
  (reader-dyn--last-page)
  (reader-doc-scale-page reader-current-doc-scale-value)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-goto-page (n)
  "Go to page number 'N' in the current document."
  (interactive "nPage: ")
  (reader-dyn--goto-page (- n 1)) ; MuPDF does 0-indexing
  (reader-doc-scale-page reader-current-doc-scale-value)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-enlarge-size ()
  "Enlarge the size of the current page with respect to the `reader-enlarge-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale-value reader-enlarge-factor)))
    (reader-doc-scale-page scaling-factor))
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-shrink-size ()
  "Shrink the size of the current page with respect to the `reader-shrink-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale-value reader-shrink-factor)))
    (reader-doc-scale-page scaling-factor))
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-fit-to-height ()
  "Scale the current page so that its height fits perfectly within the window."
  (interactive)
  (let* ((image-height (cdr (reader--get-current-doc-image-size)))
	 (pixel-window-height (window-pixel-height))
	 (unscaled-height (/ image-height reader-current-doc-scale-value))
	 (scaling-factor (/ pixel-window-height unscaled-height)))
    (reader-doc-scale-page scaling-factor)
    (reader--center-page)
    (set-window-vscroll nil 0)))

(defun reader-fit-to-width ()
  "Scale the current page so that its width fits perfectly within the window."
  (interactive)
  (let* ((image-width (car (reader--get-current-doc-image-size)))
	 (pixel-window-width (window-pixel-width))
	 (unscaled-width (/ image-width reader-current-doc-scale-value))
	 (scaling-factor (/ pixel-window-width unscaled-width)))
    (reader-doc-scale-page scaling-factor)
    (reader--center-page)))

(defun reader-doc-scale-page (factor)
  "Scales the page by a given FACTOR.

It calls the module function `reader-dyn--scale-page' that
sets the :width, :height and :scale propoerties of current page’s image.
It also updates `reader-current-doc-scale-value' to reflect the new scale."
  (reader-dyn--scale-page factor)
  (setq reader-current-doc-scale-value factor))

(defun reader-scroll-up (&optional amount)
  "Scroll up the current page.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (let* ((vscroll
	  (max (- (window-vscroll) amount) 0)))
    (set-window-vscroll nil vscroll)))

(defun reader-possible-scroll-down (&optional amount)
  "Return 1 (or AMOUNT) if that scroll is possible, otherwise return the max possible.
If none is possible return nil."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((image-height (cdr (reader--get-current-doc-image-size)))
	 (window-height (window-body-height))
	 (pixel-window-height (window-pixel-height))
	 (pixel-per-col (/ pixel-window-height
			   window-height))
	 (pixel-amount (* pixel-per-col amount))
	 (pixel-current-scroll (window-vscroll nil t))
	 (pixel-predicted-scroll (+ pixel-current-scroll
				    pixel-amount))
	 (win-bottom-pos (+ pixel-current-scroll
			    pixel-window-height))
	 (predicted-win-bottom-position (+ pixel-predicted-scroll
					   pixel-window-height))
	 (scroll-p (> predicted-win-bottom-position image-height)))
    (if scroll-p
	(let* ((pixel-max-scroll-amount (- image-height win-bottom-pos))
	       (max-scroll-amount
		(round (/ pixel-max-scroll-amount pixel-per-col))))
	  max-scroll-amount)
      amount)))

(defun reader-scroll-down (&optional amount)
  "Scroll down the current page.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((amount (reader-possible-scroll-down amount))
	 (vscroll (+ (window-vscroll) amount)))
    (set-window-vscroll nil vscroll)))

(defun reader-scroll-up-screenful ()
  "Scroll up the current page by a screenful."
  (interactive)
  (let ((prev-scroll (window-vscroll))
	(amount (- (window-body-height)
		   next-screen-context-lines)))
    (when (= prev-scroll
	     (reader-scroll-up amount))
      (message "Beginning of page"))))

(defun reader-scroll-down-screenful ()
  "Scroll down the current page by a screenful."
  (interactive)
  (let ((prev-scroll (window-vscroll))
	(amount (- (window-body-height)
		   next-screen-context-lines)))
    (when (= prev-scroll
	     (reader-scroll-down amount))
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
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll)))
    (reader-scroll-up amount)
    (when-let* (((and (= prev-scroll (window-vscroll))
		      (reader-previous-page))) ; if succeeds
		(image-height (cdr (reader--get-current-doc-image-size)))
		(pixel-window-height (window-pixel-height))
		(bottom-most-scroll-pixel
		 (- image-height pixel-window-height)))
      (set-window-vscroll nil bottom-most-scroll-pixel t))))

(defun reader-scroll-down-or-next-page (&optional amount)
  "Scroll down the current page or go to the next page if can't scroll.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll)))
    (reader-scroll-down amount)
    (when (and (= prev-scroll (window-vscroll))
	       (reader-next-page)) ; if succeeds
      (set-window-vscroll nil 0))))

(defun reader-scroll-up-screenful-or-prev-page (&optional amount)
  "Scroll up the current page by a screenful or go to the previous page if can't scroll.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (or amount (setq amount 1))
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (reader-scroll-up-or-prev-page scroll)))

(defun reader-scroll-down-screenful-or-next-page (&optional amount)
  "Scroll down the current page by a screenful or go to the next page if can't scroll.
Optionally specify the AMOUNT by which to scroll."
  (interactive "p")
  (or amount (setq amount 1))
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (reader-scroll-down-or-next-page scroll)))

(defun reader-kill-buffer ()
  "Kill the current buffer and the document."
  (interactive)
  (kill-buffer (current-buffer)))

(defun reader--get-current-doc-image-size ()
  "Get the size of the current page's doc image."
  (let* ((cdr-image (cdr (overlay-get reader-current-svg-overlay 'display)))
	 (width (plist-get cdr-image :width))
	 (length (plist-get cdr-image :length)))
    (cons width length)))

(defun reader--center-page (&optional window)
  "Centers the pages of the document with respect to the WINDOW in which the document is opened."
  (with-current-buffer (window-buffer window)
    (when (equal major-mode 'reader-mode)
      (let* ((window-width (window-body-width window))
	     (pixel-window-width (window-pixel-width))
	     (pixel-per-col (/ pixel-window-width
			       window-width))
	     (doc-image-width (car (reader--get-current-doc-image-size)))
	     (doc-fits-p (> pixel-window-width doc-image-width))
	     (raw-offset (/ (- pixel-window-width doc-image-width) 2))
	     (overlay-offset
	      `(space :width (,(if doc-fits-p raw-offset 0)))))
	(overlay-put reader-current-svg-overlay 'line-prefix overlay-offset)
	(when-let* (((not doc-fits-p)) ; scroll to the center of the doc
		    (scroll-offset
		     (round (/ (abs raw-offset) pixel-per-col))))
	  (set-window-hscroll window scroll-offset))))))

(defun reader--render-buffer ()
  "Render the document file this buffer is associated with.  It is to be called while a document’s
buffer is already opened and the buffer is not in `reader-mode'."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if file
	(progn
	  (reader-dyn--load-doc file))
      (message "No file associated with buffer."))))

;; Define the keymap for reader-mode
(defvar reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<next>") #'reader-next-page)
    (define-key map (kbd "J") #'reader-next-page)
    (define-key map (kbd "n") #'reader-next-page)

    (define-key map (kbd "j") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "C-n") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "<down>") #'reader-scroll-down-or-next-page)
    (define-key map (kbd "<wheel-down>") #'reader-scroll-down-or-next-page)

    (define-key map (kbd "C-v") #'reader-scroll-down-screenful)

    (define-key map (kbd "SPC") #'reader-scroll-down-screenful-or-next-page)

    (define-key map (kbd "<prior>") #'reader-previous-page)
    (define-key map (kbd "K") #'reader-previous-page)
    (define-key map (kbd "p") #'reader-previous-page)

    (define-key map (kbd "k") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "C-p") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "<up>") #'reader-scroll-up-or-prev-page)
    (define-key map (kbd "<wheel-up>") #'reader-scroll-up-or-prev-page)

    (define-key map (kbd "M-v") #'reader-scroll-up-screenful)

    (define-key map (kbd "DEL") #'reader-scroll-up-screenful-or-prev-page)
    (define-key map (kbd "S-SPC") #'reader-scroll-up-screenful-or-prev-page)

    (define-key map (kbd "h") #'reader-scroll-left)
    (define-key map (kbd "C-b") #'reader-scroll-left)

    (define-key map (kbd "l") #'reader-scroll-right)
    (define-key map (kbd "C-f") #'reader-scroll-right)

    (define-key map (kbd "gg") #'reader-first-page)
    (define-key map (kbd "M-<") #'reader-first-page)

    (define-key map (kbd "G") #'reader-last-page)
    (define-key map (kbd "M->") #'reader-last-page)

    (define-key map (kbd "M-g g") #'reader-goto-page)
    (define-key map (kbd "g n") #'reader-goto-page)

    (define-key map (kbd "=") #'reader-enlarge-size)

    (define-key map (kbd "-") #'reader-shrink-size)

    (define-key map (kbd "H") #'reader-fit-to-height)

    (define-key map (kbd "W") #'reader-fit-to-width)

    (define-key map (kbd "Q") #'reader-kill-buffer)
    map)
  "Keymap for reader-mode.")

;; Define the major mode
(define-derived-mode reader-mode special-mode "Emacs Reader"
  "Major mode for viewing documents in The Emacs Reader.

Keybindings:
\\{reader-mode-map}"
  :group 'reader
  (setq-local buffer-read-only t
	      global-linum-mode nil
	      cursor-type 'hollow
	      display-line-numbers-mode nil)
  (set-buffer-modified-p nil)
  (blink-cursor-mode 0)

  (setq-local bookmark-make-record-function
	      #'reader-bookmark-make-record)

  ;; Only do this when document is not already rendered
  (when (not reader-current-doc-render-status)
    (reader--render-buffer))

  (use-local-map reader-mode-map)
  (setq major-mode 'reader-mode)
  (setq mode-name "Emacs Reader")
  (run-hooks 'reader-mode-hook)

  ;; Initially fit the document to height
  (reader-fit-to-height)

  ;; Invoke centering every time window's size changes only in reader-mode windows
  (add-hook 'window-size-change-functions #'reader--center-page nil t))

;; Modeline for the reader-mode
(defun reader-mode-line ()
  "Set custom mode-line interface when reading documents."
  (setq-local mode-line-format
	      (list
	       "Page: "
	       '(:eval (number-to-string (+ 1 (reader-dyn--current-doc-pagenumber))))
	       "/"
	       '(:eval (number-to-string reader-current-doc-pagecount))
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
