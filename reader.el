;;; reader.el --- General-purpose Document Reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Divya Ranjan Pattanaik
;; Copyright (C) 2025  Tushar

;; Author: Divya Ranjan Pattanaik <divya@subvertising.org>
;; Keywords: lisp, files, tools
;; Version: 0.2.6
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

(defvar-local reader-current-doc-scale-value 1.0
  "The amount of scaling for the current document. Defaults to 1.0.")

;; Setting of `auto-mode-list' fails if not autoloaded.
;;;###autoload
(defconst reader-supported-formats (list "pdf" "epub" "mobi"
					 "fb2" "xps" "cbz"
					 "docx""pptx" "xlsx"
					 "odt" "ods" "odp" "odg")
  "File formats supported by the document reader.")

(defun reader-current-pagenumber ()
  "The current page number of the document."
  (1+ (reader-dyn--current-doc-pagenumber)))

;;;###autoload
(defun reader-open-doc (document)
  "Open DOCUMENT for viewing.

This function calls the module function `reader-dyn--load-doc' from the dynamic module
to render the first page and displays it in a new buffer.

The only files that can be opened are of the following formats:

- PDF
- EPUB
- MOBI
- FB2
- XPS/OpenXPS
- CBZ
- DOCX/PPTX/XLSX
- ODT/ODS/ODP/ODG

Any other file format will simply not show up as a candidate."
  (interactive (let* ((regexp (concat "\\." (regexp-opt reader-supported-formats t) "$"))
		      (file (read-file-name
			     "Open document: "
			     nil nil t nil
			     (lambda (f)
			       (or (file-directory-p f)
				   (string-match-p regexp f))))))
		 (list file)))
  (switch-to-buffer (create-file-buffer document))
  (insert "\n")
  (reader-dyn--load-doc (expand-file-name document))
  (reader-mode))

(defun reader-next-page ()
  "Go to the next page of the document."
  (interactive)
  (let ((status (reader-dyn--next-page)))
    (when status
      (reader-doc-scale-page reader-current-doc-scale-value)
      (reader--center-page)
      (force-mode-line-update t))
    status))

(defun reader-previous-page ()
  "Go to the previous page of the document."
  (interactive)
  (let ((status (reader-dyn--prev-page)))
    (when status
      (reader-doc-scale-page reader-current-doc-scale-value)
      (reader--center-page)
      (force-mode-line-update t))
    status))

(defun reader-first-page ()
  "Go to the first page of the document."
  (interactive)
  (reader-dyn--first-page)
  (reader-doc-scale-page reader-current-doc-scale-value)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-last-page ()
  "Go to the last page of the document."
  (interactive)
  (reader-dyn--last-page)
  (reader-doc-scale-page reader-current-doc-scale-value)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-goto-page (n)
  "Go to page number 'N' in the current document."
  (interactive "nGoto page: ")
  (reader-dyn--goto-page (- n 1)) ; MuPDF does 0-indexing
  (reader-doc-scale-page reader-current-doc-scale-value)
  (reader--center-page))

(defun reader-enlarge-size ()
  "Enlarge the size of the current page by the `reader-enlarge-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale-value reader-enlarge-factor)))
    (reader-doc-scale-page scaling-factor))
  (reader--center-page))

(defun reader-shrink-size ()
  "Shrink the size of the current page by the `reader-shrink-factor'."
  (interactive)
  (let ((scaling-factor (* reader-current-doc-scale-value reader-shrink-factor)))
    (reader-doc-scale-page scaling-factor))
  (reader--center-page))

(defun reader-fit-to-height ()
  "Scale the current page to fit its height perfectly within the window."
  (interactive)
  (let* ((image-height (cdr (reader--get-current-doc-image-size)))
	 (pixel-window-height (window-pixel-height))
	 (unscaled-height (/ image-height reader-current-doc-scale-value))
	 (scaling-factor (/ pixel-window-height unscaled-height)))
    (reader-doc-scale-page scaling-factor)
    (reader--center-page)
    (set-window-vscroll nil 0)))

(defun reader-fit-to-width ()
  "Scale the current page to fit its width perfectly within the window."
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
sets the `:width', `:height', and `:scale' properties of current page’s image.

It also updates `reader-current-doc-scale-value' to reflect the new scale."
  (reader-dyn--scale-page factor)
  (setq reader-current-doc-scale-value factor))

(defun reader-scroll-up (&optional amount window)
  "Scroll up the current page by AMOUNT (1 by default).

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (let* ((vscroll
	  (max (- (window-vscroll window) amount) 0)))
    (set-window-vscroll window vscroll)))

(defun reader-possible-scroll-down (&optional amount window)
  "Return 1 (or AMOUNT) if that scroll is possible, otherwise return the max possible.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((image-height (cdr (reader--get-current-doc-image-size)))
	 (window-height (window-body-height window))
	 (pixel-window-height (window-pixel-height window))
	 (pixel-per-col (/ pixel-window-height
			   window-height))
	 (pixel-amount (* pixel-per-col amount))
	 (pixel-current-scroll (window-vscroll window t))
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

(defun reader-scroll-down (&optional amount window)
  "Scroll down the current page by AMOUNT (1 by default).

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((amount (reader-possible-scroll-down amount window))
	 (vscroll (+ (window-vscroll window) amount)))
    (set-window-vscroll window vscroll)))

(defun reader-scroll-up-screenful (&optional window)
  "Scroll up the current page by a screenful.

Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (let ((prev-scroll (window-vscroll window))
	(amount (- (window-body-height window)
		   next-screen-context-lines)))
    (when (= prev-scroll
	     (reader-scroll-up amount window))
      (message "Beginning of page"))))

(defun reader-scroll-down-screenful (&optional window)
  "Scroll down the current page by a screenful.

Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (let ((prev-scroll (window-vscroll window))
	(amount (- (window-body-height window)
		   next-screen-context-lines)))
    (when (= prev-scroll
	     (reader-scroll-down amount window))
      (message "End of page"))))

(defun reader-scroll-left (&optional amount window)
  "Scroll to the left of the current page by AMOUNT (or 1).

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (set-window-hscroll window
		      (- (window-hscroll window) amount)))

(defun reader-scroll-right (&optional amount window)
  "Scroll to the left of the current page by AMOUNT (or 1).

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (set-window-hscroll window
		      (+ amount (window-hscroll window))))

(defun reader-scroll-up-or-prev-page (&optional amount window)
  "Scroll up the current page by AMOUNT (or 1), otherwise switch to the previous page.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let ((prev-scroll (window-vscroll window)))
    (reader-scroll-up amount)
    (when-let* (((and (= prev-scroll (window-vscroll window))
		      (reader-previous-page))) ; if succeeds
		(image-height (cdr (reader--get-current-doc-image-size)))
		(pixel-window-height (window-pixel-height window))
		(bottom-most-scroll-pixel
		 (- image-height pixel-window-height)))
      (set-window-vscroll window bottom-most-scroll-pixel t))))

(defun reader-scroll-down-or-next-page (&optional amount window)
  "Scroll down the current page by AMOUNT (or 1), otherwise switch to the next page.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let ((prev-scroll (window-vscroll window)))
    (reader-scroll-down amount window)
    (when (and (= prev-scroll (window-vscroll window))
	       (reader-next-page)) ; if succeeds
      (set-window-vscroll window 0))))

(defun reader-scroll-up-screenful-or-prev-page (&optional amount window)
  "Scroll up the current page by screenful, otherwise switch to the previous page.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let ((scroll (- (window-body-height window)
		   next-screen-context-lines)))
    (reader-scroll-up-or-prev-page scroll window)))

(defun reader-scroll-down-screenful-or-next-page (&optional amount window)
  "Scroll down the current page by screenful, otherwise switch to the next page.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let ((scroll (- (window-body-height window)
		   next-screen-context-lines)))
    (reader-scroll-down-or-next-page scroll window)))

(defun reader-mwheel-scroll-up (event)
  "Scroll up or switch to the previous page, but also handle mouse EVENT.

See also `reader-scroll-up-or-prev-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-up 1)
		   ('double-wheel-up 2)
		   ('triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader-scroll-up-or-prev-page amount scrolled-window))))

(defun reader-mwheel-scroll-down (event)
  "Scroll down or switch to the next page, but also handle mouse EVENT.

See also `reader-scroll-down-or-next-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-down 1)
		   ('double-wheel-down 2)
		   ('triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader-scroll-down-or-next-page amount scrolled-window))))

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
  "Center the document with respect to WINDOW.

If WINDOW is omitted defaults to current window."
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'reader-mode)
      (let* ((windows (get-buffer-window-list))
	     (max-window-width
	      (apply #'max (mapcar (lambda (window) (window-body-width window t)) windows)))
	     (doc-image-width (car (reader--get-current-doc-image-size)))
	     (max-left-offset (- max-window-width doc-image-width))
	     (max-left-offset (if (< 0 max-left-offset)
				  max-left-offset 0))
	     (overlay-offset `(space :width (,max-left-offset))))
	;; Add prefix until the page is at the leftmost point of the widest window.
	(overlay-put reader-current-svg-overlay 'line-prefix overlay-offset)
	;; scroll every window back to the center of the doc
	(mapcar (lambda (window)
		  (let* ((window-width (window-body-width window))
			 (pixel-window-width (window-pixel-width window))
			 (pixel-per-col (/ pixel-window-width
					   window-width))
			 (doc-left-offset (- pixel-window-width doc-image-width))
			 (window-offset (- max-left-offset doc-left-offset))
			 (doc-center-offset (/ doc-left-offset 2))
			 (scroll-offset (round (abs (/ (+ doc-center-offset window-offset)
						       pixel-per-col)))))
		    (set-window-hscroll window scroll-offset)))
		windows)))))

(defun reader--render-buffer ()
  "Render the document file current buffer is associated with.

It is to be called while a document’s buffer is already opened and the
buffer is not in `reader-mode'."
  (interactive)
  (if-let* ((file (buffer-file-name (current-buffer))))
      (reader-dyn--load-doc file)
    (message "No file associated with buffer.")))

(defvar-keymap reader-mode-map
  :doc "Keymap for `reader-mode'."
  "n"       #'reader-next-page

  "p"       #'reader-previous-page

  "<remap> <previous-line>" #'reader-scroll-up-or-prev-page
  "<remap> <next-line>" #'reader-scroll-down-or-next-page
  "<remap> <next>" #'reader-scroll-down-or-next-page
  "<remap> <prior>" #'reader-scroll-down-or-next-page

  "<wheel-up>" #'reader-mwheel-scroll-up
  "<wheel-down>" #'reader-mwheel-scroll-down

  "<remap> <scroll-down-command>" #'reader-scroll-up-screenful
  "<remap> <scroll-up-command>" #'reader-scroll-down-screenful

  "SPC"     #'reader-scroll-down-screenful-or-next-page
  "DEL"     #'reader-scroll-up-screenful-or-prev-page
  "S-SPC"   #'reader-scroll-up-screenful-or-prev-page

  "<remap> <forward-char>" #'reader-scroll-right
  "<remap> <backward-char>"  #'reader-scroll-left

  "<remap> <beginning-of-buffer>" #'reader-first-page
  "<remap> <end-of-buffer>" #'reader-last-page

  "<remap> <goto-line>"   #'reader-goto-page

  "="       #'reader-enlarge-size
  "+"       #'reader-enlarge-size
  "C-<wheel-up>" #'reader-enlarge-size

  "-"       #'reader-shrink-size
  "C-<wheel-down>" #'reader-shrink-size

  "H"       #'reader-fit-to-height
  "W"       #'reader-fit-to-width

  "Q"       #'reader-kill-buffer)

;;;###autoload
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

  (unless reader-current-doc-render-status
    (reader--render-buffer))

  (use-local-map reader-mode-map)
  (setq major-mode 'reader-mode)
  (setq mode-name "Emacs Reader")
  (run-hooks 'reader-mode-hook)

  (reader-fit-to-height)

  (add-hook 'window-size-change-functions #'reader--center-page nil t))

(defun reader-mode-line ()
  "Set custom mode-line interface when reading documents."
  (setq-local mode-line-position
              '(" P" (:eval (number-to-string (reader-current-pagenumber)))
                "/" (:eval (number-to-string reader-current-doc-pagecount)))))

(add-hook 'reader-mode-hook #'reader-mode-line)

(define-minor-mode reader-dark-mode
  "Toggle dark-mode for current reader document."
  :lighter " Dark"
  (if reader-dark-mode
      (reader-dyn--set-doc-theme "white" "black")
    (reader-dyn--set-doc-theme "black" "white"))
  (reader-doc-scale-page reader-current-doc-scale-value))

;; see `reader-saveplace' for details.
;;;###autoload
(advice-add 'save-place-find-file-hook :around #'reader--saveplace-find-file)

;;;###autoload
(advice-add 'save-place-to-alist :around #'reader--saveplace-to-alist)

;;;###autoload
(dolist (pattern reader-supported-formats)
  (add-to-list 'auto-mode-alist (cons (concat "\\." pattern "\\'") 'reader-mode)))

(provide 'reader)
;;; reader.el ends here.
