;;; reader.el --- General-purpose Document Reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Divya Ranjan Pattanaik
;; Copyright (C) 2025  Tushar

;; Author: Divya Ranjan Pattanaik <divya@subvertising.org>
;; Keywords: lisp, files, tools
;; Version: 0.3.1
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
;; relies on MuPDF to do the rendering, and Emacs Lisp’s native image
;; displaying capabilities to view the rendered images and manipulate them.

;;; Code:

(require 'image)
(require 'image-mode)
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

(defcustom reader-default-fit 'reader-fit-to-height
  "The default fitting for documents."
  :group 'reader
  :type '(radio (function-item reader-fit-to-height)
		(function-item reader-fit-to-width)))

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

;; We queue some commands because the user is expected to use the
;; commands repeatedly, such as by simply spamming a key. If we don't
;; queue the commands, Emacs may start skipping intermediate commands.

(defvar reader--command-queue nil
  "Queue of reader commands to be executed sequentially.")

(defun reader--process-command-queue ()
  "Process the next command in the queue, ensuring UI updates."
  (when-let* ((cmd (pop reader--command-queue)))
    (funcall cmd)
    (redisplay)
    (run-with-idle-timer 0.01 nil #'reader--process-command-queue)))

(defun reader--enqueue-command (cmd args)
  "Add CMD with ARGS to the queue and start processing if needed."
  (push (apply #'apply-partially cmd args) reader--command-queue)
  (unless (or (active-minibuffer-window)
              (memq #'reader--process-command-queue post-command-hook))
    (add-hook 'post-command-hook #'reader--process-command-queue)))

(defmacro reader--define-queue-command (name arglist docstring interactive &rest body)
  "Define NAME as a reader command.
Also define a necessary non-queue function.

Much like `defun', except that ARGLIST, DOCSTRING, and INTERACTIVE are required.

The reader--non-queue-NAME function is simply defined as a function with body BODY.
The reader-NAME command is simply a wrapper around `reader--enqueue-command' with
reader--non-queue-NAME as the argument."
  (declare (indent defun)
	   (doc-string 3))
  (let* ((name (symbol-name name))
	 (non-queue-function-name (concat "reader--non-queue-" name))
	 (queue-function-name (concat "reader-" name)))
    `(progn

       (defun ,(intern non-queue-function-name) ,arglist
	 ,(format "%s

This is the actual function, see `%s' for the interactive version."
		  docstring queue-function-name)
	 ,(add-to-list 'body 'progn))

       (defun ,(intern queue-function-name) ,arglist
	 ,(format "%s

This is the queuing function, see `%s' for the actual definition."
		  docstring non-queue-function-name)
	 ,interactive
	 (reader--enqueue-command #',(intern non-queue-function-name)
				  ,(remq '&optional (add-to-list 'arglist 'list)))))))

;;;###autoload
(defun reader-open-doc (document)
  "Open DOCUMENT for viewing.

This function calls the module function `reader-dyn--load-doc' from the
dynamic module to render the first page and displays it in a new buffer.

The only formats that can be opened `reader-supported-formats', any
other file format will simply not show up as a candidate."
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

(reader--define-queue-command next-page ()
  "Go to the next page of the document."
  (interactive)
  (let ((status (reader-dyn--next-page)))
    (when status
      (force-mode-line-update t))
    status))

(reader--define-queue-command previous-page ()
  "Go to the previous page of the document."
  (interactive)
  (let ((status (reader-dyn--prev-page)))
    (when status
      (force-mode-line-update t))
    status))

(defun reader-first-page ()
  "Go to the first page of the document."
  (interactive)
  (reader-dyn--first-page)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-last-page ()
  "Go to the last page of the document."
  (interactive)
  (reader-dyn--last-page)
  (reader--center-page)
  (force-mode-line-update t))

(defun reader-goto-page (n)
  "Go to page number N in the current document."
  (interactive "nGoto page: ")
  (reader-dyn--goto-page (- n 1)) ; MuPDF does 0-indexing
  (reader--center-page))

(defun reader--get-current-doc-image-size ()
  "Get the dimensions of the current page's image."
  (let* ((cdr-image (cdr (overlay-get reader-current-doc-overlay 'display)))
	 (width (plist-get cdr-image :width))
	 (height (plist-get cdr-image :height)))
    (cons width height)))

(defun reader-doc-scale-page (factor)
  "Scales the page by a given FACTOR.

It also updates `reader-current-doc-scale-value' to reflect the new scale."
  (reader-dyn--scale-page factor)
  (setq reader-current-doc-scale-value factor))

(defun reader-enlarge-size (&optional scaling-factor)
  "Enlarge the size of the current page by the `reader-enlarge-factor'.

Optionally scale it by the SCALING-FACTOR."
  (interactive (list (float
		      (* reader-current-doc-scale-value reader-enlarge-factor))))
  (reader-doc-scale-page scaling-factor)
  (reader--center-page))

(defun reader-shrink-size (&optional scaling-factor)
  "Shrink the size of the current page by the `reader-shrink-factor'.

Optionally scale it by the SCALING-FACTOR."
  (interactive (list (float
		      (* reader-current-doc-scale-value reader-shrink-factor))))
  (reader-doc-scale-page scaling-factor)
  (reader--center-page))

(defun reader-reset-size ()
  "Reset the size of the current page to 1.0."
  (interactive)
  (reader-doc-scale-page 1.0)
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
    (reader--set-window-vscroll nil 0)))

(defun reader-fit-to-width ()
  "Scale the current page to fit its width perfectly within the window."
  (interactive)
  (let* ((image-width (car (reader--get-current-doc-image-size)))
	 (pixel-window-width (window-pixel-width))
	 (unscaled-width (/ image-width reader-current-doc-scale-value))
	 (scaling-factor (/ pixel-window-width unscaled-width)))
    (reader-doc-scale-page scaling-factor)
    (reader--center-page)))

(defun reader--get-pixel-per-col (&optional window)
  "Get the no of pixels per column for WINDOW."
  (/ (window-pixel-width window) (window-body-width window)))

;; We need to do this because scrolling is possible in one direction
;; (downwards) indefinitely.

(defun reader--set-window-vscroll (window vscroll &optional pixels-p)
  "Set amount by which WINDOW should be scrolled vertically to VSCROLL.

If setting VSCROLL makes the document page's top disappear, it sets the
maximum vertical scroll possible without doing that.

If PIXELS-P is non-nil, VSCROLL is considered to be in pixels.
Also see `set-window-vscroll'."
  (let* ((image-height (cdr (reader--get-current-doc-image-size)))
	 (pixel-window-height (window-pixel-height window))
	 (window-height (window-body-height window))
	 (pixel-per-col (/ pixel-window-height window-height))
	 (pixel-vscroll (* pixel-per-col vscroll))
	 (max-pixel-vscroll (- image-height pixel-window-height))
	 (corrected-pixel-vscroll (min pixel-vscroll max-pixel-vscroll))
	 (corrected-vscroll (/ corrected-pixel-vscroll pixel-per-col)))
    (set-window-vscroll window
			(if pixels-p corrected-pixel-vscroll corrected-vscroll)
			pixels-p)))

;; Most of the scrolling functions here exist because of our handling
;; of centering in `reader--center-page'.

(defun reader--get-prefix-width ()
  "Get the line prefix width set by `reader--center-page'."
  (car
   (plist-get
    (cdr (overlay-get reader-current-doc-overlay 'line-prefix))
    :width)))

(defun reader--right-most-window-hscroll (window)
  "Get the maximum horizontal scroll value for WINDOW.

This position is at the rightmost point."
  (let* ((image-width (car (reader--get-current-doc-image-size)))
	 (line-prefix-width (reader--get-prefix-width))
	 (pixel-window-width (window-pixel-width window))
	 (max-ncol (round (/ (max line-prefix-width
				  (- image-width pixel-window-width))
			     (reader--get-pixel-per-col window)))))
    max-ncol))

(defun reader--set-window-hscroll (window ncol &optional unconstrained)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.

If setting NCOL makes the document page disappear, it sets the maximum
horizontal scroll possible without doing that. If UNCONSTRAINED is
non-nil, it allows setting NCOL even if it makes the page disappear.

See also `set-window-hscroll'."
  (let* ((line-prefix-width (reader--get-prefix-width))
	 (pixel-per-col (reader--get-pixel-per-col window))
	 (calibrated-ncol (round (- (/ line-prefix-width pixel-per-col) ncol)))
	 (max-ncol (reader--right-most-window-hscroll window))
	 (ncol (if unconstrained
		   calibrated-ncol
		 (min calibrated-ncol max-ncol))))
    (set-window-hscroll window ncol)
    (reader--window-hscroll window)))

(defun reader--window-hscroll (window)
  "Return the number of columns by which WINDOW is scrolled from left margin.
WINDOW must be a live window and defaults to the selected one.

This correctly handles the prefix width set by reader documents and does
not return the actual horizontal scroll value; for that, see
`window-hscroll'."
  (let* ((line-prefix-width (reader--get-prefix-width))
	 (pixel-per-col (reader--get-pixel-per-col window))
	 (hscroll (round (- (/ line-prefix-width pixel-per-col) (window-hscroll window)))))
    hscroll))

;; We need this hack involving line-prefix because Emacs' scrolling is
;; idiosyncratic, and doesn't allow arbitrary scrolling in every
;; direction.
(defun reader--center-page (&optional window)
  "Center the document with respect to WINDOW.

If WINDOW is omitted defaults to current window."
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'reader-mode)
      (let* ((windows (get-buffer-window-list))
	     (max-window-width
	      (apply #'max (mapcar (lambda (window) (window-body-width window t)) windows)))
	     (doc-image-width (car (reader--get-current-doc-image-size)))
	     (max-left-offset (max 0 (- max-window-width doc-image-width)))
	     (overlay-offset `(space :width (,max-left-offset))))
	;; Add prefix so that the page is at the leftmost point of the widest window.
	(overlay-put reader-current-doc-overlay 'line-prefix overlay-offset)
	;; scroll every window back to the center of the doc
	(mapcar (lambda (window)
		  (let* ((pixel-window-width (window-pixel-width window))
			 (pixel-per-col (reader--get-pixel-per-col window))
			 (doc-left-offset (- pixel-window-width doc-image-width))
			 (doc-center-offset (/ doc-left-offset 2))
			 (scroll-offset (round (/ doc-center-offset pixel-per-col))))
		    (reader--set-window-hscroll window scroll-offset t)))
		windows)))))

(defun reader-scroll-up (&optional amount window)
  "Scroll up the current page by AMOUNT (1 by default).

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll window))
	 (vscroll (- prev-scroll amount)))
    (- prev-scroll (reader--set-window-vscroll window vscroll))))

(defun reader-scroll-down (&optional amount window)
  "Scroll down the current page by AMOUNT (1 by default).

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll window))
	 (vscroll (+ prev-scroll amount)))
    (- (reader--set-window-vscroll window vscroll) prev-scroll)))

(defun reader-scroll-up-screenful (&optional window)
  "Scroll up the current page by a screenful.

Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (let ((amount (- (window-body-height window)
		   next-screen-context-lines)))
    (when (= 0 (reader-scroll-up amount window))
      (message "Beginning of page"))))

(defun reader-scroll-down-screenful (&optional window)
  "Scroll down the current page by a screenful.

Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (let ((amount (- (window-body-height window)
		   next-screen-context-lines)))
    (when (= 0 (reader-scroll-down amount window))
      (message "End of page"))))

(defun reader-scroll-left (&optional amount window)
  "Scroll to the left of the current page by AMOUNT (or 1).

Only scrolls when the document page width is larger then the window width.
Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* (((< (window-pixel-width) (car (reader--get-current-doc-image-size))))
	      (prev-scroll (reader--window-hscroll window))
	      (hscroll (+ prev-scroll amount)))
    (- (reader--set-window-hscroll window hscroll) prev-scroll)))

(defun reader-scroll-right (&optional amount window)
  "Scroll to the right of the current page by AMOUNT (or 1).

Only scrolls when the document page width is larger then the window width.
Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* (((< (window-pixel-width) (car (reader--get-current-doc-image-size))))
	      (prev-scroll (reader--window-hscroll window))
	      (hscroll (- prev-scroll amount)))
    (- prev-scroll (reader--set-window-hscroll window hscroll))))

(defun reader-scroll-left-most (&optional window)
  "Scroll to the left most point of the current page.

Only scrolls when the document page width is larger then the window width.
Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (when (< (window-pixel-width) (car (reader--get-current-doc-image-size)))
    (reader--set-window-hscroll window 0)))

(defun reader-scroll-right-most (&optional window)
  "Scroll to the right most point of the current page.

Only scrolls when the document page width is larger then the window width.
Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (when (< (window-pixel-width) (car (reader--get-current-doc-image-size)))
    ;; We use `set-window-hscroll' here because we need to go the right
    ;; most point directly, bypassing `'reader--set-window-hscroll' checks.
    (set-window-hscroll window (reader--right-most-window-hscroll window))))

(reader--define-queue-command scroll-up-or-prev-page (&optional amount window)
  "Scroll up the current page by AMOUNT (or 1), otherwise switch to the previous page.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* (((and (= 0 (reader-scroll-up amount))
		    (reader--non-queue-previous-page))) ; if succeeds
	      (image-height (cdr (reader--get-current-doc-image-size)))
	      (pixel-window-height (window-pixel-height window))
	      (bottom-most-scroll-pixel
	       (- image-height pixel-window-height)))
    (reader--set-window-vscroll window bottom-most-scroll-pixel t)))

(reader--define-queue-command scroll-down-or-next-page (&optional amount window)
  "Scroll down the current page by AMOUNT (or 1), otherwise switch to the next page.

Optionally specify the WINDOW, defaults to current window."
  (interactive "p")
  (or amount (setq amount 1))
  (when (and (= 0 (reader-scroll-down amount window))
	     (reader--non-queue-next-page)) ; if succeeds
    (reader--set-window-vscroll window 0)))

(reader--define-queue-command scroll-up-screenful-or-prev-page (&optional window)
  "Scroll up the current page by screenful, otherwise switch to the previous page.

Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (let ((scroll (- (window-body-height window)
		   next-screen-context-lines)))
    (reader--non-queue-scroll-up-or-prev-page scroll window)))

(reader--define-queue-command scroll-down-screenful-or-next-page (&optional window)
  "Scroll down the current page by screenful, otherwise switch to the next page.

Optionally specify the WINDOW, defaults to current window."
  (interactive)
  (let ((scroll (- (window-body-height window)
		   next-screen-context-lines)))
    (reader--non-queue-scroll-down-or-next-page scroll window)))

(reader--define-queue-command mwheel-scroll-up (event)
  "Scroll up or switch to the previous page, but also handle mouse EVENT.

See also `reader-non-queue-scroll-up-or-prev-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-up 1)
		   ('double-wheel-up 2)
		   ('triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader--non-queue-scroll-up-or-prev-page amount scrolled-window))))

(reader--define-queue-command mwheel-scroll-down (event)
  "Scroll down or switch to the next page, but also handle mouse EVENT.

See also `reader--non-queue-scroll-down-or-next-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-down 1)
		   ('double-wheel-down 2)
		   ('triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader--non-queue-scroll-down-or-next-page amount scrolled-window))))

(defun reader-mwheel-scroll-left (event)
  "Scroll to the left, but also handle mouse EVENT.

See also `reader-scroll-left'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('S-wheel-up 1)
		   ('S-double-wheel-up 2)
		   ('S-triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader-scroll-left amount scrolled-window))))

(defun reader-mwheel-scroll-right (event)
  "Scroll to the right, but also handle mouse EVENT.

See also `reader-scroll-right'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('S-wheel-down 1)
		   ('S-double-wheel-down 2)
		   ('S-triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader-scroll-right amount scrolled-window))))

(defun reader-mwheel-enlarge-size (event)
  "Enlarge the current page, but also handle mouse EVENT.

See also `reader-enlarge-size'."
  (interactive "e")
  (let* ((event-type (car event))
	 (scaling-factor (pcase event-type
			   ('C-wheel-up reader-enlarge-factor)
			   ('C-double-wheel-up (+ reader-enlarge-factor 0.1))
			   ('C-triple-wheel-up (+ reader-enlarge-factor 0.2))))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader-enlarge-size
       (* scaling-factor reader-current-doc-scale-value)))))

(defun reader-mwheel-shrink-size (event)
  "Shrink the current page, but also handle mouse EVENT.

See also `reader-shrink-size'."
  (interactive "e")
  (let* ((event-type (car event))
	 (scaling-factor (pcase event-type
			   ('C-wheel-down reader-shrink-factor)
			   ('C-double-wheel-down (- reader-shrink-factor 0.1))
			   ('C-triple-wheel-down (- reader-shrink-factor 0.2))))
	 (scrolled-window (car (cadr event))))
    (with-current-buffer (window-buffer scrolled-window)
      (reader-shrink-size
       (* scaling-factor reader-current-doc-scale-value)))))

(defun reader-rotate-clockwise ()
  "Rotate all pages of the current document by 90 degrees, clockwise."
  (interactive)
  (reader-dyn--rotate-doc 90)
  (reader--center-page))

(defun reader-rotate-counter-clockwise ()
  "Rotate all pages of the current document by 90 degrees, counter-clockwise."
  (interactive)
  (reader-dyn--rotate-doc -90)
  (reader--center-page))

(defun reader--clear-doc-memory ()
  "Clear document memory if we are in `reader-mode'.

Intended for use in `kill-buffer-hook'."
  (when (derived-mode-p 'reader-mode)
    (reader-dyn--close-doc)))
(add-hook 'kill-buffer-hook 'reader--clear-doc-memory)

(defun reader-close-doc ()
  "Close the current document, also prompt the user for confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to close the current document?")
    (kill-buffer (current-buffer))))

(defun reader-refresh-doc-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Refresh the buffer document with file on disk.

It reloads the entire document while preserving the previous state.
The args IGNORE-AUTO, NOCONFIRM and PRESERVE-MODES are ignored and are
simply to satisfy the template for `revert-buffer-function'.
This function is replaced as `revert-buffer-function' for `reader-mode' buffers."
  (interactive)
  (when buffer-file-name
    (let ((page (reader-current-pagenumber))
	  (scale reader-current-doc-scale-value)
	  (theme reader-current-doc-theme))
      (remove-overlays)
      (reader--render-buffer)
      (if reader-dark-mode
	(reader-dark-mode 1))
      (reader-doc-scale-page scale)
      (reader-goto-page page)
      (reader--center-page))))

(defun reader--render-buffer ()
  "Render the document file current buffer is associated with.

It is to be called while a document’s buffer is already opened and the
buffer is not in `reader-mode'."
  (interactive)
  (if-let* ((file (buffer-file-name (current-buffer))))
      (reader-dyn--load-doc file)
    (message "No file associated with buffer.")))

;; We explicitly bind default keys because relying on the rebinding
;; mechanism can lead to them not being bound at all if users have
;; overridden the defaults with other commands.
(defvar-keymap reader-mode-map
  :doc "Keymap for `reader-mode'."
  "n"       #'reader-next-page
  "p"       #'reader-previous-page

  "C-p"     #'reader-scroll-up-or-prev-page
  "C-n"     #'reader-scroll-down-or-next-page
  "<remap> <previous-line>" #'reader-scroll-up-or-prev-page
  "<remap> <next-line>" #'reader-scroll-down-or-next-page
  "<remap> <next>" #'reader-scroll-down-or-next-page
  "<remap> <prior>" #'reader-scroll-up-or-prev-page

  "<wheel-up>" #'reader-mwheel-scroll-up
  "<wheel-down>" #'reader-mwheel-scroll-down

  "S-<wheel-up>" #'reader-mwheel-scroll-left
  "S-<wheel-down>" #'reader-mwheel-scroll-right

  "C-v"      #'reader-scroll-down-screenful
  "M-v"      #'reader-scroll-up-screenful
  "<remap> <scroll-down-command>" #'reader-scroll-up-screenful
  "<remap> <scroll-up-command>" #'reader-scroll-down-screenful

  "SPC"     #'reader-scroll-down-screenful-or-next-page
  "DEL"     #'reader-scroll-up-screenful-or-prev-page
  "S-SPC"   #'reader-scroll-up-screenful-or-prev-page

  "C-f"     #'reader-scroll-right
  "C-b"     #'reader-scroll-left
  "<remap> <forward-char>" #'reader-scroll-right
  "<remap> <backward-char>" #'reader-scroll-left

  "C-e"     #'reader-scroll-right-most
  "C-a"     #'reader-scroll-left-most
  "<remap> <move-end-of-line>" #'reader-scroll-right-most
  "<remap> <move-beginning-of-line>" #'reader-scroll-left-most

  "M-<" #'reader-first-page
  "M->" #'reader-last-page
  "<remap> <beginning-of-buffer>" #'reader-first-page
  "<remap> <end-of-buffer>" #'reader-last-page

  "M-g g"   #'reader-goto-page
  "M-g M-g" #'reader-goto-page
  "<remap> <goto-line>"   #'reader-goto-page

  "="       #'reader-enlarge-size
  "+"       #'reader-enlarge-size
  "C-<wheel-up>" #'reader-mwheel-enlarge-size

  "-"       #'reader-shrink-size
  "C-<wheel-down>" #'reader-mwheel-shrink-size

  "0"       #'reader-reset-size

  "H"       #'reader-fit-to-height
  "W"       #'reader-fit-to-width

  "r"       #'reader-rotate-clockwise
  "R"       #'reader-rotate-counter-clockwise

  "<f5>"    #'reader-presentation-mode

  "o"       #'reader-outline-show

  "Q"       #'reader-close-doc)

;;;###autoload
(define-derived-mode reader-mode special-mode "Emacs Reader"
  "Major mode for viewing documents in The Emacs Reader.

Keybindings:
\\{reader-mode-map}"
  :group 'reader
  (setq-local buffer-read-only t
	      global-linum-mode nil
	      cursor-type 'hollow
	      display-line-numbers-mode nil
	      ;; messes up centering(line-prefix) otherwise, the
	      ;; default value is 8, so we keep it at that.
	      left-fringe-width 8)
  (set-buffer-modified-p nil)
  (blink-cursor-mode 0)
  (auto-revert-mode 1)
  (setq-local revert-buffer-function #'reader-refresh-doc-buffer)

  ;; Disable `pixel-scroll-precision-mode' locally because it doesn't
  ;; work nicely with `reader-mode'.
  (when (bound-and-true-p pixel-scroll-precision-mode)
    (setq-local pixel-scroll-precision-mode nil))

  (setq-local bookmark-make-record-function
	      #'reader-bookmark-make-record)

  (unless reader-current-doc-render-status
    (reader--render-buffer))

  (setq-local imenu-create-index-function #'reader--outline-imenu-create-index
	      imenu-default-goto-function #'reader--outline-imenu-goto
	      imenu-submenus-on-top nil
	      imenu-sort-function nil
              imenu-auto-rescan t)

  (use-local-map reader-mode-map)
  (setq major-mode 'reader-mode)
  (setq mode-name "Emacs Reader")
  (run-hooks 'reader-mode-hook)

  (funcall reader-default-fit)
  (add-hook 'window-size-change-functions #'reader--center-page nil t))

(defun reader-mode-line ()
  "Set custom mode-line interface when reading documents."
  (setq-local mode-line-position
              '(" P" (:eval (number-to-string (reader-current-pagenumber)))
                "/" (:eval (number-to-string reader-current-doc-pagecount)))))

(add-hook 'reader-mode-hook #'reader-mode-line)
(add-hook 'reader-mode-hook #'imenu-add-menubar-index)

(define-minor-mode reader-dark-mode
  "Toggle dark-mode for current reader document."
  :lighter " Dark"
  (when (eq major-mode 'reader-mode)
    (if reader-dark-mode
	(reader-dyn--set-dark-theme)
      (reader-dyn--redisplay-doc))
    (reader-doc-scale-page reader-current-doc-scale-value)))

(define-globalized-minor-mode reader-global-dark-mode reader-dark-mode reader-dark-mode)

(define-minor-mode reader-presentation-mode
  "Toggle presentation view for current reader document."
  :lighter ""
  (cond (reader-presentation-mode
	 (setq-local mode-line-format nil)
	 (reader-fit-to-height))
	((kill-local-variable 'mode-line-format))))

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
