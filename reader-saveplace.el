;;; reader-saveplace.el --- Saveplace integration with emacs-reader  -*- lexical-binding: t; -*-

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

(require 'bookmark)
(require 'saveplace)
(require 'reader-bookmark)

(defconst reader--saveplace-key 'reader-bookmark
  "Key under which reader-mode stores its bookmark record.")

(defun saveplace--reader-find-file (orig-fun &rest args)
  "Advice around `save-place-find-file'.
Restores the saved place for `reader-mode' buffers or falls back to ORIG-FUN."
  (or save-place-loaded (save-place-load-alist-from-file))
  (if (derived-mode-p 'reader-mode)
      (if-let* ((place (assoc buffer-file-name save-place-alist))
		(bookmark (cadr place)))
            (reader-bookmark-jump bookmark))
    (apply orig-fun args)))


(defun saveplace--reader-to-alist (orig-fun &rest args)
  "Advice around `save-place-to-alist'.
Saves the place for `reader-mode' buffers or falls back to ORIG-FUN."
  (if (derived-mode-p 'reader-mode)
      (let* ((filename buffer-file-name)
             (bookmark-record (reader-bookmark-make-record))
	     (bookmark (cons "reader-saveplace" bookmark-record)))
        (when filename
          (setq save-place-alist
                (assq-delete-all filename save-place-alist))
          (setq save-place-alist
		(add-to-list 'save-place-alist
                      `(,filename . (,bookmark))))))
    (apply orig-fun args)))

(advice-add 'save-place-find-file-hook  :around #'saveplace--reader-find-file)
(advice-add 'save-place-to-alist   :around #'saveplace--reader-to-alist)

(provide 'reader-saveplace)
;;; reader-saveplace.el ends here
