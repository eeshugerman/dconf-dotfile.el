;;; dconf-wrangler.el --- Plain text dconf management tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  elliott

;; Author: elliott<eeshugerman@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience, data, tools, maint

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

;;

;;; Code:


(defgroup dconf-wrangler nil
  "Plain text dconf management tool")

(defcustom dconf-wrangler-dump-base-schema "/"
  "Base dconf schema to operate on"
  :type 'string
  :group 'dconf)

(defvar dconf-wrangler-dump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'kill-current-buffer) ;; conflict w/ evil
    map))

(define-derived-mode dconf-wrangler-dump-mode conf-mode "dconf dump"
  (read-only-mode +1)
  (use-local-map dconf-wrangler-dump-mode-map))


(defun dconf-wrangler-dump ()
  "dconf dump"
  (interactive)
  (let ((buffer (get-buffer-create "*dconf-wrangler-dump*"))
        (command (format "dconf dump %s" dconf-wrangler-dump-base-schema)))
    (with-temp-buffer buffer
                      (shell-command command buffer "*Messages*")
                      (pop-to-buffer buffer)
                      (dconf-wrangler-dump-mode))))

(provide 'dconf-wrangler)

;;; dconf-wrangler.el ends here
