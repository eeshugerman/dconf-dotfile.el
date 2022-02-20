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
  :group 'dconf-wrangler)

(defcustom dconf-wrangler-dconf-config-file-path (f-join (or (getenv "XDG_CONFIG_HOME")
                                                             (expand-file-name "~/.config"))
                                                         "dconf-user.conf")
  "Path to dconf config file to operate on")
(defvar dconf-wrangler--source-buffer nil)

(defvar dconf-wrangler--target-buffer nil)

(define-derived-mode dconf-wrangler-dump-mode special-mode "dconf dump"
  "Mode for browsing output of `dconf dump`"
  :group 'dconf-wrangler
  ;; doesn't seem to matter for syntax highlighting but maybe does for other stuff?
  :syntax-table conf-toml-mode-syntax-table
  ;; use conf-toml-mode's syntax highlighting
  (conf-mode-initialize "#" 'conf-toml-font-lock-keywords)
  (read-only-mode +1))

(defun dconf-wrangler--init-target ()
  (let ((target-buffer (get-buffer-create "*dconf-wrangler-target*")))
    (setq dconf-wrangler--target-buffer target-buffer)
    (with-current-buffer-window target-buffer nil nil
      (insert-file-contents dconf-wrangler-dconf-config-file-path)
      (conf-toml-mode)))
  )

(defun dconf-wrangler-dump ()
  "dconf dump"
  (interactive)
  (let ((source-buffer (get-buffer-create "*dconf-wrangler-dump*"))
        (command (format "dconf dump %s" dconf-wrangler-dump-base-schema)))
    (setq dconf-wrangler--source-buffer source-buffer)
    (with-current-buffer-window source-buffer nil nil
      (shell-command command source-buffer "*Messages*")
      (dconf-wrangler-dump-mode)))
  (dconf-wrangler--init-target))

(provide 'dconf-wrangler)

;;; dconf-wrangler.el ends here
