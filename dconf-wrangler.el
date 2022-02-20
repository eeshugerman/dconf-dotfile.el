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

(defvar dconf-wrangler--frame nil)
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

(defun dconf-wrangler--init (source-type)
  (let* ((frame (make-frame))
         (target-window (frame-root-window frame))
         (source-window (split-window target-window nil 'right))
         (target-buffer (find-file dconf-wrangler-dconf-config-file-path))
         (source-buffer (get-buffer-create (format "*dconf-wrangler-%s*" source-type))))

    (setq dconf-wrangler--frame frame)
    (setq dconf-wrangler--source-buffer source-buffer)
    (setq dconf-wrangler--target-buffer target-buffer)

    (set-window-buffer target-window target-buffer)
    (set-window-buffer source-window source-buffer)

    (with-current-buffer target-buffer (conf-toml-mode))))

(defun dconf-wrangler-dump ()
  "dconf dump"
  (interactive)
  (dconf-wrangler--init "dump")
  (let ((command (format "dconf dump %s" dconf-wrangler-dump-base-schema))
        (source-buffer dconf-wrangler--source-buffer))
    (shell-command command source-buffer "*Messages*")
    (with-current-buffer source-buffer (dconf-wrangler-dump-mode))))


(defun dconf-wrangler-quit ()
  (interactive)
  ;; todo: prompt to save if there are changes
  (kill-buffer dconf-wrangler--target-buffer)
  (kill-buffer dconf-wrangler--source-buffer)
  (delete-frame dconf-wrangler--frame))

(provide 'dconf-wrangler)

;;; dconf-wrangler.el ends here
