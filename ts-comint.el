;;; ts-comint.el --- Run a JavaScript interpreter in an inferior process window.

;;; Copyright (C) 2008 Paul Huff
;;; Copyright (C) 2015 Stefano Mazzucco
;;; Copyright (C) 2016 Jostein Kjønigsen

;;; Author: Paul Huff <paul.huff@gmail.com>, Stefano Mazzucco <MY FIRST NAME - AT - CURSO - DOT - RE>
;;; Created: 28 September 2016
;;; Version: 0.0.1
;;; URL: https://github.com/josteink/ts-comint
;;; Package-Requires: ()
;;; Keywords: javascript, node, inferior-mode, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; ts-comint.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; at your option any later version.

;; ts-comint.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; ts-comint.el is a comint mode for Emacs which allows you to run a
;; compatible typescript repl like Tsun inside Emacs.
;; It also defines a few functions for sending typescript input to it
;; quickly.

;; Usage:
;;  Put ts-comint.el in your load path
;;  Add (require 'ts-comint) to your .emacs or ~/.emacs.d/init.el
;;
;;  Do: `M-x run-ts'
;;  Away you go.

;;  You can add  the following couple of lines to your .emacs to take advantage of
;;  cool keybindings for sending things to the javascript interpreter inside
;;  of Steve Yegge's most excellent js2-mode.
;;
;;   (add-hook 'typescript-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
;;               (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
;;               (local-set-key (kbd "C-c b") 'ts-send-buffer)
;;               (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
;;               (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))

;;; Code:

(require 'comint)

(defgroup inferior-ts nil
  "Run a javascript process in a buffer."
  :group 'inferior-ts)

(defcustom inferior-ts-program-command "tsun"
  "JavaScript interpreter."
  :group 'inferior-ts)

(defcustom inferior-ts-program-arguments nil
  "List of command line arguments to pass to the Typescript interpreter."
  :group 'inferior-ts)

(defcustom inferior-ts-mode-hook nil
  "*Hook for customizing inferior-ts mode."
  :type 'hook
  :group 'inferior-ts)

(defcustom inferior-ts-mode-ansi-color t
  "Use ansi-colors for inferior Typescript mode."
  :group 'inferior-ts)

(defvar inferior-ts-buffer nil
  "Name of the inferior Typescript buffer.")

(defvar ts-prompt-regexp "^\\(?:> \\)"
  "Prompt for `run-ts'.")


;;;###autoload
(defun ts--is-nodejs ()
  (string= "node"
           (substring-no-properties inferior-ts-program-command -4 nil)))

(defun ts--guess-load-file-cmd (filename)
  (let ((cmd (concat "require(\"" filename "\")\n")))
    (when (not (ts--is-nodejs))
      (setq cmd (concat "load(\"" filename "\")\n")))
    cmd
    ))

;;;###autoload
(defun run-ts (cmd &optional dont-switch-p)
  "Run an inferior Typescript process, input and output via buffer `*ts*'.
If there is a process already running in `*ts*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-ts-program-command').
Runs the hook `inferior-ts-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive
   (list
    (when current-prefix-arg
      (setq cmd
            (read-string "Run js: "
                         (mapconcat
                          'identity
                          (cons
                           inferior-ts-program-command
                           inferior-ts-program-arguments)
                          " ")))
      (setq inferior-ts-program-arguments (split-string cmd))
      (setq inferior-ts-program-command (pop inferior-ts-program-arguments)))))

  (if (not (comint-check-proc "*ts*"))
      (with-current-buffer
          (apply 'make-comint "ts" inferior-ts-program-command
                 nil inferior-ts-program-arguments)
        (inferior-ts-mode)))
  (setq inferior-ts-buffer "*ts*")
  (if (not dont-switch-p)
      (pop-to-buffer "*ts*"))

  ;; apply terminal preferences
  (if inferior-ts-mode-ansi-color
      (progn
        ;; based on
        ;; http://stackoverflow.com/questions/13862471/using-node-ts-with-ts-comint-in-emacs

        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output))))
    (setenv "NODE_NO_READLINE" "1")))

;;;###autoload
(defun ts-send-region (start end)
  "Send the current region to the inferior Typescript process."
  (interactive "r")
  (run-ts inferior-ts-program-command t)
  (comint-send-region inferior-ts-buffer start end)
  (comint-send-string inferior-ts-buffer "\n"))

;;;###autoload
(defun ts-send-region-and-go (start end)
  "Send the current region to the inferior Typescript process."
  (interactive "r")
  (run-ts inferior-ts-program-command t)
  (comint-send-region inferior-ts-buffer start end)
  ;; (comint-send-string inferior-ts-buffer "\n")
  (switch-to-js inferior-ts-buffer))

;;;###autoload
(defun ts-send-last-sexp-and-go ()
  "Send the previous sexp to the inferior Typescript process."
  (interactive)
  (ts-send-region-and-go
   (save-excursion
     (backward-sexp)
     (move-beginning-of-line nil)
     (point))
   (point)))

;;;###autoload
(defun ts-send-last-sexp ()
  "Send the previous sexp to the inferior Typescript process."
  (interactive)
  (ts-send-region
   (save-excursion
     (backward-sexp)
     (move-beginning-of-line nil)
     (point))
   (point)))

;;;###autoload
(defun ts-send-buffer ()
  "Send the buffer to the inferior Typescript process."
  (interactive)
  (ts-send-region (point-min) (point-max)))


;;;###autoload
(defun ts-send-buffer-and-go ()
  "Send the buffer to the inferior Typescript process."
  (interactive)
  (ts-send-region-and-go (point-min) (point-max)))

;;;###autoload
(defun ts-load-file (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-ts inferior-ts-program-command t)
    (comint-send-string inferior-ts-buffer (ts--guess-load-file-cmd filename))))

;;;###autoload
(defun ts-load-file-and-go (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-ts inferior-ts-program-command t)
    (comint-send-string inferior-ts-buffer (ts--guess-load-file-cmd filename))
    (switch-to-js inferior-ts-buffer)))

;;;###autoload
(defun switch-to-js (eob-p)
  "Switch to the javascript process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (and inferior-ts-buffer (get-buffer inferior-ts-buffer))
      (pop-to-buffer inferior-ts-buffer)
    (error "No current process buffer.  See variable `inferior-ts-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar inferior-ts-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-x\C-e" 'ts-send-last-sexp)
    (define-key m "\C-cl" 'ts-load-file)
    m))

;;;###autoload
(define-derived-mode inferior-ts-mode comint-mode "Inferior Typescript"
  "Major mode for interacting with an inferior javascript process.

The following commands are available:
\\{inferior-ts-mode-map}

A typescript process can be fired up with M-x run-ts.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-ts-mode-hook (in that order).

You can send text to the inferior Typescript process from other buffers containing
Typescript source.
    switch-to-js switches the current buffer to the Typescript process buffer.
    ts-send-region sends the current region to the Typescript process.
"
  :group 'inferior-ts
  (use-local-map inferior-ts-mode-map))

(provide 'ts-comint)
;;; ts-comint.el ends here
