;;; ivy-describe-modes.el --- Ivy interface to `describe-mode`  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-02 19:56:29

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to `describe-mode`.
;; Keyword:
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (ivy "0.13.0"))
;; URL: https://github.com/jcs-elpa/ivy-describe-modes

;; This file is NOT part of GNU Emacs.

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
;; Ivy interface to `describe-mode`.
;;

;;; Code:

(require 'ivy)

(defgroup ivy-describe-modes nil
  "Ivy interface to `describe-mode`."
  :prefix "ivy-describe-modes-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-describe-modes"))

(defconst ivy-describe-modes--prompt "[Describe Modes]: "
  "Prompt string when using `ivy-describe-modes'.")

(defconst ivy-describe-modes--apropos-name "*Apropos*"
  "Buffer name when called apropos.")

(defvar ivy-describe-modes--apropos-buffer nil
  "Record the apropos buffer.")

(defvar ivy-describe-modes--cache nil
  "Cache the mode list.")

(defun ivy-describe-modes--clean-up ()
  "Do clean up after command."
  (when ivy-describe-modes--apropos-buffer
    (kill-buffer ivy-describe-modes--apropos-buffer)
    (setq ivy-describe-modes--apropos-buffer nil)))

(defun ivy-describe-modes--mute-apply (fnc &rest args)
  "Execute FNC with ARGS without message."
  (let ((message-log-max nil))
    (with-temp-message (or (current-message) nil)
      (let ((inhibit-message t))
        (apply fnc args)))))

(defun ivy-describe-modes--major-mode-list ()
  "Return a list of major modes."
  (let ((cmd-success t) (lst '()) (start -1) (end -1) (str nil)
        (buf-str nil))
    (save-window-excursion
      (when (get-buffer ivy-describe-modes--apropos-name)
        (kill-buffer ivy-describe-modes--apropos-name))
      (ivy-describe-modes--mute-apply
       (lambda ()
         (setq cmd-success (ignore-errors (apropos-command "-mode$"))))))
    (when cmd-success
      (with-current-buffer ivy-describe-modes--apropos-name
        (setq buf-str (buffer-string))
        (setq ivy-describe-modes--apropos-buffer (current-buffer))
        (goto-char (point-min))
        (while (re-search-forward "-mode[ \t]*M-x" nil t)
          (setq start (1- (line-beginning-position)))
          (goto-char start)
          (re-search-forward "[ \t]" nil t)
          (setq end (- (point) 2))
          (setq str (substring buf-str start end))
          (push str lst)
          (forward-line 1))))
    lst))

(defun ivy-describe-modes--candidates ()
  "List of canddidates, all modes."
  (unless ivy-describe-modes--cache
    (setq ivy-describe-modes--cache (ivy-describe-modes--major-mode-list)))
  ivy-describe-modes--cache)

(defun ivy-describe-modes--do-action (cand)
  "Do action after ivy done with CAND."
  (describe-function (intern cand)))

;;;###autoload
(defun ivy-describe-modes ()
  "A convenient Ivy version of `describe-mode'."
  (interactive)
  (unwind-protect
      (ivy-read ivy-describe-modes--prompt
                (ivy-describe-modes--candidates)
                :require-match t
                :sort t
                :action #'ivy-describe-modes--do-action)
    (ivy-describe-modes--clean-up)))


(provide 'ivy-describe-modes)
;;; ivy-describe-modes.el ends here
