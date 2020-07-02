;;; ivy-describe-modes.el --- Ivy interface to `describe-mode`  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-02 19:56:29

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to `describe-mode`.
;; Keyword:
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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

(defgroup ivy-describe-modes nil
  "Ivy interface to `describe-mode`."
  :prefix "ivy-describe-modes-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-describe-modes"))

(defconst ivy-describe-modes--prompt "[Describe Modes]: "
  "Prompt string when using `ivy-describe-modes'.")

(defvar ivy-describe-modes--apropos-buffer nil
  "Record the apropos buffer.")

(defun ivy-describe-modes--clean-up ()
  "Do clean up after command."
  (when ivy-describe-modes--apropos-buffer
    (kill-buffer ivy-describe-modes--apropos-buffer)
    (setq ivy-describe-modes--apropos-buffer nil)))

(defun ivy-describe-modes--major-mode-list ()
  "Return a list of major modes."
  (unless (ignore-errors (apropos-command "-mode$"))
    (setq ivy-describe-modes--apropos-buffer (current-buffer))
    
    ))

(defun ivy-describe-modes--minor-mode-list ()
  "Return a list of minor modes."
  (let ((final-lst '()))
    (dolist (mode minor-mode-list)
      (push (symbol-name mode) final-lst))
    final-lst))

(defun ivy-describe-modes--candidates ()
  "List of canddidates, all modes."
  (append (ivy-describe-modes--major-mode-list) (ivy-describe-modes--minor-mode-list)))

(defun ivy-describe-modes--do-action (cand)
  "Do action after ivy done."
  (describe-variable cand))

;;;###autoload
(defun ivy-describe-modes ()
  "A convenient Ivy version of `describe-mode'."
  (interactive)
  (ivy-read ivy-describe-modes--prompt
            (ivy-describe-modes--candidates)
            :require-match t
            :action #'ivy-describe-modes--do-action)
  (ivy-describe-modes--clean-up))


(provide 'ivy-describe-modes)
;;; ivy-describe-modes.el ends here
