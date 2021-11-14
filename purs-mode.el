;;; purs-mode.el --- PureScript mode for Emacs -*- lexical-binding: t -*-

;; Copyright 2021, PureFunctor

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module defines the PureScript major mode for Emacs.

;;; Code:

(require 'purs-font-lock)
(require 'purs-indent)

(defconst purs-syntax-table
 (let ((table (make-syntax-table)))
   (modify-syntax-entry ?_  "w" table)
   (modify-syntax-entry ?\?  "w" table)
   (modify-syntax-entry ?.  "." table)
   (modify-syntax-entry ?'  "w" table)
   (modify-syntax-entry ?\\ "w" table)
   (modify-syntax-entry ?\{  "(}1nb" table)
   (modify-syntax-entry ?\}  "){4nb" table)
   (modify-syntax-entry ?-  "_ 123" table)
   (modify-syntax-entry ?\n ">" table)
   table))

;;;###autoload
(define-derived-mode purs-mode prog-mode "PureScript"
  "Major mode for editing PureScript programs."
  :group 'purescript
  :syntax-table purs-syntax-table
  (setq-local indent-tabs-mode nil
              case-fold-search nil
              comment-start "--"
              comment-end ""
              font-lock-defaults '(purs-font-lock-keywords))
  :after-hook purs-mode-hook)

(progn
  (add-to-list 'auto-mode-alist '("\\.purs\\'" . purs-mode))
  (modify-coding-system-alist 'file "\\.purs\\'" 'utf-8))

(provide 'purs-mode)

;;; purs-mode.el ends here
