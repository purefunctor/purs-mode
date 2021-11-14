;;; purs-font-lock.el --- `font-lock' definitions for `purs-mode'. -*- lexical-binding: t -*-

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

;; This module defines fontification for PureScript source files through
;; `font-lock-mode'.

;;; Code:

(defconst purs-keywords-regexp
  (concat (regexp-opt
           '("if" "then" "else" "do"
             "ado" "in" "module" "as"
             "import" "foreign" "data"
             "kind" "type" "infix"
             "infixr" "infixl" "where"
             "class" "instance" "forall"
             "let" "derive" "newtype"
             "case" "of")
           'words)
          "\\|∀")
  "Regexp for matching keywords.")

(defconst purs-value-decl-regexp
  "^\\([a-z_][0-9A-Za-z_']*\\)"
  "Regexp for matching top-level value declarations.")

(defconst purs-type-module-regexp
  "\\<[A-Z][0-9A-Za-z_']*\\(.[A-Z][0-9A-Za-z_']*\\)*"
  "Regexp for matching type and module names.")

(defvar purs-font-lock-keywords
  `((,purs-keywords-regexp . 'font-lock-keyword-face)
    (,purs-value-decl-regexp 1 'font-lock-function-name-face)
    (,purs-type-module-regexp . 'font-lock-type-face))
  "Keywords for fontification of PureScript source files.")

(provide 'purs-font-lock)

;;; purs-font-lock.el ends here
