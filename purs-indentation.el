;;; purs-indentation.el --- indentation module for PureScript Mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Kristof Bastiaensen, Gergely Risko

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>
;; Author: Gergely Risko <errge@nilcons.com>
;; Keywords: indentation haskell
;; URL: https://github.com/haskell/haskell-mode

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

;; Notice
;;
;; This file is forked from haskell-indentation.el that lives in
;; https://github.com/haskell/haskell-mode. Haskell-specific code e.g.
;; is either removed for simplicity or renamed to PureScript.

;;; Code:

;; TODO eliminate magic number 2 where possible, use a variable

;; TODO `haskell-indentation-find-indentation' — fix it, get rid of "safe"
;; version

(require 'cl-lib)
(require 'purs-lexeme)

;;;###autoload
(defgroup purs-indentation nil
  "Haskell indentation."
  :link '(custom-manual "(purs-mode)Indentation")
  :group 'haskell
  :prefix "purs-indentation-")

(defcustom purs-indentation-layout-offset 2
  "Extra indentation to add before expressions in a Haskell layout list."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-starter-offset 2
  "Extra indentation after an opening keyword (e.g. \"let\")."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after \"do\")."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-where-pre-offset 2
  "Extra indentation before the keyword \"where\"."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-where-post-offset 2
  "Extra indentation after the keyword \"where\"."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-electric-flag nil
  "Non-nil means insertion of some characters may auto reindent the line.
If the variable `electric-indent-mode' is non-nil then this variable is
overridden."
  :type 'symbol
  :group 'purs-indentation)
(make-variable-buffer-local 'purs-indentation-electric-flag)

(defvar purs-indentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'purs-indentation-newline-and-indent)
    (define-key map (kbd "<backtab>") #'purs-indentation-indent-backwards)
    (define-key map (kbd ",") #'purs-indentation-common-electric-command)
    (define-key map (kbd ";") #'purs-indentation-common-electric-command)
    (define-key map (kbd ")") #'purs-indentation-common-electric-command)
    (define-key map (kbd "}") #'purs-indentation-common-electric-command)
    (define-key map (kbd "]") #'purs-indentation-common-electric-command)
    map)
  "Keymap for `purs-indentation-mode'.")

;;;###autoload
(define-minor-mode purs-indentation-mode
  "Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs."
  :keymap purs-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'indent-region-function)

  (when purs-indentation-mode
    (when (and (bound-and-true-p purs-indent-mode)
               (fboundp 'turn-off-purs-indent))
      (turn-off-purs-indent))
    (setq-local indent-line-function #'purs-indentation-indent-line)
    (setq-local indent-region-function #'purs-indentation-indent-region)))

;;;###autoload
(defun turn-on-purs-indentation ()
  "Turn on the purs-indentation minor mode."
  (interactive)
  (purs-indentation-mode t))

(make-obsolete 'turn-on-purs-indentation
               'purs-indentation-mode
               "2015-05-25")

;;----------------------------------------------------------------------------
;; UI starts here

(defun purs-indentation-reindent-to (col &optional move)
  "Reindent current line to COL, move the point there if MOVE is non-NIL."
  (let* ((ci (purs-indentation-current-indentation)))
    (save-excursion
      (move-to-column ci)
      (if (<= ci col)
          (insert-before-markers (make-string (- col ci) ? ))
        (delete-char (- col ci))))
    (when move
      (move-to-column col))))

(defun purs-indentation-indent-rigidly (start end arg)
  "Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large
negative ARG."
  (interactive "*r\np")
  (save-excursion
    (goto-char end)
    (let ((end-marker (point-marker)))
      (goto-char start)
      (or (bolp) (forward-line 0))
      (while (< (point) end-marker)
        (let ((ci (purs-indentation-current-indentation)))
          (when (and t
                     (eq (char-after) ?>))
            (forward-char 1))
          (skip-syntax-forward "-")
          (unless (eolp)
            (purs-indentation-reindent-to (max 0 (+ ci arg))))
          (forward-line 1)))
      (move-marker end-marker nil))))

(defun purs-indentation-current-indentation ()
  "Column position of first non-whitespace character in current line."
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward "-")
    (current-column)))

(defun purs-indentation-newline-and-indent ()
  "Insert newline and indent."
  (interactive "*")
  (let ((ci (purs-indentation-current-indentation)))
    ;; - jump to the next line and reindent to at the least same level
    (delete-horizontal-space)
    (newline)
    ;; calculate indentation after newline is inserted because if we
    ;; break an identifier we might create a keyword, for example
    ;; "dowhere" => "do where"
    (let ((indentations (or (purs-indentation-find-indentations)
                            '(0))))
      (purs-indentation-reindent-to
       (purs-indentation-next-indentation (- ci 1) indentations 'nofail)
       'move))))

(defun purs-indentation-next-indentation (col indentations &optional nofail)
  "Find the leftmost indentation which is greater than COL.
Indentations are taken from INDENTATIONS, which should be a
list.  Return the last indentation if there are no bigger ones and
NOFAIL is non-NIL."
  (when (null indentations)
    (error "purs-indentation-next-indentation called with empty list"))
  (or (cl-find-if (lambda (i) (> i col)) indentations)
      (when nofail
        (car (last indentations)))))

(defun purs-indentation-previous-indentation (col indentations &optional nofail)
  "Find the rightmost indentation less than COL from INDENTATIONS.
When no indentations are less than COL, return the rightmost indentation
if NOFAIL is non-nil, or nil otherwise."
  (when (null indentations)
    (error "purs-indentation-previous-indentation called with empty list"))
  (let ((rev (reverse indentations)))
    (or (cl-find-if (lambda (i) (< i col)) rev)
        (when nofail
          (car rev)))))

(defvar purs-indentation-dyn-last-direction nil
  "") ; FIXME
(defvar purs-indentation-dyn-last-indentations nil
  "") ; FIXME

(defun purs-indentation-indent-line ()
  "Indent current line, cycle though indentation positions.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
  (interactive "*")
  ;; try to repeat
  (when (not (purs-indentation-indent-line-repeat))
    (setq purs-indentation-dyn-last-direction nil)
    ;; parse error is intentionally not cought here, it may come from
    ;; `purs-indentation-find-indentations', but escapes the scope
    ;; and aborts the opertaion before any moving happens
    (let* ((cc (current-column))
           (ci (purs-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (or (purs-indentation-find-indentations)
                       '(0))))
           (valid (memq ci inds))
           (cursor-in-whitespace (< cc ci)))

      (if (and valid cursor-in-whitespace)
          (move-to-column ci)
        (purs-indentation-reindent-to
         (purs-indentation-next-indentation ci inds 'nofail)
         cursor-in-whitespace))
      (setq purs-indentation-dyn-last-direction 'right
            purs-indentation-dyn-last-indentations inds))))

(defun purs-indentation-indent-line-repeat ()
  "Cycle though indentation positions."
  (cond
   ((and (memq last-command
               '(indent-for-tab-command
                 purs-indentation-indent-backwards))
         (eq purs-indentation-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (purs-indentation-indent-rigidly
       (region-beginning)
       (region-end)
       1))
    t)
   ((and (eq last-command 'indent-for-tab-command)
         (memq purs-indentation-dyn-last-direction '(left right))
         purs-indentation-dyn-last-indentations)
    (let ((ci (purs-indentation-current-indentation)))
      (if (eq purs-indentation-dyn-last-direction 'left)
          (purs-indentation-reindent-to
           (purs-indentation-previous-indentation
            ci purs-indentation-dyn-last-indentations 'nofail))
        ;; right
        (if (purs-indentation-next-indentation
             ci purs-indentation-dyn-last-indentations)
            (purs-indentation-reindent-to
             (purs-indentation-next-indentation
              ci purs-indentation-dyn-last-indentations 'nofail))
          ;; but failed, switch to left
          (setq purs-indentation-dyn-last-direction 'left)
          (purs-indentation-indent-line-repeat)))
      t))
   (t nil)))

(defun purs-indentation-indent-region (_start _end)
  "This function does nothing.

It is better to do nothing to indent region in Haskell than to
break the semantics of indentation.  This function is used for
`indent-region-function' because the default is to call
`indent-line-function' on every line from START to END and that
also produces catastrophic results.

Someday we will have indent region that preserves semantics and
fixes up only indentation."
  nil)

(defun purs-indentation-indent-backwards ()
  "Indent the current line to the previous indentation point."
  (interactive "*")
  (cond
   ((and (memq last-command
               '(indent-for-tab-command purs-indentation-indent-backwards))
         (eq purs-indentation-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (purs-indentation-indent-rigidly (region-beginning) (region-end) -1)))
   ((use-region-p)
    (setq purs-indentation-dyn-last-direction 'region)
    (purs-indentation-indent-rigidly (region-beginning) (region-end) -1)
    (message "Press TAB or S-TAB again to indent the region more"))
   (t
    (setq purs-indentation-dyn-last-direction nil)
    (let* ((cc (current-column))
           (ci (purs-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (or (purs-indentation-find-indentations)
                       '(0))))
           (cursor-in-whitespace (< cc ci))
           (pi (purs-indentation-previous-indentation ci inds)))
      (if (null pi)
          ;; if there are no more indentations to the left, just go to column 0
          (purs-indentation-reindent-to
           (car (purs-indentation-first-indentation)) cursor-in-whitespace)
        (purs-indentation-reindent-to pi cursor-in-whitespace))))))

(defun purs-indentation-common-electric-command (arg)
  "Call `self-insert-command' to insert the character typed ARG times
and indent when all of the following are true:
1) The character is the first non-whitespace character on the line.
2) There is only one possible indentation position.
3) The variable `electric-indent-mode' or `purs-indentation-electric-flag'
   is non-nil.
4) The point is not in a comment, string, or quasiquote."
  (interactive "*p")
  (let ((col (current-column))
        ind)
    (self-insert-command arg)
    (when (and (or purs-indentation-electric-flag
                   electric-indent-mode)
               (= (purs-indentation-current-indentation)
                  col)
               (> arg 0)
               (not (nth 8 (syntax-ppss)))
               (= 1 (save-excursion
                      (move-to-column col)
                      (length (setq ind (purs-indentation-find-indentations))))))
      (purs-indentation-reindent-to (car ind)))))


;;----------------------------------------------------------------------------
;; Parser Starts Here

;; The parser is implemented as a recursive descent parser.  Each parser
;; advances the point to after the expression it parses, and sets the
;; dynamic scoped variables containing the information about the
;; indentations.  The dynamic scoping allows transparent backtracking to
;; previous states of these variables.  A new state can be set using `let'.
;; When the scope of this function ends, the variable is automatically
;; reverted to its old value.

;; This is basically a performance hack.  It would have been possible to
;; thread this state using a association-list through the parsers, but it
;; would be probably more complicated and slower due to the lack of real
;; closures in Emacs Lisp.
;;
;; When finished parsing, the tokenizer returns 'end-token, and
;; following-token is set to the token after point.  The parser adds its
;; indentations to possible-indentations and returns to it's parent, or
;; exits non-locally by throwing parse-end, so that the parent will not add
;; new indentations to it.

;; the parse state:
(defvar following-token)        ;; the next token after parsing finished
;; the token at the current parser point or a pseudo-token (see
;; `purs-indentation-read-next-token')
(defvar current-token)
(defvar previous-token)
(defvar left-indent)            ;; most left possible indentation
(defvar starter-indent)         ;; column at a keyword
(defvar current-indent)         ;; the most right indentation
(defvar layout-indent)          ;; the column of the layout list
(defvar possible-indentations)  ;; the return value of the indentations
(defvar indentation-point)      ;; where to stop parsing
(defvar implicit-layout-active) ;; is "off-side" rule active?

(defun purs-indentation-goto-least-indentation ()
  "" ; FIXME
  (beginning-of-line)
  (catch 'return
    (while (not (bobp))
      (let ((point (point)))
        ;; (forward-comment -1) gets lost if there are unterminated
        ;; string constants and does not move point anywhere. We fix
        ;; that case with (forward-line -1)
        (forward-comment (- (buffer-size)))
        (if (equal (point) point)
            (forward-line -1)
          (beginning-of-line)))
      (let* ((ps (syntax-ppss))
             (start-of-comment-or-string (nth 8 ps))
             (start-of-list-expression (nth 1 ps)))
        (cond
         (start-of-comment-or-string
          ;; inside comment or string
          (goto-char start-of-comment-or-string))
         (start-of-list-expression
          ;; inside a parenthesized expression
          (goto-char start-of-list-expression))
         ((= 0 (purs-indentation-current-indentation))
          (throw 'return nil))))))
  (beginning-of-line)
  (when (bobp)
    (forward-comment (buffer-size))))

(defun purs-indentation-parse-to-indentations ()
  "" ; FIXME
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (current-indent purs-indentation-layout-offset)
          (starter-indent purs-indentation-layout-offset)
          (left-indent purs-indentation-layout-offset)
          (case-fold-search nil)
          current-token
          previous-token
          following-token
          possible-indentations
          implicit-layout-active)
      (purs-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
          (purs-indentation-first-indentation)
        (setq current-token (purs-indentation-peek-token))
        (catch 'parse-end
          (purs-indentation-toplevel))
        possible-indentations))))

(defun purs-indentation-first-indentation ()
  "Return column of first indentation."
  (list 0))

(defun purs-indentation-find-indentations ()
  "Return list of indentation positions corresponding to actual cursor position."
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (if (save-excursion
            (and (forward-line -1)
                 (< (nth 8 ppss) (point))))
          ;; if this string goes over more than one line we want to
          ;; sync with the last line, not the first one
          (list (save-excursion
                  (forward-line -1)
                  (current-indentation)))

        (append
         (purs-indentation-first-indentation)
         (list (save-excursion
                 (goto-char (nth 8 ppss))
                 (current-column))))))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (purs-indentation-parse-to-indentations)
        (purs-indentation-first-indentation)))
     (t
      (purs-indentation-parse-to-indentations)))))

(defconst purs-indentation-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("⤙" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("⤚" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation from UnicodeSyntax tokens to their ASCII representation.")

(defconst purs-indentation-toplevel-list
  `(("module"    . purs-indentation-module)
    ("signature" . purs-indentation-module)
    ("data"      . purs-indentation-data)
    ("type"      . purs-indentation-data)
    ("newtype"   . purs-indentation-data)
    ("import"    . purs-indentation-import)
    ("foreign"   . purs-indentation-foreign)
    ("where"     . purs-indentation-toplevel-where)
    ("class"     . purs-indentation-class-declaration)
    ("instance"  . purs-indentation-class-declaration)
    ("else"      . purs-indentation-else-instance)
    ("deriving"  . purs-indentation-deriving))
  "Alist of toplevel keywords with associated parsers.")

(defconst purs-indentation-type-list
  `(("::" .
     ,(apply-partially 'purs-indentation-with-starter
                       (apply-partially 'purs-indentation-separated
                                        'purs-indentation-type '("->" "=>"))))
    ("("  .
     ,(apply-partially 'purs-indentation-list
                       'purs-indentation-type ")" ","))
    ("["  .
     ,(apply-partially 'purs-indentation-list
                       'purs-indentation-type "]" ","))
    ("{"  .
     ,(apply-partially 'purs-indentation-list
                       'purs-indentation-type "}" ",")))
  "Alist of tokens in type declarations with associated parsers.")

(defconst purs-indentation-expression-list
  `(("data"    . purs-indentation-data)
    ("type"    . purs-indentation-data)
    ("newtype" . purs-indentation-data)
    ("if"      . purs-indentation-if)
    ("let"     .
     ,(apply-partially 'purs-indentation-phrase
                       '(purs-indentation-declaration-layout
                         "in" purs-indentation-expression)))
    ("do"      .
     ,(apply-partially 'purs-indentation-with-starter
                       'purs-indentation-expression-layout))
    ("mdo"     .
     ,(apply-partially 'purs-indentation-with-starter
                       'purs-indentation-expression-layout))
    ("rec"     .
     ,(apply-partially 'purs-indentation-with-starter
                       'purs-indentation-expression-layout))
    ("case"    .
     ,(apply-partially 'purs-indentation-phrase
                       '(purs-indentation-comma-separated
                         "of" purs-indentation-case-layout)))
    ("\\"      .
     ,(apply-partially 'purs-indentation-with-starter
                       'purs-indentation-lambda-maybe-lambdacase))
    ("proc"    .
     ,(apply-partially 'purs-indentation-phrase
                       '(purs-indentation-expression
                         "->" purs-indentation-expression)))
    ("where"   .
     ,(apply-partially 'purs-indentation-with-starter
                       'purs-indentation-declaration-layout nil t))
    ("::"      .        purs-indentation-scoped-type)
    ("="       .
     ,(apply-partially 'purs-indentation-statement-right
                       'purs-indentation-expression))
    ("<-"      .
     ,(apply-partially 'purs-indentation-statement-right
                       'purs-indentation-expression))
    ("("       .
     ,(apply-partially 'purs-indentation-list
                       'purs-indentation-expression
                       ")"
                       '(list "," "->")))
    ("["       .
     ,(apply-partially 'purs-indentation-list
                       'purs-indentation-expression "]" "," "|"))
    ("{"       .
     ,(apply-partially 'purs-indentation-list
                       'purs-indentation-expression "}" ",")))
  "Alist of keywords in expressions with associated parsers.")

(defun purs-indentation-expression-layout ()
  "Parse layout list with expressions, such as after \"do\"."
  (purs-indentation-layout #'purs-indentation-expression))

(defun purs-indentation-declaration-layout ()
  "Parse layout list with declarations, such as after \"where\"."
  (purs-indentation-layout #'purs-indentation-declaration))

(defun purs-indentation-case-layout ()
  "Parse layout list with case expressions."
  (purs-indentation-layout #'purs-indentation-case))

(defun purs-indentation-lambda-maybe-lambdacase ()
  "Parse lambda or lambda-case expression.
After a lambda (backslash) there are two possible cases:

- the new lambdacase expression, that can be recognized by the
  next token being \"case\";

- or simply an anonymous function definition in the form of
  \"expression -> expression\"."
  (if (string= current-token "case")
      (purs-indentation-with-starter
       #'purs-indentation-case-layout)
    (purs-indentation-phrase-rest
     '(purs-indentation-expression "->" purs-indentation-expression))))

(defun purs-indentation-fundep ()
  "Parse functional dependency."
  (purs-indentation-with-starter
   (apply-partially #'purs-indentation-separated
                    #'purs-indentation-fundep1 ",")))

(defun purs-indentation-fundep1 ()
  "Parse an item in functional dependency declaration."
  (let ((current-indent (current-column)))
    (while (member current-token '(value "->"))
      (purs-indentation-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (purs-indentation-add-indentation current-indent))))

(defun purs-indentation-toplevel ()
  "Parse toplevel statements."
  (purs-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token purs-indentation-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (purs-indentation-declaration))))))

(defun purs-indentation-type ()
  "Parse type declaration."
  (let ((current-indent (current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->" "=>"))
          (purs-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "(" "[" "{" "::"))
            (if (equal following-token "=>")
                (purs-indentation-add-indentation starter-indent)
              (purs-indentation-add-indentation current-indent))
            (purs-indentation-add-indentation left-indent))
          (throw 'return nil))
         (t (let ((parser (assoc current-token purs-indentation-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

(defun purs-indentation-type-1 ()
  "Parse a single type declaration."
  (let ((current-indent (current-column)))
    (catch 'return
      (cond
       ((member current-token '(value operator "->" "=>"))
        (purs-indentation-read-next-token))

       ((eq current-token 'end-tokens)
        (when (member following-token
                      '(value operator no-following-token
                              "->" "=>" "(" "[" "{" "::"))
          (purs-indentation-add-indentation current-indent))
        (throw 'return nil))
       (t (let ((parser (assoc current-token purs-indentation-type-list)))
            (if (not parser)
                (throw 'return nil)
              (funcall (cdr parser)))))))))

(defun purs-indentation-scoped-type ()
  "Parse scoped type declaration.

For example
   let x :: Int = 12
   do x :: Int <- return 12"
  (purs-indentation-with-starter
   (apply-partially #'purs-indentation-separated #'purs-indentation-type '("->" "=>")))
  (when (member current-token '("<-" "="))
    (purs-indentation-statement-right #'purs-indentation-expression)))

(defun purs-indentation-data ()
  "Parse data or type declaration."
  (purs-indentation-read-next-token)
  (when (string= current-token "instance")
    (purs-indentation-read-next-token))
  (purs-indentation-type)
  (cond ((eq current-token 'end-tokens)
         (when (member following-token '("=" "where"))
           (purs-indentation-add-indentation current-indent)
           (throw 'parse-end nil)))
        ((string= current-token "=")
         (let ((starter-indent-inside (current-column)))
           (purs-indentation-with-starter
            (lambda ()
              (purs-indentation-separated
               #'purs-indentation-expression "|")))
           (cond
            ((equal current-token 'end-tokens)
             (when (string= following-token "deriving")
               (purs-indentation-push-indentation starter-indent-inside)
               (purs-indentation-add-left-indent)))
            ((equal current-token "deriving")
             (purs-indentation-with-starter
              #'purs-indentation-type-1)))))
        ((string= current-token "where")
         (purs-indentation-with-starter
          #'purs-indentation-expression-layout nil)
         (cond
          ((equal current-token 'end-tokens)
           (when (string= following-token "deriving")
             (purs-indentation-add-left-indent)))
          ((equal current-token "deriving")
           (purs-indentation-with-starter
            #'purs-indentation-type-1))))))

(defun purs-indentation-import ()
  "Parse import declaration."
  (purs-indentation-with-starter #'purs-indentation-expression))

(defun purs-indentation-foreign ()
  "Parse foreign import declaration."
  (purs-indentation-with-starter (apply-partially #'purs-indentation-expression '(value operator "import"))))

(defun purs-indentation-class-declaration ()
  "Parse class declaration."
  (purs-indentation-with-starter
   (lambda ()
     (purs-indentation-type)
     (when (string= current-token "|")
       (purs-indentation-fundep))
     (when (string= current-token "where")
       (purs-indentation-with-starter
        #'purs-indentation-declaration-layout nil)))))

(defun purs-indentation-else-instance ()
  "Parse instance chain delcaration."
  (purs-indentation-with-starter
   (lambda ()
     (purs-indentation-class-declaration)) nil))

(defun purs-indentation-comma-separated ()
  "Parse comma-separated expressions."
  (purs-indentation-separated #'purs-indentation-expression "," nil))

(defun purs-indentation-deriving ()
  "Parse standalone declaration."
  (purs-indentation-with-starter
   (lambda ()
     (when (string= "instance" current-token)
       (purs-indentation-read-next-token))
     (when (equal current-token 'end-tokens)
       (purs-indentation-add-left-indent)
       (throw 'parse-end nil))
     (purs-indentation-type)
     (when (string= current-token "|")
       (purs-indentation-fundep)))))

(defun purs-indentation-module ()
  "Parse module declaration."
  (purs-indentation-with-starter
   (lambda ()
     (purs-indentation-read-next-token)
     (when (equal current-token 'layout-item)
       (purs-indentation-read-next-token))
     (when (string= current-token "(")
       (purs-indentation-list
        #'purs-indentation-module-export
        ")" ","))
     (if (string= current-token "where")
         (purs-indentation-read-next-token)

       (when (eq current-token 'end-tokens)
         (when (member following-token '(value no-following-token "("))
           (purs-indentation-add-indentation
            (+ starter-indent purs-indentation-starter-offset))
           (purs-indentation-add-indentation
            (+ left-indent purs-indentation-starter-offset))
           (throw 'parse-end nil))
         (purs-indentation-add-layout-indent)
         (throw 'parse-end nil))))))

(defun purs-indentation-toplevel-where ()
  "Parse 'where' that we may hit as a standalone in module declaration."
  (purs-indentation-read-next-token)

  (when (eq current-token 'end-tokens)
    (purs-indentation-add-layout-indent)
    (throw 'parse-end nil)))

(defun purs-indentation-module-export ()
  "Parse export list."
  (cond ((string= current-token "module")
         (let ((current-indent (current-column)))
           (purs-indentation-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (purs-indentation-add-indentation current-indent))
                 ((eq current-token 'value)
                  (purs-indentation-read-next-token)))))
        (t (purs-indentation-type))))

(defun purs-indentation-list (parser end sep &optional stmt-sep)
  "Parse a list, pair or other expression containing multiple
items parsed by PARSER, separated by SEP or STMT-SEP, and ending
with END."
  ;; note that we use macro expansion here to preserver Emacs 23
  ;; compatibility and its lack of lexical binding
  (purs-indentation-with-starter
   `(lambda ()
      (let ((implicit-layout-active nil))
        (purs-indentation-separated
         #',parser ,sep ,stmt-sep)))
   end))

(defun purs-indentation-with-starter (parser &optional end where-expr?)
  "Parse an expression starting with a keyword or parenthesis.
Skip the keyword or parenthesis." ; FIXME: better description needed
  (let ((starter-column (current-column))
        (current-indent current-indent)
        (left-indent
         (if (= (current-column) (purs-indentation-current-indentation))
             (current-column)
           left-indent)))
    (purs-indentation-read-next-token)
    (when (eq current-token 'end-tokens)
      (cond ((equal following-token end)
             ;; indent before keyword or parenthesis
             (purs-indentation-add-indentation starter-column))
            (where-expr?
             ;; left indent + where post indent
             (purs-indentation-add-where-post-indent left-indent))
            (t
             (purs-indentation-add-left-indent)))
      (throw 'parse-end nil))
    (let* ((current-indent (current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent
            (if end
                (+ starter-indent purs-indentation-starter-offset)
              left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               ;; indent before keyword or parenthesis
               (purs-indentation-add-indentation starter-indent))
             ;; add no more indentations if we expect a closing keyword
             (when end
               (throw 'parse-end nil)))
            ((equal current-token end)
             (purs-indentation-read-next-token))))))

(defun purs-indentation-case-alternative ()
  "" ; FIXME
  (setq left-indent (current-column))
  (purs-indentation-separated #'purs-indentation-expression "," nil)
  (cond ((eq current-token 'end-tokens)
         (purs-indentation-add-indentation current-indent))
        ((string= current-token "->")
         (purs-indentation-statement-right #'purs-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun purs-indentation-case ()
  "" ; FIXME
  (purs-indentation-comma-separated)
  (cond ((eq current-token 'end-tokens)
         (purs-indentation-add-indentation current-indent))
        ((string= current-token "|")
         (purs-indentation-with-starter
          (apply-partially #'purs-indentation-separated
                           #'purs-indentation-case-alternative "|" nil)
          nil))
        ((string= current-token "->")
         (purs-indentation-statement-right #'purs-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun purs-indentation-statement-right (parser)
  "Process right side of a statement.
Set `current-indent' to the current column and calls the given
parser.  If parsing ends here, set indentation to left-indent."
  (purs-indentation-read-next-token)
  (when (eq current-token 'end-tokens)
    (purs-indentation-add-left-indent)
    (purs-indentation-add-indentation current-indent)
    (throw 'parse-end nil))
  (funcall parser)
  (when (equal current-token "where")
    (purs-indentation-with-starter
     #'purs-indentation-expression-layout nil)))

(defun purs-indentation-guard ()
  "Parse \"guard\" statement."
  (setq left-indent (current-column))
  (purs-indentation-separated
   #'purs-indentation-expression "," nil))

(defun purs-indentation-declaration ()
  "Parse function or type declaration."
  (purs-indentation-separated #'purs-indentation-expression "," nil)
  (when (string= current-token "|")
    (purs-indentation-with-starter
     (apply-partially #'purs-indentation-separated
                      #'purs-indentation-guard "|" nil)
     nil))
  (when (eq current-token 'end-tokens)
   (when (member following-token '("|" "=" "::" ","))
     (purs-indentation-add-indentation current-indent)
     (throw 'parse-end nil))))

(defun purs-indentation-layout (parser)
  "Parse layout list, where each layout item is parsed by parser."
  (purs-indentation-implicit-layout-list parser))

(defun purs-indentation-expression-token-p (token)
  "Return non-NIL value if TOKEN is an expression token."
  (member token
          '("if" "let" "do" "case" "\\" "(" "{" "[" "::"
            value operator no-following-token)))

(defun purs-indentation-expression (&optional accepted-tokens)
  "Parse an expression until an unknown token is encountered."
  (catch 'return
    (let ((current-indent (current-column)))
      (unless accepted-tokens
        (setq accepted-tokens '(value operator)))
      (while t
        (cond
         ((memq current-token accepted-tokens)
          (purs-indentation-read-next-token))
         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (purs-indentation-add-where-pre-indent)) ; before a where
                ((purs-indentation-expression-token-p following-token)
                 ;; a normal expression can be either continued or have
                 ;; left indent
                 (purs-indentation-add-indentation
                  current-indent)
                 (purs-indentation-add-indentation
                  left-indent)))
          (throw 'return nil))
         (t (let ((parser (assoc current-token
                                 purs-indentation-expression-list)))
              (when (null parser)
                (throw 'return nil)) ; not expression token, so exit
              (funcall (cdr parser)) ; run parser

              ;; after an 'open' expression such as 'if', exit
              (unless (member (car parser) '("(" "[" "{" "case"))
                (throw 'return nil)))))))))

(defun purs-indentation-separated (parser separator &optional stmt-separator)
  "Evaluate PARSER separated by SEPARATOR and STMT-SEPARATOR.
If STMT-SEPARATOR is not NIL, it will be used to set a new starter-indent.

For example:

   [ i | i <- [1..10]
    ,"
  (catch 'return
    (unless (listp separator)
      (setq separator (list separator)))
    (unless (listp stmt-separator)
      (setq stmt-separator (list stmt-separator)))
    (while t
      (funcall parser)
      (cond ((member current-token separator)
             (purs-indentation-at-separator))

            ((member current-token stmt-separator)
             (setq starter-indent (current-column))
             (purs-indentation-at-separator))

            ((eq current-token 'end-tokens)
             (when (or (member following-token separator)
                       (member following-token stmt-separator))
               ;; Set an indentation before a separator, for example:
               ;;  [ 1   or   [ 1 | a
               ;;  , 2            , 20
               (purs-indentation-add-indentation starter-indent)
               (when (< left-indent starter-indent)
                 (purs-indentation-add-indentation left-indent))
               (throw 'parse-end nil))
             (when (equal following-token 'no-following-token)
               ;; Set an indentation before a separator, for example:
               ;;  [ 1   or   [ 1 | a
               ;;  , 2            , 20
               (purs-indentation-add-indentation starter-indent)
               (purs-indentation-add-indentation left-indent))
             (throw 'return nil))
            (t (throw 'return nil))))))

(defun purs-indentation-at-separator ()
  "At a separator.

If at a new line, set starter-indent at the separator
and current-indent after the separator, for example:

l = [  1
     , 2
     ,    -- start now here."
  (let ((separator-column
         (and (= (current-column) (purs-indentation-current-indentation))
              (current-column))))
    (purs-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (purs-indentation-add-indentation current-indent)
           (purs-indentation-add-indentation left-indent)
           (throw 'return nil))
          (separator-column ; on the beginning of the line
           (setq current-indent (current-column))
           (setq starter-indent separator-column)
           (setq left-indent
            (+ starter-indent purs-indentation-starter-offset))))))

(defun purs-indentation-implicit-layout-list (parser)
  "An implicit layout list, elements are parsed with PARSER.
This sets the `layout-indent' variable to the column where the
layout starts."
  (let* ((layout-indent (current-column))
         (current-indent (current-column))
         (left-indent (current-column))
         (implicit-layout-active t))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-item ";"))
               (purs-indentation-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (and
                          (not (member following-token '("{" operator)))
                          (not (member previous-token '(operator)))
                          (purs-indentation-expression-token-p following-token))
                         (string= following-token ";")
                         (and (equal layout-indent 0)
                              (member following-token (mapcar #'car purs-indentation-toplevel-list))
                              (not (string= following-token "where"))))
                 (purs-indentation-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put `purs-indentation-read-next-token' outside the current-indent
  ;; definition so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (let ((implicit-layout-active t))
      ;; leave layout at 'layout-end or illegal token
      (purs-indentation-read-next-token))))

(defun purs-indentation-if ()
  "" ; FIXME
  (purs-indentation-with-starter
   (lambda ()
     (if (string= current-token "|")
         (purs-indentation-with-starter
          (lambda ()
            (purs-indentation-separated
             #'purs-indentation-case-alternative "|" nil))
          nil)
       (purs-indentation-phrase-rest
        '(purs-indentation-expression
          "then" purs-indentation-expression
          "else" purs-indentation-expression))))
   nil))

(defun purs-indentation-phrase (phrase)
  "" ; FIXME
  (purs-indentation-with-starter
   (apply-partially #'purs-indentation-phrase-rest phrase)
   nil))

(defun purs-indentation-phrase-rest (phrase1)
  "" ; FIXME
  (while phrase1
    (let ((phrase phrase1))
      (setq phrase1 nil)
      (let ((current-indent (current-column))
            (left-indent left-indent)
            (layout-indent layout-indent))
        (funcall (car phrase)))
      (cond
       ((eq current-token 'end-tokens)
        (cond ((null (cdr phrase))) ;; fallthrough
              ((equal following-token (cadr phrase))
               (purs-indentation-add-indentation starter-indent)
               (unless (member following-token '("," ";"))
                 ;; we want to keep comma and semicolon aligned always
                 (purs-indentation-add-indentation left-indent))
               (throw 'parse-end nil))
              ((string= (cadr phrase) "in")
               (when (= left-indent layout-indent)
                 (purs-indentation-add-layout-indent)
                 (throw 'parse-end nil)))
              (t (throw 'parse-end nil))))
       ((null (cdr phrase)))
       ((equal (cadr phrase) current-token)
        (purs-indentation-read-next-token)
        (when (eq current-token 'end-tokens)
          (purs-indentation-add-indentation
           (+ starter-indent purs-indentation-starter-offset))
          (purs-indentation-add-indentation
           (+ left-indent purs-indentation-starter-offset))
          (throw 'parse-end nil))
        (setq phrase1 (cddr phrase)))
       ((string= (cadr phrase) "in"))))))

(defun purs-indentation-add-indentation (indent)
  "" ; FIXME
  (purs-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent purs-indentation-layout-offset)
     indent)))

(defun purs-indentation-add-layout-indent ()
  "" ; FIXME
  (purs-indentation-push-indentation layout-indent))

(defun purs-indentation-add-where-pre-indent ()
  "" ; FIXME
  (purs-indentation-push-indentation
   (+ layout-indent purs-indentation-where-pre-offset))
  (if (= layout-indent purs-indentation-layout-offset)
      (purs-indentation-push-indentation
       purs-indentation-where-pre-offset)))

(defun purs-indentation-add-where-post-indent (indent)
  "" ; FIXME
  (purs-indentation-push-indentation
   (+ indent purs-indentation-where-post-offset)))

(defun purs-indentation-add-left-indent ()
  "" ; FIXME
  (purs-indentation-add-indentation
   (+ left-indent purs-indentation-left-offset)))

(defun purs-indentation-push-indentation (indent)
  "Add INDENT to list of possible indentations.

Add INDENT to `possible-indentations' if it is not there
yet. Keep the list in ascending order."
  (unless (member indent possible-indentations)
    (setq possible-indentations
          (sort (cons indent possible-indentations) #'<))))

(defun purs-indentation-read-next-token ()
  "Go to the next token and set current-token to the next token.

The following symbols are used as pseudo tokens:

'layout-item: A new item in a layout list.  The next token
              will be the first token from the item.

'layout-end:  the end of a layout list.  Next token will be
              the first token after the layout list.

'end-tokens:  back at point where we started, following-token
              will be set to the next token.

Pseudo tokens are used only when implicit-layout-active is
t. That is the case only after keywords \"do\", \"where\",
\"let\" and \"of\".

If we are at a new line, parse-line is increased, and
current-indent and left-indent are set to the indentation of the
line."
  (cond ((and implicit-layout-active
              (eq current-token 'end-tokens))
         'end-tokens)
        ((and implicit-layout-active
              (eq current-token 'layout-end))
         (cond ((> layout-indent (current-column))
                'layout-end)
               ((= layout-indent (current-column))
                (setq current-token 'layout-item))
               ((< layout-indent (current-column))
                (setq current-token (purs-indentation-peek-token)))))
        ((and implicit-layout-active
              (eq current-token 'layout-item))
         (setq current-token (purs-indentation-peek-token)))
        ((and implicit-layout-active
              (> layout-indent (current-column)))
         (setq current-token 'layout-end))
        (t
         (setq previous-token (purs-indentation-peek-token))
         (purs-indentation-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (and (not (eobp))
                              (= (point) indentation-point))
                         (purs-indentation-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (current-column) (purs-indentation-current-indentation))
             ;; on a new line
             (setq current-indent (current-column)))
           (cond ((and implicit-layout-active
                       (> layout-indent (current-column)))
                  (setq current-token 'layout-end))
                 ((and implicit-layout-active
                       (= layout-indent (current-column)))
                  (setq current-token 'layout-item))
                 (t (setq current-token (purs-indentation-peek-token))))))))

(defun purs-indentation-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|signature\\|deriving\\|import\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\|=>\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\|⇒\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok purs-indentation-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun purs-indentation-skip-token ()
  "Skip to the next token."
  (if (purs-lexeme-looking-at-token)
      (goto-char (match-end 0))
    ;; otherwise skip until space found
    (skip-syntax-forward "^-"))
  ;; we have to skip unterminated string fence at the end of line
  (skip-chars-forward "\n")
  (forward-comment (buffer-size)))

(provide 'purs-indentation)

;; Local Variables:
;; tab-width: 8
;; End:

;;; purs-indentation.el ends here
