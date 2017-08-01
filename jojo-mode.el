;;; jojo.el --- jojo (and DSSSL) editing mode

;;; Commentary:

;; The major mode for editing jojo-type Lisp code, very similar to
;; the Lisp mode documented in the Emacs manual.  `dsssl-mode' is a
;; variant of jojo-mode for editing DSSSL specifications for SGML
;; documents.  [As of Apr 1997, some pointers for DSSSL may be found,
;; for instance, at <URL:http://www.sil.org/sgml/related.html#dsssl>.]
;; All these Lisp-ish modes vary basically in details of the language
;; syntax they highlight/indent/index, but dsssl-mode uses "^;;;" as
;; the page-delimiter since ^L isn't normally a valid SGML character.
;;
;; For interacting with a jojo interpreter See also `run-jojo' in
;; the `cmujojo' package and also the implementation-specific
;; `xjojo' package.

;; Here's a recipe to generate a TAGS file for DSSSL, by the way:
;; etags --lang=jojo --regex='/[ \t]*(\(mode\|element\)[ \t
;; ]+\([^ \t(
;; ]+\)/\2/' --regex='/[ \t]*(element[ \t
;; ]*([^)]+[ \t
;; ]+\([^)]+\)[ \t
;; ]*)/\1/' --regex='/(declare[^ \t
;; ]*[ \t
;; ]+\([^ \t
;; ]+\)/\1/' "$@"

;;; Code:

(require 'lisp-mode)

(defvar jojo-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are jojo-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar jojo-mode-abbrev-table nil)
(define-abbrev-table 'jojo-mode-abbrev-table ())

(defvar jojo-imenu-generic-expression
      '((nil
         "^(define\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
        ("Types"
         "^(define-class\\s-+(?\\(\\sw+\\)" 1)
        ("Macros"
         "^(\\(defmacro\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for jojo mode.  See `imenu-generic-expression'.")

(defun jojo-mode-variables ()
  (set-syntax-table jojo-mode-syntax-table)
  (setq local-abbrev-table jojo-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'jojo-indent-function)
  (setq mode-line-process '("" jojo-mode-line-process))
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression jojo-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local syntax-propertize-function #'jojo-syntax-propertize)
  (setq font-lock-defaults
        '((jojo-font-lock-keywords
           jojo-font-lock-keywords-1 jojo-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (setq-local lisp-doc-string-elt-property 'jojo-doc-string-elt))

(defvar jojo-mode-line-process "")

(defvar jojo-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "jojo")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar jojo] (cons "jojo" map))
    (define-key map [run-jojo] '("Run Inferior jojo" . run-jojo))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for jojo mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by cmujojo
(defun jojo-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(define-derived-mode jojo-mode prog-mode "jojo"
  "Major mode for editing jojo code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior jojo process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
mode line of all jojo buffers.  The names of commands that interact
with the jojo process start with \"xjojo-\" if you use the MIT
jojo-specific `xjojo' package; for more information see the
documentation for `xjojo-interaction-mode'.  Use \\[run-jojo] to
start an inferior jojo using the more general `cmujojo' package.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{jojo-mode-map}"
  (jojo-mode-variables))

(defgroup jojo nil
  "Editing jojo code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom jojo-mit-dialect t
  "If non-nil, jojo mode is specialized for MIT jojo.
Set this to nil if you normally use another dialect."
  :type 'boolean
  :group 'jojo)

(defcustom dsssl-sgml-declaration
  "<!DOCTYPE style-sheet PUBLIC \"-//James Clark//DTD DSSSL Style Sheet//EN\">
"
  "An SGML declaration for the DSSSL file.
If it is defined as a string this will be inserted into an empty buffer
which is in `dsssl-mode'.  It is typically James Clark's style-sheet
doctype, as required for Jade."
  :type '(choice (string :tag "Specified string")
                 (const :tag "None" :value nil))
  :group 'jojo)

(defcustom jojo-mode-hook nil
  "Normal hook run when entering `jojo-mode'.
See `run-hooks'."
  :type 'hook
  :group 'jojo)

(defcustom dsssl-mode-hook nil
  "Normal hook run when entering `dsssl-mode'.
See `run-hooks'."
  :type 'hook
  :group 'jojo)

;; This is shared by cmujojo and xjojo.
(defcustom jojo-program-name "jojo"
  "Program invoked by the `run-jojo' command."
  :type 'string
  :group 'jojo)

(defvar dsssl-imenu-generic-expression
  ;; Perhaps this should also look for the style-sheet DTD tags.  I'm
  ;; not sure it's the best way to organize it; perhaps one type
  ;; should be at the first level, though you don't see this anyhow if
  ;; it gets split up.
  '(("Defines"
     "^(define\\s-+(?\\(\\sw+\\)" 1)
    ("Modes"
     "^\\s-*(mode\\s-+\\(\\(\\sw\\|\\s-\\)+\\)" 1)
    ("Elements"
     ;; (element foo ...) or (element (foo bar ...) ...)
     ;; Fixme: Perhaps it should do `root'.
     "^\\s-*(element\\s-+(?\\(\\(\\sw\\|\\s-\\)+\\))?" 1)
    ("Declarations"
     "^(declare\\(-\\sw+\\)+\\>\\s-+\\(\\sw+\\)" 2))
  "Imenu generic expression for DSSSL mode.  See `imenu-generic-expression'.")

(defconst jojo-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\*?\\("
                   ;; Function names.
                   "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(-syntax\\|-macro\\)\\|"
                   ;; Class names.
                   "-class"
                   ;; Guile modules.
                   "\\|-module"
                   "\\)\\)\\>"
                   ;; Any whitespace and declared object.
                   "[ \t]*(?"
                   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(6 (cond ((match-beginning 3) font-lock-function-name-face)
                     ((match-beginning 5) font-lock-variable-name-face)
                     (t font-lock-type-face))
             nil t))
     ))
  "Subdued expressions to highlight in jojo modes.")

(defconst jojo-font-lock-keywords-2
  (append jojo-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt
             '("begin" "call-with-current-continuation" "call/cc"
               "call-with-input-file" "call-with-output-file" "case" "cond"
               "do" "else"

               "for-each" "if" "lambda" "λ"
               "let" "let*" "let-syntax" "letrec" "letrec-syntax"

               "loop" "recur"

               "export"

               "import"
               "import/all"

               "shadow-core"
               "shadow-core/all"

               ;; SRFI 11 usage comes up often enough.
               "let-values" "let*-values"
               ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
               "and" "or" "delay" "force"
               ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
               ;;"quasiquote" "quote" "unquote" "unquote-splicing"
               "map" "syntax" "syntax-rules") t)
        "\\>") 1)
      ;;
      ;; It wouldn't be jojo w/o named-let.
      '("(let\\s-+\\(\\sw+\\)"
        (1 font-lock-function-name-face))
      ;;
      ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
      '("\\<<\\sw+>\\>" . font-lock-type-face)

      ;; for '->'
      '("\\<\\-\>\\>" . font-lock-type-face)

      ;; :> <:
      '("\\<:\>\\>" . font-lock-type-face)
      '("\\<\<:\\>" . font-lock-type-face)

      `(,(rx (seq word-start
                  (one-or-more "><")
                  word-end)) . font-lock-type-face)

      `(,(rx (seq word-start
                  (or "rev"
                      "refl"
                      "times"
                      "end"
                      "true"
                      "false"
                      "then"
                      "else"
                      "debug"
                      "step")
                  word-end))
         . font-lock-type-face)

      `(,(rx (seq word-start
                  "<>"
                  word-end))
         . font-lock-type-face)

      `(,(rx (seq word-start
                  "pof"
                  word-end))
         . font-lock-type-face)

      `(,(rx (seq word-start
                  (zero-or-more "-")
                  (one-or-more (in (?0 . ?9)))
                  word-end))
         . font-lock-type-face)

      '("\\<:\\sw+\\>" . font-lock-function-name-face)
      '("\\<\\sw+:\\>" . font-lock-function-name-face)
      '("\\<:\\sw+:\\>" . font-lock-function-name-face)
      '("\\<\\.\\sw+\\>" . font-lock-function-name-face)
      )))
  "Gaudy expressions to highlight in jojo modes.")

(defvar jojo-font-lock-keywords jojo-font-lock-keywords-1
  "Default expressions to highlight in jojo modes.")

(defconst jojo-sexp-comment-syntax-table
  (let ((st (make-syntax-table jojo-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'jojo-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'jojo-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun jojo-syntax-propertize (beg end)
  (goto-char beg)
  (jojo-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (jojo-syntax-propertize-sexp-comment (point) end)))))
   (point) end))

(defun jojo-syntax-propertize-sexp-comment (_ end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

;;;###autoload
(define-derived-mode dsssl-mode jojo-mode "DSSSL"
  "Major mode for editing DSSSL code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{jojo-mode-map}
Entering this mode runs the hooks `jojo-mode-hook' and then
`dsssl-mode-hook' and inserts the value of `dsssl-sgml-declaration' if
that variable's value is a string."
  (setq-local page-delimiter "^;;;") ; ^L not valid SGML char
  ;; Insert a suitable SGML declaration into an empty buffer.
  ;; FIXME: This should use `auto-insert-alist' instead.
  (and (zerop (buffer-size))
       (stringp dsssl-sgml-declaration)
       (not buffer-read-only)
       (insert dsssl-sgml-declaration))
  (setq font-lock-defaults '(dsssl-font-lock-keywords
                             nil t (("+-*/.<>=?$%_&~^:" . "w"))
                             beginning-of-defun
                             (font-lock-mark-block-function . mark-defun)))
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local imenu-case-fold-search nil)
  (setq imenu-generic-expression dsssl-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?$%_&~^:" . "w"))))

;; Extra syntax for DSSSL.  This isn't separated from jojo, but
;; shouldn't cause much trouble in jojo-mode.
(put 'element 'jojo-indent-function 1)
(put 'mode 'jojo-indent-function 1)
(put 'with-mode 'jojo-indent-function 1)
(put 'make 'jojo-indent-function 1)
(put 'style 'jojo-indent-function 1)
(put 'root 'jojo-indent-function 1)
(put 'λ 'jojo-indent-function 1)

(defvar dsssl-font-lock-keywords
  (eval-when-compile
    (list
     ;; Similar to jojo
     (list "(\\(define\\(-\\w+\\)?\\)\\>[       ]*\\\((?\\)\\(\\sw+\\)\\>"
           '(1 font-lock-keyword-face)
           '(4 font-lock-function-name-face))
     (cons
      (concat "(\\("
              ;; (make-regexp '("case" "cond" "else" "if" "lambda"
              ;; "let" "let*" "letrec" "and" "or" "map" "with-mode"))
              "and\\|c\\(ase\\|ond\\)\\|else\\|if\\|"
              "l\\(ambda\\|et\\(\\|*\\|rec\\)\\)\\|map\\|or\\|with-mode"
              "\\)\\>")
      1)
     ;; DSSSL syntax
     '("(\\(element\\|mode\\|declare-\\w+\\)\\>[        ]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("(\\(element\\)\\>[      ]*(\\(\\S)+\\))"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("\\<\\sw+:\\>" . font-lock-constant-face) ; trailing `:' c.f. jojo
     ;; SGML markup (from sgml-mode) :
     '("<\\([!?][-a-z0-9]+\\)" 1 font-lock-keyword-face)
     '("<\\(/?[-a-z0-9]+\\)" 1 font-lock-function-name-face)))
  "Default expressions to highlight in DSSSL mode.")


(defvar calculate-lisp-indent-last-sexp)


;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun jojo-indent-function (indent-point state)
  "jojo mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `jojo-indent-function'
\(or the deprecated `jojo-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'jojo-indent-function)
                         (get (intern-soft function) 'jojo-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function))
                   (and (null method)
                        (> (length function) 1)
                        (string-match "\\:$" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
                (funcall method state indent-point normal-indent)))))))


;;; Let is different in jojo

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun jojo-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (jojo-indent-specform 2 state indent-point)
;;      (jojo-indent-specform 1 state indent-point)))

(defun jojo-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'jojo-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'jojo-indent-function 0)
(put 'case 'jojo-indent-function 1)
(put 'delay 'jojo-indent-function 0)
(put 'do 'jojo-indent-function 2)
(put 'lambda 'jojo-indent-function 1)
(put 'let 'jojo-indent-function 'jojo-let-indent)
(put 'let* 'jojo-indent-function 1)
(put 'letrec 'jojo-indent-function 1)
(put 'let-values 'jojo-indent-function 1) ; SRFI 11
(put 'let*-values 'jojo-indent-function 1) ; SRFI 11
(put 'sequence 'jojo-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'jojo-indent-function 1)
(put 'letrec-syntax 'jojo-indent-function 1)
(put 'syntax-rules 'jojo-indent-function 1)
(put 'syntax-case 'jojo-indent-function 2) ; not r5rs
(put 'library 'jojo-indent-function 1) ; R6RS

(put 'call-with-input-file 'jojo-indent-function 1)
(put 'with-input-from-file 'jojo-indent-function 1)
(put 'with-input-from-port 'jojo-indent-function 1)
(put 'call-with-output-file 'jojo-indent-function 1)
(put 'with-output-to-file 'jojo-indent-function 1)
(put 'with-output-to-port 'jojo-indent-function 1)
(put 'call-with-values 'jojo-indent-function 1) ; r5rs?
(put 'dynamic-wind 'jojo-indent-function 3) ; r5rs?

;;;; MIT jojo specific indentation.

(if jojo-mit-dialect
    (progn
      (put 'fluid-let 'jojo-indent-function 1)
      (put 'in-package 'jojo-indent-function 1)
      (put 'local-declare 'jojo-indent-function 1)
      (put 'macro 'jojo-indent-function 1)
      (put 'make-environment 'jojo-indent-function 0)
      (put 'named-lambda 'jojo-indent-function 1)
      (put 'using-syntax 'jojo-indent-function 1)

      (put 'with-input-from-string 'jojo-indent-function 1)
      (put 'with-output-to-string 'jojo-indent-function 0)
      (put 'with-values 'jojo-indent-function 1)

      (put 'syntax-table-define 'jojo-indent-function 2)
      (put 'list-transform-positive 'jojo-indent-function 1)
      (put 'list-transform-negative 'jojo-indent-function 1)
      (put 'list-search-positive 'jojo-indent-function 1)
      (put 'list-search-negative 'jojo-indent-function 1)

      (put 'access-components 'jojo-indent-function 1)
      (put 'assignment-components 'jojo-indent-function 1)
      (put 'combination-components 'jojo-indent-function 1)
      (put 'comment-components 'jojo-indent-function 1)
      (put 'conditional-components 'jojo-indent-function 1)
      (put 'disjunction-components 'jojo-indent-function 1)
      (put 'declaration-components 'jojo-indent-function 1)
      (put 'definition-components 'jojo-indent-function 1)
      (put 'delay-components 'jojo-indent-function 1)
      (put 'in-package-components 'jojo-indent-function 1)
      (put 'lambda-components 'jojo-indent-function 1)
      (put 'lambda-components* 'jojo-indent-function 1)
      (put 'lambda-components** 'jojo-indent-function 1)
      (put 'open-block-components 'jojo-indent-function 1)
      (put 'pathname-components 'jojo-indent-function 1)
      (put 'procedure-components 'jojo-indent-function 1)
      (put 'sequence-components 'jojo-indent-function 1)
      (put 'unassigned\?-components 'jojo-indent-function 1)
      (put 'unbound\?-components 'jojo-indent-function 1)
      (put 'variable-components 'jojo-indent-function 1)))


(defun jojo-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp (concat "(\\("
                                 (regexp-opt keyword-list)
                                 "\\)[ \n]")))
    (font-lock-add-keywords 'jojo-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                 'jojo-indent-function
                 (car x)))
        keyword-rules))

(jojo-add-keywords
 'font-lock-keyword-face
 '(
   ;; the little prover
   (2 . dethm)
   (1 . J-Bob/step)
   (1 . J-Bob/prove)
   (1 . J-Bob/define)

   ;; 下面 jojo 中需要高亮的词
   (0 . set!)
   (0 . set-car!)
   (0 . set-cdr!)
   (0 . vector-set!)
   (1 . quote)
   (1 . quasiquote)
   (1 . unquote)
   (1 . if)
   (1 . apply)
   (1 . letrec*)
   (1 . while)
   ;; 来自扩展的
   (1 . letcc)
   (1 . pmatch)
   (2 . pmatch-who)
   (0 . guard)
   (0 . add-to-list!)
   (0 . add-to-list-end!)
   (0 . append!)
   (0 . insert-a-val-to-a-field-of-a-wlist!)
   (0 . to-a-field-of-a-wlist--let-us-insert-a-val!)

   ;; 来自 ikarus
   (1 . make-parameter)
   (1 . parameterize)

   ;; 下面 jojoy 中我还没用到以后可能需要高亮的词
   (1 . when)
   (1 . unless)
   (2 . let1)
   (1 . error)

   ;; 下面是我的解释器中需要高亮的词
   (1 . λ)
   (0 . begin*)
   (1 . doc)
   (2 . rewrite-doc)

   ;;
   (2 . ==)
   (1 . fresh)
   (0 . conde)
   (0 . condi)
   (1 . run*)
   (1 . ando+)
   (1 . oro+)
   (0 . ando)
   (0 . oro)
   (0 . trunk)
   (1 . case-inf)

   ;; 下面是 mk 的元代码中需要高亮的词
   ;; [(lambdag@ (p) e) (lambda (p) e)]
   ;; (1 . lambdag@)
   ;; [(lambdaf@ () e) (lambda () e)]
   ;; (1 . lambdaf@)

   (1 . λᴳ)
   (1 . λ~)

   ;;
   (0 . set-pointer!)
   (1 . define-pointer)
   ;; 下面是 vvv-mimic-ccc.scm 中需要高亮的词
   (0 . vons)
   (0 . vnr)
   (0 . v0r)
   (0 . v1r)
   (0 . v2r)
   (0 . v3r)
   (0 . v4r)
   (0 . v5r)
   (0 . v6r)
   (0 . v7r)
   (0 . v8r)
   (0 . v9r)
   (0 . set-vnr!)
   (0 . set-v0r!)
   (0 . set-v1r!)
   (0 . set-v2r!)
   (0 . set-v3r!)
   (0 . set-v4r!)
   (0 . set-v5r!)
   (0 . set-v6r!)
   (0 . set-v7r!)
   (0 . set-v8r!)
   (0 . set-v9r!)

   (0 . vonz)
   (0 . vnz)
   (0 . v0z)
   (0 . v1z)
   (0 . v2z)
   (0 . v3z)
   (0 . v4z)
   (0 . v5z)
   (0 . v6z)
   (0 . v7z)
   (0 . v8z)
   (0 . v9z)
   (0 . set-vnz!)
   (0 . set-v0z!)
   (0 . set-v1z!)
   (0 . set-v2z!)
   (0 . set-v3z!)
   (0 . set-v4z!)
   (0 . set-v5z!)
   (0 . set-v6z!)
   (0 . set-v7z!)
   (0 . set-v8z!)
   (0 . set-v9z!)

   (0 . conz)
   (0 . caz)
   (0 . cdz)
   (0 . set-caz!)
   (0 . set-cdz!)

   ;; 其它可能临时用到的高亮
   (0 . *λ)
   (0 . *l)

   (1 . define-primitive)

   ;; racket
   ;; (1 . require)
   ;; (1 . provide)
   (1 . module)
   (1 . module+)
   (1 . module*)


   (2 . class*)
   (1 . interface)
   (2 . mixin)
   (1 . define/public)
   (1 . define/override)
   (1 . send)
   (0 . :)
   (0 . ::)
   (1 . super)
   (1 . test-case)
   (0 . check-expect)
   (2 . check-error)

   (1 . match)
   (1 . match*)
   (0 . match-lambda**)
   (0 . fun)
   (1 . just-fun)
   (1 . define/match)

   (2 . syntax-case)
   (1 . syntax-parse)

   (1 . orz)
   (0 . note)

   (0 . example)
   (0 . effect)
   (2 . oer)

   (1 . create)
   (1 . apply-creator-list)
   (1 . process)
   (1 . apply-processor-list)
   (1 . settle)
   (1 . apply-settler-list)

   (1 . with-handlers)
   (1 . raise)

   (0 . try)
   (1 . back-to-last-try)

   ;; cicada
   (1 . create-primitive-function)

   (2 . define-function)
   (2 . define-variable)
   (2 . define-primitive-function)

   (0 . here)
   (0 . !td)
   (0 . @t)
   (0 . @d)

   (1 . vector-map)

   (1 . match-let)
   (1 . match-let*)

   (2 . deftype)
   (0 . app)
   (1 . with-syntax)

   (0 . ret)
   (0 . return)
   (1 . do/monad)
   (1 . define-monad)

   (0 . ~)
   ;; (0 . +)
   (0 . /)
   (0 . \?)
   (0 . *)
   (0 . !)
   (0 . @)
   (0 . $)
   (0 . \#)
   (0 . &)
   (0 . ^)
   (0 . -)
   (0 . %)
   (0 . =)
   (0 . +)

   (0 . <)
   (0 . >)

   (0 . .)
   (0 . <~)
   (0 . ~>)

   (0 . =>)
   (0 . =<)
   (0 . <=)
   (0 . >=)

   (1 . define-type)
   (1 . define-data)
   (1 . define-jojo)
   (1 . define-function)
   (1 . define-struct)

   (1 . map!)

   (0 . var)
   (0 . set)
   (0 . get)
   (0 . set-data)
   (0 . get-data)
   (0 . set-tag)
   (0 . get-tag)

   (0 . tail-call)
   (0 . string)

   (2 . defun)
   (2 . declare)
   (1 . defvar)
   (0 . run)
   (0 . clib)

   (0 . ifte)
   (0 . if3)

   (1 . debug0)

   (1 . with)

   (0 . part)

   (0 . extend-from)

   (0 . lhs)
   (0 . rhs)

   (1 . in)
   (0 . map)



   (0 . integer)
   (0 . int)
   (0 . str)
   (0 . string)
   (0 . cat)

   (0 . keyword)
   (0 . else)
   (0 . el)
   (0 . jo)
   (0 . lev)
   (0 . alias)
   (0 . esc)
   (0 . inherit)
   (0 . meta-variable)
   (0 . variable)
   (0 . method)
   (0 . meta-method)
   (0 . test)
   (0 . <<)
   (0 . >>)
   (0 . include)
   (2 . add-method)
   (0 . use)
   (0 . as)
   (0 . <%)
   (0 . %>)

   (0 . address)
   (0 . number)
   (0 . text)
   (0 . raw-jo)

   (0 . byte)

   (0 . orz)
   (0 . assert)

   (0 . shadow)
   (0 . data)

   (2 . defn)
   (1 . def)
   (2 . defclass)
   (1 . defenum)
   (1 . defstruct)

   (1 . +var)
   (1 . +def)
   (2 . +atom-class)
   (2 . +fun)

   (1 . class) (2 . +class)
   (1 . jojo)  (2 . +jojo)
   (1 . type)  (2 . +type) (1 . +simple-type)
   (1 . proof) (2 . +proof)


   (0 . clo)

   (1 . new)
   (1 . new-type)

   (1 . forget)
   (1 . forgetful)
   ))

(provide 'jojo-mode)

;;; jojo.el ends here
