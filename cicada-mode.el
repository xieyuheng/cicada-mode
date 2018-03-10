;;; cicada-mode.el --- Major mode for cicada code

;; copy from clojure-mode.el

(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp))

(require 'cl-lib)
(require 'align)
(require 'xyhlib)

(defgroup cicada nil
  "Major mode for editing cicada code."
  :prefix "cicada-"
  :group 'languages)

(defconst cicada-mode-version "5.7.0-snapshot"
  "The current version of `cicada-mode'.")

(defcustom cicada-indent-style :always-align
  "Indentation style to use for function forms and macro forms.
There are two cases of interest configured by this variable.

- Case (A) is when at least one function argument is on the same
  line as the function name.
- Case (B) is the opposite (no arguments are on the same line as
  the function name).  Note that the body of macros is not
  affected by this variable, it is always indented by
  `lisp-body-indent' (default 2) spaces.

Note that this variable configures the indentation of function
forms (and function-like macros), it does not affect macros that
already use special indentation rules.

The possible values for this variable are keywords indicating how
to indent function forms.

    `:always-align' - Follow the same rules as `lisp-mode'.  All
    args are vertically aligned with the first arg in case (A),
    and vertically aligned with the function name in case (B).
    For instance:
        (reduce merge
                some-coll)
        (reduce
         merge
         some-coll)

    `:always-indent' - All args are indented like a macro body.
        (reduce merge
          some-coll)
        (reduce
          merge
          some-coll)

    `:align-arguments' - Case (A) is indented like `lisp', and
    case (B) is indented like a macro body.
        (reduce merge
                some-coll)
        (reduce
          merge
          some-coll)"
  :safe #'keywordp
  :type '(choice (const :tag "Same as `lisp-mode'" :always-align)
          (const :tag "Indent like a macro body" :always-indent)
          (const :tag "Indent like a macro body unless first arg is on the same line"
           :align-arguments))
  :package-version '(cicada-mode . "5.2.0"))

(define-obsolete-variable-alias 'cicada-defun-style-default-indent
    'cicada-indent-style "5.2.0")

(defcustom cicada-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom cicada-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

(defcustom cicada-build-tool-files '("project.es" "build.boot" "build.gradle")
  "A list of files, which identify a cicada project's root.
Out-of-the box `cicada-mode' understands lein, boot and gradle."
  :type '(repeat string)
  :package-version '(cicada-mode . "5.0.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defvar cicada-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c SPC") #'cicada-align)
    (easy-menu-define cicada-mode-menu map "cicada Mode Menu"
                      '("cicada"))
    map)
  "Keymap for cicada mode.")

(make-syntaxes
 cicada-mode-syntax-table
 ;; note that, if modify one syntax entry twice
 ;; the second will shadow the first
 ;; whitespace characters:
 ;; (   '(0 . 32)    "-"  )
 ;; (      127       "-"  )
 ( ?\t "    " )
 ( ?\n ">   " )
 ( ?\f "    " )
 ( ?\r "    " )
 ( ?\s "    " )

 (   ?\.        "_p"  )
 (   ?\,        "_p"  )
 (   ?\-        "_p"  )
 (   ?\_        "_p"  )
 (   ?\/        "_p"  )

 (   ?\"        "\"   ")
 (   ?'         "'   ")
 (   ?`         "'   ")
 (   ?\;        "<   ")

 ;; symbol constituent:
 ;; the following functions need this:
 ;; ``forward-word'' and so on ...
 ;; (  '(33 . 47)    "_"  )
 ;; (  '(58 . 64)    "_"  )
 ;; (  '(91 . 96)    "_"  )
 ;; ( '(123 . 126)   "_"  )
 ;; open/close delimiter:
 ;; the following functions need this:
 ;; ``forward-sexp'' ``backward-sexp''
 ;; ``mark-sexp'' and so on ...
 (  ?\(    "("  )
 (  ?\)    ")"  )
 (  ?\[    "("  )
 (  ?\]    ")"  )
 (  ?\{    "("  )
 (  ?\}    ")"  ))

(defun cicada-mode-variables ()
  "Set up initial buffer-local variables for cicada mode."
  (set-syntax-table cicada-mode-syntax-table)
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)

  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local parse-sexp-ignore-comments t)

  (setq-local indent-line-function #'cicada-indent-line)
  (setq-local indent-region-function #'cicada-indent-region)
  (setq-local lisp-indent-function #'cicada-indent-function))

;;;###autoload
(define-derived-mode cicada-mode
    prog-mode "cicada"
    "Major mode for editing cicada code."
    (cicada-mode-variables)
    (cicada-font-lock-setup))



(defvar cicada-font-lock-keywords
  `(
    ;; 0123456789
    (,(rx (seq symbol-start
               (group (one-or-more (in (?0 . ?9))))
               word-end))
      (1 font-lock-constant-face))

    (,(rx (group ","))
      (1 font-lock-keyword-face))

    (,(rx symbol-start
          (group "#"
                 (zero-or-more (not blank))
                 "!")
          word-end)
      (1 font-lock-variable-name-face))

    (,(rx symbol-start
          (group "#"
                 (zero-or-more (not blank)))
          word-end)
      (1 font-lock-constant-face))

    (,(rx symbol-start
          (group "@")
          word-end)
      (1 font-lock-constant-face))

    (,(rx symbol-start
          (group "λ")
          word-end)
      (1 font-lock-constant-face))

    (,(rx symbol-start
          (group "%")
          word-end)
      (1 font-lock-constant-face))

    ;; +def
    (,(rx symbol-start
          (group "+" (one-or-more (not blank)))
          word-end)
      (1 font-lock-keyword-face))

    ;; <type>
    (,(rx symbol-start
          (group "<" (one-or-more (not blank)) ">")
          word-end)
      (1 font-lock-type-face))

    ;; infix
    (,(rx symbol-start
          (group (or ":"
                     "::"
                     "<"
                     ">"
                     "<:"
                     ":>"
                     "="
                     ":="
                     "=:"))
          word-end)
      (1 font-lock-variable-name-face))

    ;; Class
    (,(rx symbol-start
          (group (in (?A . ?Z))
                 (zero-or-more (not blank)))
          word-end)
      (1 font-lock-type-face))

    ;; @fun
    (,(rx symbol-start
          (group "@" (one-or-more (not blank)))
          word-end)
      (1 font-lock-preprocessor-face))

    ;; name!
    (,(rx symbol-start
          (group (one-or-more (not blank)) "!")
          word-end)
      (1 font-lock-variable-name-face))

    ;; module-name:name
    (,(rx symbol-start
          (group (one-or-more (not (in ": \t")))
                 ":")
          (group (one-or-more (not blank)))
          word-end)
      (1 font-lock-type-face))

    ;; name.field
    (,(rx symbol-start
          (group (one-or-more (not (in ". \t"))))
          (group "." (one-or-more (not blank)))
          word-end)
      (2 font-lock-constant-face))

    ;; :local-name.field-name
    ;; :local-name.field-name.field-name
    (,(rx symbol-start
          (group ":" (one-or-more (not (in ". \t"))))
          (group "." (one-or-more (not blank)))
          word-end)
      (1 font-lock-preprocessor-face)
      (2 font-lock-constant-face))

    ;; :local-name
    (,(rx symbol-start
          (group ":" (one-or-more (not (in ". \t"))))
          word-end)
      (1 font-lock-preprocessor-face))

    ;; .field-name
    ;; .field-name.field-name
    (,(rx symbol-start
          (group "." (one-or-more (not blank)))
          word-end)
      (1 font-lock-constant-face))

    (,(rx symbol-start
          (group (or "--"
                     "=="
                     "=>"
                     "->"
                     "<="
                     "<-"
                     "|"
                     "+"
                     "rev"
                     "default"
                     "refl"
                     "><"
                     "><><"
                     "><><><"
                     "times"
                     "end"
                     "bye"
                     "recur"
                     "loop"
                     "true"
                     "false"
                     "then"
                     "else"
                     "yield"

                     "receive"
                     "send"
                     "spawn"

                     "emit"
                     "give"
                     "debug"
                     "step"
                     "quote"))
          word-end)
      (1 font-lock-type-face))

    ;; type-t[t]*
    (,(rx symbol-start
          (group (one-or-more (not blank)) "-" (one-or-more "t"))
          word-end)
      (1 font-lock-type-face))

    ;; class-tc
    (,(rx symbol-start
          (group (one-or-more (not blank)) "-tc")
          word-end)
      (1 font-lock-type-face))

    ;; cons-c create-cr
    (,(rx symbol-start
          (group (one-or-more (not blank))
                 (or "-c"
                     "-cr"))
          word-end)
      (1 font-lock-variable-name-face))

    ;; clone
    (,(rx symbol-start
          (group "clone")
          word-end)
      (1 font-lock-variable-name-face))

    ;; quotient-q
    (,(rx symbol-start
          (group (one-or-more (not blank)) "-q")
          word-end)
      (1 font-lock-type-face))

    ;; union-u
    (,(rx symbol-start
          (group (one-or-more (not blank)) "-u")
          word-end)
      (1 font-lock-type-face))

    ;; keyword
    (,(rx symbol-start
          (group (or
                  "import"
                  "as"
                  "error"
                  "match"
                  "case"
                  "if"))
          word-end)
      (1 font-lock-keyword-face))

    ;; (keyword ...)
    (,(rx "("
          (group (or "~" "=" "+" "*" ":"
                     "do" "el" "if" "throw" "try" "catch" "finally"
                     "set!" "new" "."
                     (seq "lit/" (one-or-more (not blank)))))
          word-end)
      (1 font-lock-keyword-face))

    ;; Dynamic variables - *something*
    ("\\(?:\\<\\|/\\)\\(\\*[a-z-]*\\*\\)\\>"
     1 font-lock-variable-name-face)

    ;; prefix
    ;; (,(rx symbol-start
    ;;       (group (or "return"))
    ;;       word-end)
    ;;   (1 font-lock-variable-name-face))

    (,(rx (minimal-match
           (seq word-start
                (group "\""
                       (one-or-more (not (in 34)))
                       "\"")
                word-end)))
      (1 font-lock-string-face)))

  "Default expressions to highlight in cicada mode.")


(defun cicada-font-lock-setup ()
  "Configures font-lock for editing cicada code."
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(cicada-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$#%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun))))

;;; Indentation
(defun cicada-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`cicada-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))))

(defun cicada-indent-line ()
  "Indent current line as cicada code."
  (lisp-indent-line))

(defvar cicada-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `cicada-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `cicada-indent-function'
for more information.")

(defun cicada--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `cicada-get-indent-function', then try the
`cicada-indent-function' and `cicada-backtracking-indent'
symbol properties."
  (or (when (functionp cicada-get-indent-function)
        (funcall cicada-get-indent-function function-name))
      (get (intern-soft function-name) 'cicada-indent-function)
      (get (intern-soft function-name) 'cicada-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'cicada-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'cicada-backtracking-indent)))
      (when (string-match (rx (or "let" "unless" "when" "while") (syntax symbol))
                          function-name)
        (cicada--get-indent-method (substring (match-string 0 function-name) 0 -1)))))

(defvar cicada--current-backtracking-depth 0)

(defun cicada--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `cicada--find-indent-spec'."
  (when (and (>= cicada-max-backtracking cicada--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((cicada--current-backtracking-depth (1+ cicada--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (cicada-backward-logical-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (cicada--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                               (goto-char start)
                               (cicada--find-indent-spec-backtracking)))))))
        (when (numberp method)
          (setq method (list method)))
        (pcase method
          ((pred functionp)
           (when (= pos 0)
             method))
          ((pred sequencep)
           (pcase (length method)
             (`0 nil)
             (`1 (let ((head (elt method 0)))
                   (when (or (= pos 0) (sequencep head))
                     head)))
             (l (if (>= pos l)
                    (elt method (1- l))
                  (elt method pos)))))
          ((or `defun `:defn)
           (when (= pos 0)
             :defn))
          (_
           (message "Invalid indent spec for `%s': %s" function method)
           nil))))))

(defun cicada--find-indent-spec ()
  "Return the indent spec that applies to current sexp.
If `cicada-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if cicada-use-backtracking-indent
      (save-excursion
        (cicada--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (cicada--get-indent-method function))))

(defun cicada--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `cicada-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (cicada-backward-logical-sexp 1)
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match
                  "[^[:blank:]]"
                  (buffer-substring (line-beginning-position) (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point is now at
      ;; the function name), so the behaviour depends on INDENT-MODE and on
      ;; whether there's also an argument on this line (case A or B).
      (let ((case-a ; The meaning of case-a is explained in `cicada-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
          ;; For compatibility with the old `cicada-defun-style-default-indent', any
          ;; value other than these 3 is equivalent to `always-body'.
          ((not (memq indent-mode '(:always-align :align-arguments nil)))
           (+ (current-column) lisp-body-indent -1))
          ;; There's an arg after the function name, so align with it.
          (case-a (goto-char last-sexp-start)
                  (current-column))
          ;; Not same line.
          ((eq indent-mode :align-arguments)
           (+ (current-column) lisp-body-indent -1))
          ;; Finally, just align with the function name.
          (t (current-column)))))))

(defun cicada--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (or (member (char-after) '(?\[ ?\{))
      (save-excursion ;; Catch #?@ (:ess ...)
        (skip-chars-backward "\r\n[:blank:]")
        (when (eq (char-before) ?@)
          (forward-char -1))
        (and (eq (char-before) ?\?)))
      ;; Car of form is not a symbol.
      (not (looking-at ".\\(?:\\sw\\|\\s_\\)"))))

;; Check the general context, and provide indentation for data structures and
;; special macros. If current form is a function (or non-special macro),
;; delegate indentation to `cicada--normal-indent'.
(defun cicada-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a cicada function with a
non-nil property `cicada-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `cicada-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (cicada--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (cicada--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (cicada-forward-logical-sexp 1)
                 (cl-incf pos))
             ;; If indent-point is _after_ the last sexp in the
             ;; current sexp, we detect that by catching the
             ;; `scan-error'. In that case, we should return the
             ;; indentation as if there were an extra sexp at point.
             (scan-error (cl-incf pos)))
           (cond
             ;; The first non-special arg. Rigidly reduce indentation.
             ((= pos (1+ method))
              (+ lisp-body-indent containing-form-column))
             ;; Further non-special args, align with the arg above.
             ((> pos (1+ method))
              (cicada--normal-indent last-sexp :always-align))
             ;; Special arg. Rigidly indent with a large indentation.
             (t
              (+ (* 2 lisp-body-indent) containing-form-column)))))
        (`:defn
         (+ lisp-body-indent containing-form-column))
        ((pred functionp)
         (funcall method indent-point state))
        ;; No indent spec, do the default.
        (`nil
         (let ((function (thing-at-point 'symbol)))
           (cond
             ;; Preserve useful alignment of :require (and friends) in `ns' forms.
             ((and function (string-match "^:" function))
              (cicada--normal-indent last-sexp :always-align))
             ;; This is should be identical to the :defn above.
             ((and function
                   (or (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                     function)
                       (string-match "\\`\\(?:\\S +/\\)?\\(else\\|with-\\)"
                                     function)
                       (string-match "\\`\\(?:\\S +/\\)?\\([^[:blank:]]*-t\\|with-\\)"
                                     function)
                       (string-match "\\`\\(?:\\S +/\\)?\\([^[:blank:]]*-u\\|with-\\)"
                                     function)
                       (string-match "\\`\\(?:\\S +/\\)?\\([^[:blank:]]*-c\\|with-\\)"
                                     function))
                   (not (string-match "\\`default" (match-string 1 function))))
              (+ lisp-body-indent containing-form-column))
             ;; Finally, nothing special here, just respect the user's
             ;; preference.
             (t (cicada--normal-indent last-sexp cicada-indent-style)))))))))

;;; setting indentation
(defun put-cicada-indent (sym indent)
  "Instruct `cicada-indent-function' to indent the body of SYM by INDENT."
  (put sym 'cicada-indent-function indent))

(put (intern "λ") 'lisp-indent-function 0)

(defmacro define-cicada-indent (&rest kvs)
  "Call `put-cicada-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x)
                 `(progn (put-cicada-indent
                          (quote ,(car x)) ,(cadr x))
                         (setq cicada-font-lock-keywords
                               (cons (list ,(concat
                                             "("
                                             (regexp-opt
                                              (list (symbol-name (car x))) t)
                                             "\\>")
                                           1 font-lock-keyword-face)
                                     cicada-font-lock-keywords))))
               kvs)))


(defun add-custom-cicada-indents (name value)
  "Allow `cicada-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-cicada-indent x 'defun))
          value))

(defcustom cicada-defun-indents nil
  "List of additional symbols with defun-style indentation in cicada.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-cicada-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-cicada-indents)

(define-cicada-indent
    ;; built-ins
    (ns 1)
    (fn :defn)
  (def :defn)
  (defn :defn)
  (begin :defn)
  (if :defn)
  (when :defn)
  (unless :defn)
  (if-not 1)
  (case :defn)
  (cond 0)
  (choice :defn)
  (^ 0)
  (- 0)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy '(2 nil nil (:defn)))
  (as-> 2)

  (reify '(:defn (1)))
  (deftype '(2 nil nil (:defn)))
  (defrecord '(2 nil nil (:defn)))
  (defprotocol '(1 (:defn)))
  (definterface '(1 (:defn)))
  (extend 1)
  (extend-protocol '(1 :defn))
  (extend-type '(1 :defn))
  ;; specify and specify! are from cicadaScript
  (specify '(1 :defn))
  (specify! '(1 :defn))
  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let :defn)
  (letfn '(1 ((:defn)) nil))
  (binding 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)

  (+class :defn) (class :defn)
  (+supertype :defn) (supertype :defn)
  (+subtype :defn) (subtype :defn)
  (+type :defn) (type :defn) (+simple-type :defn) (simple-type :defn)
  (+space :defn)
  (+fun :defn)
  (composition :defn)
  (+jojo :defn) (fun :defn)
  (+cicada :defn) (cicada :defn)
  (+union :defn) (union :defn)
  (+high :defn)  (+higher :defn)
  (high :defn)  (higher :defn)

  (loop :defn)
  (main :defn)

  (main-act :defn)

  (+generator :defn)

  (define :defn)

  (primitive :defn)

  (+method :defn) (method :defn)

  (+process :defn) (process :defn)
  (+channel :defn) (channel :defn)
  (+ch :defn) (ch :defn)
  (+proc :defn) (proc :defn)

  (create :defn)

  (clone :defn)

  (clo :defn)

  (where :defn)

  (define :defn)


  (+method :defn) (method :defn)

  (+process :defn) (process :defn)
  (+channel :defn) (channel :defn)
  (+ch :defn) (ch :defn)
  (+proc :defn) (proc :defn)

  (diff :defn)
  (letrec :defn)
  (call :defn)

  (dict :defn)
  (vect :defn)
  (stack :defn)
  (tuple :defn)
  (set :defn)

  (+macro :defn)
  (+type-alias :defn) (type-alias :defn)
  (+imp :defn) (imp :defn)
  (+member :defn)
  (+proof :defn) (proof :defn)
  (run :defn)
  (list :defn)
  (+def :defn)
  (+data :defn)

  (+var :defn)
  (+atom :defn)
  (set :defn)

  (receive :defn)
  (send :defn)

  (note :defn)
  (test :defn)
  (assert :defn)
  (assert! :defn)

  (array :defn)
  (vector :defn)

  (+gene :defn)
  (+disp :defn)
  (+disp-default :defn)

  (+var :defn)

  (forget :defn)
  (let-bind :defn)

  (type-sum :defn)
  (type-case :defn)

  (match :defn)

  (module :defn)
  (+module :defn)
  (import :defn)
  (from :defn)
  (export :defn)
  (use :defn)
  (in :defn)
  (include :defn)

  (table :defn)


  (+impl :defn)
  (+trait :defn)

  (+quotient :defn))

;;; Sexp navigation
(defun cicada--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.

\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\^\\|#[?[:alpha:]]"))

(defun cicada-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (cicada-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (cicada--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun cicada-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (cicada-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (cicada--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

(provide 'cicada-mode)
