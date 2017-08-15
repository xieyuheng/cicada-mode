;;; jojo-mode.el --- Major mode for jojo code

;; copy from clojure-mode.el

(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp))

(require 'cl-lib)
(require 'align)
(require 'xyhlib)

(defgroup jojo nil
  "Major mode for editing jojo code."
  :prefix "jojo-"
  :group 'languages)

(defconst jojo-mode-version "5.7.0-snapshot"
  "The current version of `jojo-mode'.")

(defcustom jojo-indent-style :always-align
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
  :package-version '(jojo-mode . "5.2.0"))

(define-obsolete-variable-alias 'jojo-defun-style-default-indent
    'jojo-indent-style "5.2.0")

(defcustom jojo-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom jojo-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

(defcustom jojo-build-tool-files '("project.es" "build.boot" "build.gradle")
  "A list of files, which identify a jojo project's root.
Out-of-the box `jojo-mode' understands lein, boot and gradle."
  :type '(repeat string)
  :package-version '(jojo-mode . "5.0.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defvar jojo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c SPC") #'jojo-align)
    (easy-menu-define jojo-mode-menu map "jojo Mode Menu"
                      '("jojo"))
    map)
  "Keymap for jojo mode.")

(make-syntaxes
 jojo-mode-syntax-table
 ;; note that, if modify one syntax entry twice
 ;; the second will shadow the first
 ;; whitespace characters:
 (   '(0 . 32)    "-"  )
 (      127       "-"  )
 (   ?\,        "_p"  )
 (   ?\-        "_p"  )
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

(defun jojo-mode-variables ()
  "Set up initial buffer-local variables for jojo mode."
  (set-syntax-table jojo-mode-syntax-table)
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local indent-line-function #'jojo-indent-line)
  (setq-local indent-region-function #'jojo-indent-region)
  (setq-local lisp-indent-function #'jojo-indent-function)
  (setq-local parse-sexp-ignore-comments t))

;;;###autoload
(define-derived-mode jojo-mode
    prog-mode "jojo"
    "Major mode for editing jojo code."
    (jojo-mode-variables)
    (jojo-font-lock-setup))



(defvar jojo-font-lock-keywords
  `(
    ;; 0123456789
    (,(rx (seq symbol-start
               (group (one-or-more (in (?0 . ?9))))
               word-end))
      (1 font-lock-constant-face))

    ;; ','
    (,(rx (group ","))
      (1 font-lock-keyword-face))

    ;; '#'
    (,(rx symbol-start
          (group "#")
          symbol-end)
      (1 font-lock-constant-face))

    ;; '%'
    (,(rx symbol-start
          (group "%")
          symbol-end)
      (1 font-lock-constant-face))

    ;; <type>
    (,(rx symbol-start
          (group "<" (one-or-more (not blank)) ">")
          word-end)
      (1 font-lock-type-face))

    ;; @:fun
    (,(rx symbol-start
          (group "@")
          (group ":" (one-or-more (not blank)))
          word-end)
      (1 font-lock-constant-face)
      (2 font-lock-preprocessor-face))

    ;; name!
    (,(rx symbol-start
          (group (one-or-more (not blank)) "!")
          word-end)
      (1 font-lock-variable-name-face))

    ;; local-name.field-name
    ;; local-name.field-name.field-name
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
                     "->"
                     "rev"
                     "refl"
                     "times"
                     "end"
                     "true"
                     "false"
                     "then"
                     "else"
                     "debug"
                     "step"))
          word-end)
      (1 font-lock-type-face))

    ;; (keyword ...)
    (,(rx "("
          (group (or "=" "*" ":"
                     "do" "el" "if" "loop"
                     "recur" "throw" "try" "catch" "finally"
                     "set!" "new" "." "quote"))
          word-end)
      (1 font-lock-keyword-face))

    ;; Dynamic variables - *something*
    ("\\(?:\\<\\|/\\)\\(\\*[a-z-]*\\*\\)\\>" 1 font-lock-variable-name-face)

    ;; Global constants - nil, true, false
    (,(concat
       "\\<"
       (regexp-opt
        '("true" "false") t)
       "\\>")
      0 font-lock-constant-face)

    ;; CONST SOME_CONST (optionally prefixed by /)
    ("\\(?:\\<\\|/\\)\\([A-Z]+\\|\\([A-Z]+_[A-Z1-9_]+\\)\\)\\>" 1 font-lock-constant-face))

  "Default expressions to highlight in jojo mode.")


(defun jojo-font-lock-setup ()
  "Configures font-lock for editing jojo code."
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(jojo-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$#%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun))))

;;; Vertical alignment
(defcustom jojo-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.
Automatically means it is done as part of indenting code.  This
applies to binding forms (`jojo-align-binding-forms'), to cond
forms (`jojo-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting \\<jojo-mode-map>`\\[indent-for-tab-command]'
will align the values like this:
    {:some-key 10
     :key2     20}"
  :package-version '(jojo-mode . "5.1")
  :safe #'booleanp
  :type 'boolean)

(defcustom jojo-align-binding-forms
  '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
    "doseq" "for" "with-open" "with-local-vars" "with-redefs")
  "List of strings matching forms that have binding forms."
  :package-version '(jojo-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defcustom jojo-align-cond-forms '("condp" "cond" "cond->" "cond->>" "case" "are")
  "List of strings identifying cond-like forms."
  :package-version '(jojo-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defun jojo--position-for-alignment ()
  "Non-nil if the sexp around point should be automatically aligned.
This function expects to be called immediately after an
open-brace or after the function symbol in a function call.

First check if the sexp around point is a map literal, or is a
call to one of the vars listed in `jojo-align-cond-forms'.  If
it isn't, return nil.  If it is, return non-nil and place point
immediately before the forms that should be aligned.

For instance, in a map literal point is left immediately before
the first key; while, in a let-binding, point is left inside the
binding vector and immediately before the first binding
construct."
  ;; Are we in a map?
  (or (and (eq (char-before) ?{))
      ;; Are we in a cond form?
      (let* ((fun    (car (member (thing-at-point 'symbol) jojo-align-cond-forms)))
             (method (and fun (jojo--get-indent-method fun)))
             ;; The number of special arguments in the cond form is
             ;; the number of sexps we skip before aligning.
             (skip   (cond ((numberp method) method)
                           ((null method) 0)
                           ((sequencep method) (elt method 0)))))
        (when (and fun (numberp skip))
          (jojo-forward-logical-sexp skip)
          (comment-forward (point-max))
          fun)) ; Return non-nil (the var name).
      ;; Are we in a let-like form?
      (when (member (thing-at-point 'symbol)
                    jojo-align-binding-forms)
        ;; Position inside the binding vector.
        (jojo-forward-logical-sexp)
        (backward-sexp)
        (when (eq (char-after) ?\[)
          (forward-char 1)
          (comment-forward (point-max))
          ;; Return non-nil.
          t))))

(defun jojo--find-sexp-to-align (end)
  "Non-nil if there's a sexp ahead to be aligned before END.
Place point as in `jojo--position-for-alignment'."
  ;; Look for a relevant sexp.
  (let ((found))
    (while (and (not found)
                (search-forward-regexp
                 (concat "{\\|(" (regexp-opt
                                  (append jojo-align-binding-forms
                                          jojo-align-cond-forms)
                                  'symbols))
                 end 'noerror))

      (let ((ppss (syntax-ppss)))
        ;; If we're in a string or comment.
        (unless (or (elt ppss 3)
                    (elt ppss 4))
          ;; Only stop looking if we successfully position
          ;; the point.
          (setq found (jojo--position-for-alignment)))))
    found))

(defun jojo--search-whitespace-after-next-sexp (&optional bound _noerror)
  "Move point after all whitespace after the next sexp.

Set the match data group 1 to be this region of whitespace and
return point.

BOUND is bounds the whitespace search."
  (unwind-protect
       (ignore-errors
         (jojo-forward-logical-sexp 1)
         (search-forward-regexp "\\([,\s\t]*\\)" bound)
         (pcase (syntax-after (point))
           ;; End-of-line, try again on next line.
           (`(12) (jojo--search-whitespace-after-next-sexp bound))
           ;; Closing paren, stop here.
           (`(5 . ,_) nil)
           ;; Anything else is something to align.
           (_ (point))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

(defun jojo-align (beg end)
  "Vertically align the contents of the sexp around point.
If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.
When called from lisp code align everything between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (jojo-backward-logical-sexp)
                     (list (point) end)))))
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (jojo--find-sexp-to-align end)
      (let ((sexp-end (save-excursion
                        (backward-up-list)
                        (forward-sexp 1)
                        (point-marker)))
            (jojo-align-forms-automatically nil)
            (count 1))
        ;; For some bizarre reason, we need to `align-region' once for each
        ;; group.
        (save-excursion
          (while (search-forward-regexp "^ *\n" sexp-end 'noerror)
            (cl-incf count)))
        (dotimes (_ count)
          (align-region (point) sexp-end nil
                        '((jojo-align (regexp . jojo--search-whitespace-after-next-sexp)
                           (group . 1)
                           (separate . "^ *$")
                           (repeat . t)))
                        nil))
        ;; Reindent after aligning because of #360.
        (indent-region (point) sexp-end)))))

;;; Indentation
(defun jojo-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`jojo-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))
    (when jojo-align-forms-automatically
      (condition-case nil
          (jojo-align beg end)
        (scan-error nil)))))

(defun jojo-indent-line ()
  "Indent current line as jojo code."
  (lisp-indent-line))

(defvar jojo-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `jojo-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `jojo-indent-function'
for more information.")

(defun jojo--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `jojo-get-indent-function', then try the
`jojo-indent-function' and `jojo-backtracking-indent'
symbol properties."
  (or (when (functionp jojo-get-indent-function)
        (funcall jojo-get-indent-function function-name))
      (get (intern-soft function-name) 'jojo-indent-function)
      (get (intern-soft function-name) 'jojo-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'jojo-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'jojo-backtracking-indent)))
      (when (string-match (rx (or "let" "when" "while") (syntax symbol))
                          function-name)
        (jojo--get-indent-method (substring (match-string 0 function-name) 0 -1)))))

(defvar jojo--current-backtracking-depth 0)

(defun jojo--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `jojo--find-indent-spec'."
  (when (and (>= jojo-max-backtracking jojo--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((jojo--current-backtracking-depth (1+ jojo--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (jojo-backward-logical-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (jojo--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                               (goto-char start)
                               (jojo--find-indent-spec-backtracking)))))))
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

(defun jojo--find-indent-spec ()
  "Return the indent spec that applies to current sexp.
If `jojo-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if jojo-use-backtracking-indent
      (save-excursion
        (jojo--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (jojo--get-indent-method function))))

(defun jojo--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `jojo-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (jojo-backward-logical-sexp 1)
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
      (let ((case-a ; The meaning of case-a is explained in `jojo-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
          ;; For compatibility with the old `jojo-defun-style-default-indent', any
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

(defun jojo--not-function-form-p ()
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
;; delegate indentation to `jojo--normal-indent'.
(defun jojo-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a jojo function with a
non-nil property `jojo-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `jojo-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (jojo--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (jojo--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (jojo-forward-logical-sexp 1)
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
              (jojo--normal-indent last-sexp :always-align))
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
              (jojo--normal-indent last-sexp :always-align))
             ;; This is should be identical to the :defn above.
             ((and function
                   (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                 function)
                   (not (string-match "\\`default" (match-string 1 function))))
              (+ lisp-body-indent containing-form-column))
             ;; Finally, nothing special here, just respect the user's
             ;; preference.
             (t (jojo--normal-indent last-sexp jojo-indent-style)))))))))

;;; setting indentation
(defun put-jojo-indent (sym indent)
  "Instruct `jojo-indent-function' to indent the body of SYM by INDENT."
  (put sym 'jojo-indent-function indent))

(defmacro define-jojo-indent (&rest kvs)
  "Call `put-jojo-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x)
                 `(progn (put-jojo-indent
                          (quote ,(car x)) ,(cadr x))
                         (setq jojo-font-lock-keywords
                               (cons (list ,(concat
                                             "("
                                             (regexp-opt
                                              (list (symbol-name (car x))) t)
                                             "\\>")
                                           1 font-lock-keyword-face)
                                     jojo-font-lock-keywords))))
               kvs)))


(defun add-custom-jojo-indents (name value)
  "Allow `jojo-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-jojo-indent x 'defun))
          value))

(defcustom jojo-defun-indents nil
  "List of additional symbols with defun-style indentation in jojo.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-jojo-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-jojo-indents)

(define-jojo-indent
    ;; built-ins
    (ns 1)
    (fn :defn)
  (def :defn)
  (defn :defn)
  (bound-fn :defn)
  (if 1)
  (if-not 1)
  (case 1)
  (cond 0)
  (condp 2)
  (cond-> 1)
  (cond->> 1)
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
  ;; specify and specify! are from jojoScript
  (specify '(1 :defn))
  (specify! '(1 :defn))
  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn '(1 ((:defn)) nil))
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)

  (+class :defn) (class :defn)
  (+supertype :defn) (supertype :defn)
  (+subtype :defn) (subtype :defn)
  (+type :defn) (type :defn) (+simple-type :defn) (simple-type :defn)
  (+fun :defn) (fun :defn)
  (+jojo :defn) (jojo :defn)
  (+type-alias :defn) (type-alias :defn)
  (+imp :defn) (imp :defn)
  (+proof :defn) (proof :defn)
  (run :defn)
  (+def :defn)
  (+data :defn)
  (+var :defn)
  (note :defn)

  (forget :defn)

  (type-sum :defn) (type-case :defn))

;;; Sexp navigation
(defun jojo--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.

\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\^\\|#[?[:alpha:]]"))

(defun jojo-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (jojo-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (jojo--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun jojo-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (jojo-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (jojo--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

(provide 'jojo-mode)