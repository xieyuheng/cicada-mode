;;; cicada-mode.el --- Major mode for cicada code

;; copy from clojure-mode.el

(require 'cl-lib)
(require 'align)
(require 'xyhlib)

(defgroup cicada nil
  "Major mode for editing cicada code."
  :prefix "cicada-"
  :group 'languages)

(defconst cicada-mode-version "5.7.0-snapshot"
  "The current version of `cicada-mode'.")

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
 (?\t "    ")
 (?\n ">   ")
 (?\f "    ")
 (?\r "    ")
 (?\s "    ")
 (?\. "_p")
 (?\, "_p")
 (?\- "_p")
 (?\_ "_p")
 (?\" "\"   ")
 (?'  "'   ")
 (?`  "'   ")
 (?\/ "<   ")
 (?\( "(")
 (?\) ")")
 (?\[ "(")
 (?\] ")")
 (?\{ "(")
 (?\} ")"))

(defun cicada-mode-variables ()
  "Set up initial buffer-local variables for cicada mode."
  (set-syntax-table cicada-mode-syntax-table)
  (setq-local paragraph-ignore-fill-prefix t)

  (setq-local comment-start "/")
  (setq-local comment-start-skip "/+[ \t]*")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local parse-sexp-ignore-comments t))

;;;###autoload
(define-derived-mode cicada-mode
    prog-mode "cicada"
    "Major mode for editing cicada code."
    (cicada-mode-variables)
    (cicada-font-lock-setup))

(defvar cicada-font-lock-keywords
  `(
    ;; 0123456789
    (,(rx (seq word-start
               (group (one-or-more (in (?0 . ?9))))
               word-end))
      (1 font-lock-constant-face))

    (,(rx (group ","))
      (1 font-lock-keyword-face))

    (,(rx word-start
          (group (or "#" "$")
                 (zero-or-more (not blank))
                 "!")
          word-end)
      (1 font-lock-variable-name-face))

    (,(rx word-start
          (group (or "#" "$")
                 (zero-or-more (not blank)))
          word-end)
      (1 font-lock-constant-face))

    (,(rx word-start
          (group "@")
          word-end)
      (1 font-lock-constant-face))

    (,(rx word-start
          (group "&")
          word-end)
      (1 font-lock-constant-face))

    (,(rx word-start
          (group "λ")
          word-end)
      (1 font-lock-constant-face))

    (,(rx word-start
          (group "%")
          word-end)
      (1 font-lock-constant-face))

    (,(rx word-start
          (group "%%")
          word-end)
      (1 font-lock-constant-face))

    ;; type-t
    (,(rx word-start
          (group (one-or-more (not blank))
                 (seq "-" (or "t"
                              "tt"
                              "ttt"
                              "tttt"
                              "ttttt"
                              "tttttt")))
          word-end)
      (1 font-lock-type-face))

    ;; cons-c
    (,(rx word-start
          (group (one-or-more (not blank))
                 (seq "-" (or "c"
                              "cc"
                              "ccc"
                              "cccc"
                              "ccccc"
                              "cccccc")))
          word-end)
      ;; (1 font-lock-variable-name-face)
      (1 font-lock-constant-face))


    ;; set-s
    (,(rx word-start
          (group (one-or-more (not blank)) "-s")
          word-end)
      (1 font-lock-variable-name-face))

    ;; .:
    (,(rx word-start
          (group ".:" (one-or-more (not blank)))
          word-end)
      (1 font-lock-keyword-face))

    ;; <type>
    (,(rx word-start
          (group "<" (one-or-more (not blank)) ">")
          word-end)
      (1 font-lock-type-face))

    ;; infix
    (,(rx word-start
          (group (or ":"
                     "<"
                     ">"
                     "<:"
                     ":>"
                     "="
                     ":="
                     "=:"))
          word-end)
      (1 font-lock-variable-name-face))

    ;; infix2
    (,(rx word-start
          (group (or "::"))
          word-end)
      (1 font-lock-constant-face))

    ;; keyword
    (,(rx word-start
          (group (or
                  "note"
                  "import"
                  "as"

                  "error"
                  "match"
                  "instance"
                  "class"
                  "type"
                  "set"
                  "union"
                  "ref"
                  "join"
                  "disj"
                  "conj"
                  "lit"
                  "literal"
                  "tuple"
                  "enum"
                  "heri"
                  "inherit"
                  "data"
                  "lambda"

                  "unique"
                  "of"
                  "under"
                  "such-that"
                  "give"

                  "macro"

                  "*"

                  "if"
                  "then"
                  "else"

                  "λ"
                  "case"
                  "do"
                  "let"
                  "open-up"
                  "in"
                  "with-details"
                  ))
          word-end)
      (1 font-lock-keyword-face))

    ;; type like
    (,(rx word-start
          (group (or "--"
                     "=="
                     "=>"
                     "->"
                     "~>"
                     ">-"
                     "-<"
                     "<="
                     "<-"
                     "|"
                     "+"
                     "default"
                     "refl"
                     "><"
                     "><><"
                     "><><><"
                     "times"
                     "bye"
                     "recur"
                     "loop"
                     "true"
                     "false"
                     "lazy"
                     "yield"

                     "receive"
                     "send"
                     "spawn"

                     "emit"
                     "debug"
                     "step"
                     "quote"))
          word-end)
      (1 font-lock-type-face))

    ;; @fun
    (,(rx word-start
          (group "@" (one-or-more (not blank)))
          word-end)
      (1 font-lock-preprocessor-face))

    ;; fun@
    (,(rx word-start
          (group (one-or-more (not blank)) "@")
          word-end)
      (1 font-lock-preprocessor-face))

    ;; fun&
    (,(rx word-start
          (group (one-or-more (not blank)) "&")
          word-end)
      (1 font-lock-type-face))

    ;; name!
    (,(rx word-start
          (group (one-or-more (not blank)) "!")
          word-end)
      (1 font-lock-variable-name-face))

    ;; module-name:name
    (,(rx word-start
          (group (one-or-more (not (in ": \t")))
                 ":")
          (group (one-or-more (not blank)))
          word-end)
      (1 font-lock-type-face))

    ;; :local-name
    (,(rx word-start
          (group ":" (one-or-more (not (in ". \t"))))
          word-end)
      (1 font-lock-preprocessor-face))

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
          (("+-*.<>=!?$#%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun))))

(provide 'cicada-mode)
