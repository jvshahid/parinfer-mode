;;; -*- lexical-binding: t; -*-

(require 'parinfer-smart)
(require 'cl-lib)
(require 'dash)

(defun parinfer--str (&rest args)
  (apply #'concat
         (-interpose "\n" args)))

(defconst parinfer--test-modify-buffer-commands
  [newline-and-indent insert delete-char backward-delete-char
                      backward-kill-word kill-word
                      kill-line parinfer-shift-left parinfer-shift-right
                      delete-indentation])

(defun parinfer--test-execute-command (c)
  (when (seq-contains parinfer--test-modify-buffer-commands (car c))
    (parinfer--before-change-hook nil nil))
  (eval c)
  (parinfer--post-command-hook))

(defun parinfer--test (id orig pos commands result &optional err)
  (with-temp-buffer
    (erase-buffer)
    (clojure-mode)
    (insert orig)
    (goto-char (point-min))
    (forward-line (car pos))
    (forward-char (cdr pos))
    (let ((debug-on-error t))
      (mapc #'parinfer--test-execute-command
            commands))
    (if err
        (if (equal err parinfer--last-error)
            (message "%s success!" id)
          (message "%s failed\nerror=%s" id parinfer--last-error))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (if (and (string-equal text result)
                 (not parinfer--last-error))
            (message "%s success!" id)
          (if parinfer--last-error
              (message "%s failed!\nunexpected error=%s" id parinfer--last-error)
            (message "%s failed!\nresult=\n%s" id text)))))))

;; -----------------------------------------------------------------------------
;; CASES
;; -----------------------------------------------------------------------------

(parinfer--test
 "case01"
 "(foo)"
 (cons 0 4)
 [(newline-and-indent)]
 "(foo\n )")

;; -----------------------------------------------------------------------------

(parinfer--test
 "case02"
 (parinfer--str "(let [a 1])")
 (cons 0 9)
 [(newline-and-indent)
  (right-char)]
 (parinfer--str "(let [a 1])"
                ""))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case03"
 (parinfer--str "()")
 (cons 0 1)
 [(newline-and-indent)]
 (parinfer--str "("
                " )"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case04"
 (parinfer--str "(let [{:keys [x]} data])")
 (cons 0 15)
 [(newline-and-indent)
  (right-char)]
 (parinfer--str "(let [{:keys [x]}"
                "      data])"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case05"
 (parinfer--str "(let [{:keys [x]} data]"
                "  (+ x 2))")
 (cons 0 15)
 [(newline-and-indent)
  (insert "y")
  (right-char)]
 (parinfer--str "(let [{:keys [x"
                "              y]} data]"
                "  (+ x 2))"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case06"
 (parinfer--str "(foo {:a 1"
                "      :b 2} hello)")
 (cons 1 10)
 [(newline-and-indent)
  (forward-line)]
 (parinfer--str "(foo {:a 1"
                "      :b 2}"
                "     hello)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case07"
 (parinfer--str "(foo {:a 1"
                "      :b 2} hello)")
 (cons 1 10)
 [(newline-and-indent)
  (insert "x")]
 (parinfer--str "(foo {:a 1"
                "      :b 2"
                "      x} hello)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case08"
 (parinfer--str "(foo"
                " {:a 1"
                "  :b 2})")
 (cons 1 1)
 [(backward-delete-char 1)]
 (parinfer--str "(foo)"
                "{:a 1"
                " :b 2}"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case09"
 (parinfer--str "(foo)")
 (cons 0 4)
 [(newline-and-indent)
  (backward-delete-char 1)]
 (parinfer--str "(foo)"
                ""))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case10"
 (parinfer--str "(foo)"
                "")
 (cons 1 0)
 [(insert " ")]
 (parinfer--str "(foo)"
                " "))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case11"
 (parinfer--str "(foo)"
                "")
 (cons 1 0)
 [(insert " a")]
 (parinfer--str "(foo"
                " a)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case12"
 (parinfer--str "(foo"
                " a)")
 (cons 1 3)
 [(backward-delete-char 1)
  (backward-delete-char 1)]
 (parinfer--str "(foo"
                " )"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case13"
 (parinfer--str "(foo"
                " a)")
 (cons 1 1)
 [(insert ";")]
 (parinfer--str "(foo)"
                " ;a)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case14"
 (parinfer--str "(foo)"
                " ;a)")
 (cons 1 2)
 [(backward-delete-char 1)]
 (parinfer--str "(foo"
                " a)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case15"
 (parinfer--str "(foo"
                " a)")
 (cons 0 0)
 [(insert ";")]
 (parinfer--str ";(foo"
                " a"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case16"
 (parinfer--str ";(foo"
                " a")
 (cons 0 1)
 [(backward-delete-char 1)]
 (parinfer--str "(foo"
                " a)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case17"
 (parinfer--str "(foo"
                " a)")
 (cons 1 0)
 [(kill-line)]
 (parinfer--str "(foo)"
                ""))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case18"
 (parinfer--str "(foo"
                " a)")
 (cons 0 0)
 [(kill-line)]
 (parinfer--str ""
                " a"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case19"
 (parinfer--str "foo")
 (cons 0 0)
 [(insert "(")]
 (parinfer--str "(foo)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case20"
 (parinfer--str "(foo)")
 (cons 0 1)
 [(insert "[")]
 (parinfer--str "([foo])"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case21"
 (parinfer--str "(foo)")
 (cons 0 0)
 [(insert "[")]
 (parinfer--str "[(foo)]"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case22"
 (parinfer--str "(() a)")
 (cons 0 2)
 [(insert "[")]
 (parinfer--str "(([) a)")
 parinfer--error-unmatched-close-paren)

;; -----------------------------------------------------------------------------

(parinfer--test
 "case23"
 (parinfer--str "(() a)")
 (cons 0 1)
 [(insert "[")]
 (parinfer--str "([() a])"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case24"
 (parinfer--str "(() a)")
 (cons 0 5)
 [(insert "[")]
 (parinfer--str "(() a[])"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case25"
 (parinfer--str "(foo {:a 1"
                "      :b 2})")
 (cons 0 5)
 [(newline-and-indent)]
 (parinfer--str "(foo"
                " {:a 1"
                "  :b 2})"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case26"
 (parinfer--str "(foo)"
                "{:a 1"
                " :b 2}")
 (cons 1 0)
 [(backward-delete-char 1)
  (backward-delete-char 1)
  (insert " ")]
 (parinfer--str "(foo {:a 1"
                "      :b 2})"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case27"
 (parinfer--str "(foo"
                " {:a 1"
                "  :b 2})")
 (cons 1 1)
 [(delete-indentation)]
 (parinfer--str "(foo {:a 1"
                "      :b 2})"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case28"
 (parinfer--str "(foo"
                "  (bar)"
                "  (baz))")
 (cons 2 2)
 [(insert "                 ")
  (previous-line)]
 (parinfer--str "(foo"
                "  (bar"
                "   (baz)))"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case29"
 (parinfer--str "(foo"
                "  (bar"
                "   (baz)))")
 (cons 2 2)
 [(backward-delete-char 1)
  (previous-line)]
 (parinfer--str "(foo"
                "  (bar)"
                "  (baz))"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case30"
 (parinfer--str "{:a 1"
                " :b 2}")
 (cons 0 1)
 [(insert "(")]
 (parinfer--str "{(:a 1)"
                " :b 2}"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case31"
 (parinfer--str "{:a 1"
                " :b 2}")
 (cons 0 0)
 [(insert "(")]
 (parinfer--str "({:a 1"
                "  :b 2})"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case32"
 (parinfer--str "(foo {:a 1"
                "      :b 2})")
 (cons 0 5)
 [(insert "[")]
 (parinfer--str "(foo [{:a 1"
                "       :b 2}])"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case33"
 (parinfer--str "(aaaaaaa {:a 1"
                "          :b 2}"
                "         foo)")
 (cons 0 8)
 [(backward-delete-char 3)]
 (parinfer--str "(aaaa {:a 1"
                "       :b 2}"
                "         foo)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case34"
 (parinfer--str "(aaaaaaa {:a 1"
                "          :b 2}"
                "         foo)")
 (cons 0 8)
 [(backward-delete-char 3)
  (next-line)]
 (parinfer--str "(aaaa {:a 1"
                "       :b 2}"
                "      foo)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case35"
 (parinfer--str "(foo)"
                "  ")
 (cons 1 2)
 [(insert "bar")
  (right-char)]
 (parinfer--str "(foo"
                "  bar)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case36"
 (parinfer--str "(foo [])")
 (cons 0 6)
 [(insert ")")]
 (parinfer--str "(foo [])"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case37"
 (parinfer--str "(foo)  ;;c"
                "")
 (cons 2 0)
 [(insert " ")
  (insert "a")]
 (parinfer--str "(foo  ;;c"
                " a)"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case38"
 (parinfer--str "(a"
                " b)  ;;c")
 (cons 1 1)
 [(backward-delete-char 1)]
 (parinfer--str "(a)"
                "b  ;;c"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case39"
 (parinfer--str "(foo"
                " (bar))")
 (cons 1 1)
 [(insert ";")
  (backward-delete-char 1)]
 (parinfer--str "(foo"
                " (bar))"))

;; -----------------------------------------------------------------------------

(parinfer--test
 "case40"
 (parinfer--str "(foo"
                "")
 (cons 1 0)
 [(insert "    bar")
  (right-char)]
 (parinfer--str "(foo"
                " bar)"))

;; -----------------------------------------------------------------------------
;; Do barf by insert close paren.
;; Testing for re-indent.

(parinfer--test
 "case41"
 (parinfer--str "(foo"
                " bar"
                " baz)")
 (cons 0 4)
 [(insert ")")
  (next-line)]
 (parinfer--str "(foo)"
                "bar"
                "baz"))


;; -----------------------------------------------------------------------------
;; Don't align code if we are do comment line,
;; so that we can uncomment later.

(parinfer--test
 "case42"
 (parinfer--str "(foo"
                " bar)")
 (cons 0 0)
 [(insert ";")
  (next-line)]
 (parinfer--str ";(foo"
                " bar"))

;; -----------------------------------------------------------------------------
;; Test shift text

(parinfer--test
 "case43"
 (parinfer--str "(foo)"
                "bar"
                "baz"
                "abc")
 (cons 1 2)
 [(call-interactively #'set-mark-command)
  (next-line)
  (parinfer-shift-right 1)
  (call-interactively #'set-mark-command)]
 (parinfer--str "(foo"
                " bar"
                " baz)"
                "abc"))
