(require 'paredit)

(defvar-local parinfer--ignore-commands
  '(undo))

(defvar-local parinfer--x nil)

(defvar-local parinfer--line nil)

(defvar-local parinfer--lock-begin nil
  "The beginning of the locked sexp, this sexp will neither slurp nor barf.")

(defvar-local parinfer--lock-end nil
  "The end of the locked sexp.")

(defvar-local parinfer--in-comment nil)

(defvar-local parinfer--change-pos nil)

(defvar-local parinfer--reindent nil)

(defvar-local parinfer--buffer-will-change nil)

(defvar-local parinfer--prev-x nil)

(defvar-local parinfer--process-range ())

(defvar-local parinfer--paren-stack ()
  "Element should be (paren point)")

(defvar-local parinfer--op-stack ()
  "Element should be (point :insert/:delete insert-text/delete-N)")

;;; Some basic rules
;;; When changing, the rest form of current sexp will be locked.
;;; the locked sexp will neither slurp nor barf.

(defun parinfer--get-line ()
  (1- (string-to-number (format-mode-line "%l"))))

(defun parinfer--get-x ()
  (- (point) (line-beginning-position)))

(defun parinfer--opener-to-closer (opener)
  (cond
   ((= opener 40) 41)
   ((= opener 91) 93)
   ((= opener 123) 125)))

(defun parinfer--closer-to-opener (closer)
  (cond
   ((= closer 41) 40)
   ((= closer 93) 91)
   ((= closer 125) 123)))

(defun parinfer--repeat-string (s n)
  (let ((r "")
        (i 0))
    (while (< i n)
      (setq r (concat r s))
      (setq i (1+ i)))
    r))

(defun parinfer--get-process-range ()
  "Return (begin, end)."
  (save-mark-and-excursion
    (let ((orig (point)))
      (beginning-of-defun)
      (let ((begin (point))
            (end nil))
        (if (ignore-errors (forward-sexp) t)
            (if (>= orig (point))
                (progn
                  (goto-char orig)
                  (goto-char (line-end-position))
                  (when (> (point-max) (point))
                    (forward-char))
                  (setq end (point)))
              (setq end (point)))
          (setq end (point-max)))
        (list begin end))))
  (list (point-min) (point-max))
  )

(defun parinfer--parse-buffer ()
  (goto-char (point-min))
  (skip-syntax-forward "[\";]'"))

;; -----------------------------------------------------------------------------
;;
;;   PREDICATES
;;
;; -----------------------------------------------------------------------------

(defun parinfer--opener-p ()
  (and (not (parinfer--char-p))
       (let ((ch (char-after (point))))
         (and ch
              (or (= ch 40)
                  (= ch 91)
                  (= ch 123))))))

(defun parinfer--closer-p ()
  (and (not (parinfer--char-p))
       (let ((ch (char-after (point))))
         (and ch
              (or (= ch 41)
                  (= ch 93)
                  (= ch 125))))))

(defun parinfer--unstable-closer-p ()
  (string-match-p
   "^[])}]+ *\\(?:;.*\\)?$"
   (buffer-substring-no-properties (point)
                                   (line-end-position))))

(defun parinfer--at-buffer-last-empty-line-p ()
  (and (= (point) (point-max))
       (= (line-beginning-position)
          (line-end-position))))

(defun parinfer--char-p ()
  (paredit-in-char-p))

(defun parinfer--string-p ()
  ;; (message "ppss")
  (nth 3 (syntax-ppss)))

(defun parinfer--comment-p ()
  (if (= parinfer--line (parinfer--get-line))
      (save-mark-and-excursion
        (unless (= (point) (line-end-position))
          (forward-char))
        ;; (message "ppss")
        (nth 4 (syntax-ppss)))
    (let ((f (get-text-property (point) 'face)))
      (or (eq f 'font-lock-comment-face)
          (eq f 'font-lock-comment-delimiter-face)))))

(defun parinfer--line-end-in-string-p ()
  (save-mark-and-excursion
    (goto-char (line-end-position))
    (parinfer--string-p)))

(defun parinfer--line-begin-with-comment-p ()
  (string-match-p
   "^ *;.*$"
   (buffer-substring-no-properties (line-beginning-position)
                                   (line-end-position))))

(defun parinfer--line-begin-with-closer-p ()
  (save-mark-and-excursion
    (back-to-indentation)
    (parinfer--unstable-closer-p)))

(defun parinfer--line-begin-with-string-literals-p ()
  (save-mark-and-excursion
    (goto-char (line-beginning-position))
    (parinfer--string-p)))



(defun parinfer--line-end-p ()
  "Return if we are at the end of line."
  (= (point) (line-end-position)))

(defun parinfer--trail-paren-p ()
  "Return if we are before a trail paren."
  (string-match-p
   "^[])}]+ *\\(?:;.*\\)?$"
   (buffer-substring-no-properties (point)
                                   (line-end-position))))

(defun parinfer--empty-line-p ()
  "Return if current line is a blank line."
  (or (eq (line-beginning-position) (line-end-position))
      (string-match-p
       "^[[:blank:]]+$"
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position)))))

(defun parinfer--end-of-code-p ()
  "The end of code, ignore the following comment.
This will ensure the current X is greater than parinfer--x if
current line is parinfer--line."
  (let ((ch (char-before)))
    (or (= (point) (point-max))
        (and ch
             (not (= (line-beginning-position) (point)))
             (not (parinfer--string-p))
             (string-match-p "^[])} ]*\\(?:;.*\\)?$"
                             (buffer-substring-no-properties
                              (point) (line-end-position)))
             (not (parinfer--line-end-in-string-p))
             (or (not (= (parinfer--get-line) parinfer--line))
                 (and (= (parinfer--get-line) parinfer--line)
                      (>= (parinfer--get-x) parinfer--x)))
             ;; Only first end-of-code is available,
             ;; so there's no need to check if we are in comment.
             ;; (save-mark-and-excursion
             ;;   (backward-char)
             ;;   (not (parinfer--comment-p)))
             ))))

;; -----------------------------------------------------------------------------
;;
;;   NAVIGATIONS
;;
;; -----------------------------------------------------------------------------

(defun parinfer--goto-next-indentation-1 ()
  (forward-line)
  (back-to-indentation))

(defun parinfer--goto-next-indentation (&optional end)
  "Goto the next line indentation before `END or the first after `END,
will skip empty lines, comment-only and begin-with-string-literal lines.
  If there's no more indentation to go, will ensure there's a newline
in the end of file.
  Return t if position changed, otherwise nil.
"
  (let ((end (or end (point-max))))
    (when (and (< (point)
                  end)
               (not (parinfer--at-buffer-last-empty-line-p)))
      (parinfer--goto-next-indentation-1)
      (while (and (> end (point))
                  (or (parinfer--empty-line-p)
                      (parinfer--line-begin-with-string-literals-p)
                      (parinfer--line-begin-with-comment-p)
                      (parinfer--line-begin-with-closer-p)))
        (parinfer--goto-next-indentation-1))
      ;; (message "indentation line: %s, x: %s"
      ;;          (parinfer--get-line)
      ;;          (parinfer--get-x))
      t)))

(defun parinfer--goto-next-opener-or-closer-or-end-of-code (&optional end)
  "Goto the next opener or closer.
Will return `opener or `closer if found, otherwise nil."
  (let ((end (or end (point-max))))
    (if (= end (point))
        nil
      (let ((found nil)
            (prev-ending-pos nil))
        (while (and (not found)
                    (> end (point)))
          (cond
           ((and (or (not prev-ending-pos)
                     (> (point) prev-ending-pos))
                 (parinfer--end-of-code-p))
            (progn (setq found :end-of-code)
                   (save-mark-and-excursion
                     (goto-char (line-end-position))
                     (setq prev-ending-pos (point)))))

           ((parinfer--string-p)
            (unless (parinfer--escape-string)
              (error "Can't escape string!")))

           ((parinfer--comment-p)
            (forward-line))

           ((parinfer--closer-p)
            (setq found :closer))

           ((parinfer--opener-p)
            (setq found :opener))

           (t
            (progn
              (if (= parinfer--line (parinfer--get-line))
                  (forward-char)
                (when (zerop (skip-syntax-forward "^(^)^w" end))
                  (forward-symbol 1)))))))
        ;; (message "%s is %s" (point) found)
        found))))

(defun parinfer--goto-tail-sexp-opener ()
  (let ((paren-stack ())
        (pos nil)
        (end (line-end-position))
        (end-in-comment (save-mark-and-excursion
                          (goto-char (line-end-position))
                          (parinfer--comment-p))))
    (while (> end (point))
      (cond
       ((and end-in-comment (parinfer--comment-p))
        (goto-char end))

       ((parinfer--opener-p)
        (progn (unless paren-stack (setq pos (point)))
               (push (char-after) paren-stack)))

       ((parinfer--closer-p)
        (pop paren-stack)))
      (forward-char))
    (if pos
        (progn (goto-char pos) t)
      nil)))

(defun parinfer--goto-line (line)
  (goto-char (point-min))
  (forward-line line))

(defun parinfer--goto-x (x)
  (if (> (- (line-end-position) (line-beginning-position))
         x)
      (forward-char x)
    (goto-char (line-end-position))))

(defun parinfer--escape-string ()
  (ignore-errors
    (backward-up-list 1 t)
    (forward-sexp)
    t))

;; -----------------------------------------------------------------------------
;;
;;   PROCEDURES
;;
;; -----------------------------------------------------------------------------

(defun parinfer--fix-end-empty-line ()
  (if (= (point-min) (point-max))
      (newline)
    (progn
      (goto-char (point-max))
      (unless (= (line-beginning-position) (line-end-position))
        (goto-char (line-end-position))
        (newline)))))

(defun parinfer--save-position ()
  (setq parinfer--x (parinfer--get-x))
  (setq parinfer--line (parinfer--get-line)))

(defun parinfer--save-lock-range ()
  (let ((pos (point)))
    (ignore-errors
      (backward-up-list))
    (when (not (= pos (point)))
      (setq parinfer--lock-begin ))))

(defun parinfer--restore-position ()
  (parinfer--goto-line parinfer--line)
  (if (< (- (line-end-position) (line-beginning-position))
         parinfer--x)
      (goto-char (line-end-position))
    ;; (messag "parinfer--restore-position begin")
    (forward-char parinfer--x)
    ;; (messag "parinfer--restore-position end")
    ))

(defun parinfer--correct-closer ()
  (let* ((opener (pop parinfer--paren-stack))
         (ch (car opener))
         (correct-ch (parinfer--opener-to-closer ch)))
    (unless (= correct-ch (char-after))
      (push (list (point) :insert correct-ch) parinfer--op-stack)
      (push (list (point) :delete 1) parinfer--op-stack))))

(defun parinfer--insert-unstable-parens (indent)
  ;; If end of code is followed by a closer.
  ;; This closer must be an unstable closer.
  ;; and it will not be searched again,
  ;; so we remove it here.
  ;; (message "insert unstable parens, point: %s, indent: %s" (point) indent)
  (if (< parinfer--lock-begin (point) parinfer--lock-end)
      (when (parinfer--closer-p)
        (parinfer--correct-closer))
    (progn
        (let ((break nil))
          (while (not break)
            (let ((last-opener (pop parinfer--paren-stack)))
              (if (not last-opener)
                  (setq break t)
                (let* ((ch (car last-opener))
                       (i (cadr last-opener))
                       (closer (parinfer--opener-to-closer ch)))
                  (if (>= i indent)
                      (push (list (point) :insert closer)
                            parinfer--op-stack)
                    (progn
                      (push last-opener parinfer--paren-stack)
                      (setq break t))))))))
        (when (parinfer--closer-p)
          (push (list (point) :delete 1) parinfer--op-stack)))))

(defun parinfer--reindent-last-changed-maybe ()
  "Goto the change-pos, reindent the sexp there,
and reindent all the  lines following."
  (let ((line (car parinfer--change-pos))
        (x (cadr parinfer--change-pos)))
    (when (not (and (= (parinfer--get-line) line)
                    (<= (parinfer--get-x) x)))
      (parinfer--goto-line line)
      (if (> (- (line-end-position) (line-beginning-position))
             x)
          (forward-char x)
        (goto-char (line-end-position)))
      (ignore-errors
        (unless (= (point) (line-beginning-position))
          (backward-up-list))
        (indent-sexp)
        (forward-sexp)
        (forward-line)
        (let ((break nil))
          (while (not break)
            (let ((old-len (- (line-end-position) (line-end-position))))
              (lisp-indent-line)
              (if (= (- (line-end-position) (line-beginning-position))
                     old-len)
                  (setq break t)
                (forward-line))))))
      (setq parinfer--change-pos nil)
      (setq parinfer--buffer-will-change nil))))

(defun parinfer--execute-op-1 (op)
  (let ((pos (car op))
        (op-type (cadr op))
        (arg (caddr op)))
    (if (eq op-type :insert)
        (progn
          ;; May need shift some point variables
          (goto-char pos)
          (insert arg))
      (progn
        ;; May need shift some point variables
        (goto-char pos)
        (delete-char arg)))))

(defun parinfer--execute-op ()
  ;; (message "%s" parinfer--op-stack)
  (mapcar #'parinfer--execute-op-1
          parinfer--op-stack)
  (setq parinfer--op-stack ()))

(defun parinfer--fix-opener ()
  (let ((opener (char-after (point)))
        (x (parinfer--get-x)))
    (push (list opener x) parinfer--paren-stack)))

(defun parinfer--fix-closer ()
  "If in the lock range, just pop or update paren type."
  (if (< parinfer--lock-begin (point) parinfer--lock-end)
      (parinfer--correct-closer)
    (let ((in-edit-scope (and (= (parinfer--get-line) parinfer--line)
                              ;; If we want open [] in {},
                              ;; the current cursor position must not be in edit-scope
                              (<= (parinfer--get-x) parinfer--x))))
      (if (and (parinfer--unstable-closer-p)
               (not in-edit-scope))
          (push (list (point) :delete 1) parinfer--op-stack)
        (let* ((closer (char-after (point)))
               (last-opener-info (pop parinfer--paren-stack))
               (last-opener (car last-opener-info)))
          (cond
           ((not last-opener)
            (push (list (point) :delete 1) parinfer--op-stack))
           ((not (= last-opener (parinfer--closer-to-opener closer)))
            (if in-edit-scope
                (progn
                  (push (list (point) :delete 1) parinfer--op-stack)
                  (push last-opener-info parinfer--paren-stack))
              (let ((correct-closer (parinfer--opener-to-closer last-opener)))
                (push (list (point) :insert correct-closer) parinfer--op-stack))))))))))

(defun parinfer--fix-paren (begin end indent)
  (let ((break nil))
    (while (not break)
      (if-let* ((type (parinfer--goto-next-opener-or-closer-or-end-of-code end)))
          (progn
            (cond
             ((equal type :opener) (parinfer--fix-opener))
             ((equal type :closer) (parinfer--fix-closer))
             ((equal type :end-of-code) (parinfer--insert-unstable-parens indent)))
            (when type
              (forward-char)))
        (setq break t)))))

(defun parinfer--align-tail-sexp ()
  (setq parinfer--lock-begin -1
        parinfer--lock-end -1)
  (let ((x (parinfer--get-x)))
    (when (and parinfer--prev-x
               (parinfer--goto-tail-sexp-opener)
               (not parinfer--in-comment)
               (not (parinfer--empty-line-p))
               (not (parinfer--comment-p)))
      (let* ((begin (point))
             (delta (- x parinfer--prev-x))
             (orig-x (- x delta)))
        (goto-char begin)
        (when (ignore-errors (forward-sexp) t)
          (let ((end (point)))
            (goto-char (line-beginning-position))
            (while (< begin (point))
              (if (> delta 0)
                  (insert (parinfer--repeat-string " " delta))
                (delete-char (abs delta)))
              (forward-line -1))
            (goto-char begin)
            (ignore-errors (forward-sexp))
            (setq parinfer--lock-begin begin
                  parinfer--lock-end (point)))))))
  (setq parinfer--prev-x nil))

(defun parinfer--pre-process ()
  (save-mark-and-excursion
    (parinfer--fix-end-empty-line)))

(defun parinfer--process (&optional begin end)
  (parinfer--pre-process)
  (condition-case ex
      (when (and (not (region-active-p))
                 (not (seq-contains parinfer--ignore-commands this-command)))
        (if parinfer--buffer-will-change
            (let* ((begin-end parinfer--process-range)
                   (begin (car begin-end))
                   (end (cadr begin-end)))
              (parinfer--process-changing begin end)
              (setq parinfer--buffer-will-change nil))
          (parinfer--process-moving)))
    (error
     (message (cadr ex))
     (parinfer--restore-position))))

(defun parinfer--process-moving ()
  (when parinfer--change-pos
    ;; (message "process moving")
    (parinfer--save-position)
    (parinfer--reindent-last-changed-maybe)
    (parinfer--restore-position)))

(defun parinfer--process-changing (&optional begin end)
  ;; Save end-line and end-x
  ;; (message "process changing")
  ;; (message "px: %s x: %s" parinfer--prev-x (parinfer--get-x))
  (parinfer--save-position)
  (goto-char end)
  (let ((end-line (parinfer--get-line))
        (end-x (parinfer--get-x)))
    (parinfer--restore-position)
    (parinfer--align-tail-sexp)
    (parinfer--goto-line end-line)
    (parinfer--goto-x end-x)
    (setq end (point)))
  (setq parinfer--op-stack ()
        parinfer--paren-stack ())
  (let* ((begin (or begin (point-min)))
         (end (or end (point-max)))
         (last-indent-pos begin)
         (curr-indent-pos begin))
    (goto-char begin)
    (while (> end (point))
        (setq last-indent-pos curr-indent-pos)
        (when (parinfer--goto-next-indentation)
          (setq curr-indent-pos (point))
          (let ((indent (parinfer--get-x)))
            (goto-char last-indent-pos)
            ;; (message "%s %s %s" last-indent-pos curr-indent-pos indent)
            ;; (message "%s" parinfer--paren-stack)
            (parinfer--fix-paren last-indent-pos curr-indent-pos indent))))
    (unless (and (= (line-beginning-position) (line-end-position))
                 (= (point-max) (point)))
      (parinfer--fix-paren curr-indent-pos (point-max) 0))
    (parinfer--execute-op))
  (parinfer--restore-position)
  (setq parinfer--change-pos (list (parinfer--get-line)
                                   (parinfer--get-x))))

(defun parinfer--before-change (start end)
  ;; (message "before start: %s end:%s" start end)
  (parinfer--pre-process)
  (when (not parinfer--buffer-will-change)
    (setq parinfer--process-range
          (parinfer--get-process-range))
    (setq parinfer--buffer-will-change t
          parinfer--in-comment (parinfer--comment-p)
          parinfer--prev-x (parinfer--get-x))))

;; -----------------------------------------------------------------------------
;;
;; MODE
;;
;; -----------------------------------------------------------------------------

(defun parinfer-mode-enable ()
  (if (bound-and-true-p electric-indent-mode)
    (electric-indent-mode -1))
  (add-hook 'before-change-functions #'parinfer--before-change t t)
  (add-hook 'post-command-hook #'parinfer--process t t))

(defun parinfer-mode-disable ()
  (remove-hook 'before-change-functions #'parinfer--before-change t)
  (remove-hook 'post-command-hook #'parinfer--process t))

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap newline] 'newline-and-indent)
    ;; (define-key map "\\" 'paredit-backslash)
    ;; (define-key map "\"" 'paredit-doublequote)
    map))

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode."
  nil "Parinfer" parinfer-mode-map
  (if parinfer-mode
      (parinfer-mode-enable)
    (parinfer-mode-disable)))

;;; parinfer.el ends here
