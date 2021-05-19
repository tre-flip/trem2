;;; trem2.el --- my own modal mode

;; Author: treflip <TODO@TODO.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem2.el
;; Package-Requires: ((trem2-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on  xah-fly-keys, ryo-modal, modalka, kakoune.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'multiple-cursors)
(require 'avy)
(require 'helm)
(require 'helm-gtags)
(require 'expand-region)
(require 'the-org-mode-expansions)

;;;;;;;;;;;;;;;
;; VARIABLES ;;
;;;;;;;;;;;;;;;
(defvar trem2--mode-line-message " CMD")

(defvar trem2-excluded-modes nil)

(defvar-local trem2-eval-buffer #'eval-buffer)
(defvar-local trem2-eval-region #'eval-region)
(defvar-local trem2-shell "bash")

;; expected to be set to #'er/expand-region from expand-region package:
(defvar-local trem2-mark #'mark-word)

;; default is the line/paragraph, TODO: DON'T FORGET TO SET LATER:
(defvar-local trem2-text-object-next #'ignore)
(defvar-local trem2-text-object-prev #'ignore)

;; expected to execute "clear" or analogs in terminal modes
(defvar-local trem2-whole-buffer #'mark-whole-buffer)

;;;;;;;;;;;;;;;;;;;;;
;; MODE DEFINITION ;;
;;;;;;;;;;;;;;;;;;;;;

(defgroup trem2 nil
  "Introduce native modal editing of your own design"
  :group  'editing
  :tag    "Trem2"
  :prefix "trem2-"
  )

;;;###autoload
(defvar trem2-mode-map (make-sparse-keymap)
  "This is Trem2 mode map, used to translate your keys.")

(defvar trem2-cursor-type 'box)

;;;###autoload
(define-minor-mode trem2-mode
  "Toggle the `trem2-mode' minor mode.
With a prefix argument ARG, enable `trem2-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'."
  nil " CMD" trem2-mode-map
  (if trem2-mode
      (setq-local cursor-type trem2-cursor-type)
    (setq-local cursor-type (default-value 'cursor-type))))

(defun trem2--maybe-activate ()
  "Activate `trem2-mode' if current buffer is not minibuffer or blacklisted.
This is used by `trem2-global-mode'."
  (unless (or (minibufferp)
              (member major-mode trem2-excluded-modes))
    (trem2-mode 1)))

;;;###autoload
(define-globalized-minor-mode trem2-global-mode
  trem2-mode
  trem2--maybe-activate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL KEYMAP DEFINITIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a map for a lead key
(define-prefix-command 'trem2-spc-map)

;; the following keymaps are meant to be transient
(defvar trem2-nav-map (make-sparse-keymap)
  "A keymap for navigation.")
(defvar trem2-repeat-map (make-sparse-keymap)
  "A keymap for repetition.")
(defvar trem2-scroll-map (make-sparse-keymap)
  "A keymap for scrolling.")
(defvar trem2-mark-map (make-sparse-keymap)
  "A keymap for fast marking")
(defvar trem2-format-map (make-sparse-keymap)
  "A keymap for text formatting")
(defvar trem2-eob-map (make-sparse-keymap)
  "A keymap for end-of-buffer command (transient)")
(defvar trem2-register-map (make-sparse-keymap)
  "A keymap for fast advanced editing primitives, like reformat-line, reformat-word etc.")

(defvar trem2-help-mode-map (make-sparse-keymap)
  "A keymap for displaying bindings without cyrillic characters.")
(defvar trem2-help-spc-map (make-sparse-keymap)
  "A keymap for displaying bindings without cyrillic characters.")

;;;;;;;;;;;;;;;
;; UTILITIES ;;
;;;;;;;;;;;;;;;
(defun trem2-beginning-of-buffer ()
  (interactive)
  (if (bobp)
      (pop-global-mark)
    (beginning-of-buffer)))

(defun trem2-end-of-buffer ()
  (interactive)
  (if (eobp)
      (pop-global-mark)
    (end-of-buffer)))

(defun trem2-append-at-eol-space ()
  "Go to end of line, format it to just one space at the end if it's not blank and leave CMD mode."
  (interactive)
  (end-of-line)
  (unless (looking-back "^[ \t]*$") 
    (just-one-space))
  (trem2-global-mode -1))

(defun trem2-append-at-eol ()
  (interactive)
  (end-of-line)
  (trem2-global-mode -1))

(defun trem2-open-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (trem2-global-mode -1))

(defun trem2-open-below ()
  "Go to end of line, then newline-and-indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (trem2-global-mode -1))

(defun trem2-shell-pipe-lines-or-regions ()
  "Run a shell command on each of the current regions separately and replace the current regions with its output."
  (interactive)
  (mc/for-each-cursor-ordered
     (shell-command-on-region (mc/cursor-beg cursor)
                              (mc/cursor-end cursor)
                              trem2-shell
                              nil
                              1)))

(defun trem2-toggle-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower."
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun trem2-mark-line ()
  "Select current line, or select next line if called again."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

(defun trem2-scroll-up ()
  (interactive)
  (scroll-up 2))
(defun trem2-scroll-down ()
  (interactive)
  (scroll-down 2))
 
(defun trem2-replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	         (yank))
    (progn (delete-region (point) (1+ (point)))
	       (yank))))

(defun trem2-delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem2-toggle-mark ()
  "Set mark if it's inactive, deactivate it if it's active."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
      (command-execute #'set-mark-command)))

(defun trem2-reformat-whitespaces-to-one-space (@begin @end)
  "Replace whitespaces by one space."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil "move")
        (replace-match " ")))))

(defun trem2-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection."
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun trem2-append-to-register-1 ()
  "Append current line or text selection to register 1.
When no selection, clear register.
See also: `trem2-paste-from-register-1', `copy-to-register'."
  (interactive)
  (if (use-region-p)
        (progn
	  (append-to-register ?1 (region-beginning) (region-end))
	  (message "Appended to register 1."))
      (set-register ?1 "")
      (message "Cleared register 1.")))

(defun trem2-paste-from-register-1 ()
  "Paste text from register 1.
See also: `trem2-copy-to-register-1', `insert-register'."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defvar trem2-brackets nil "string of left/right brackets pairs.")
(setq trem2-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠")

(defvar trem2-left-brackets '("\""  "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")


  "List of left bracket chars.")

(defvar trem2-right-brackets '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")

(defvar trem2-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq trem2-punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")

(defun trem2-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `trem2-punctuation-regex'"
  (interactive "p")
  (re-search-forward trem2-punctuation-regex nil t n))

(defun trem2-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `trem2-forward-punct'"
  (interactive "p")
  (re-search-backward trem2-punctuation-regex nil t n))

(defun trem2-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `trem2-left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt trem2-left-brackets) nil t))

(defun trem2-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `trem2-right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt trem2-right-brackets) nil t))

(defun trem2-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at (regexp-opt trem2-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt trem2-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun trem2-change-bracket-pairs ( @from-chars @to-chars)
  "Change bracket pairs from one type to another.
For example, change all parenthesis () to square brackets [].
Works on selected text, or current text block.
When called in lisp program, @from-chars or @to-chars is a string of bracket pair. eg \"(paren)\",  \"[bracket]\", etc.
The first and last characters are used. (the middle is for convenience in ido selection.)
If the string contains “,2”, then the first 2 chars and last 2 chars are used, for example  \"[[bracket,2]]\".
If @to-chars is equal to string “none”, the brackets are deleted."
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"ascii quote\""
            "[[double square,2]]"
            "“curly quote”"
            "‘single quote’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "『white corner』"
            "【lenticular】"
            "〖white lenticular〗"
            "〈angle〉"
            "《double angle》"
            "〔tortoise〕"
            "〘white tortoise〙"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly⦄"
            "〈pointing angle〉"
            "⦑ANGLE WITH DOT⦒"
            "⧼CURVED ANGLE⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE ORNAMENT❱"
            "none"
            )))
     (list
      (ido-completing-read "Replace this:" $bracketsList )
      (ido-completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))


(defun trem2-delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem2-reformat-to-multi-lines ( &optional @begin @end @min-length)
  "Replace spaces by a newline at places so lines are not long.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
If `universal-argument' is called first, use the number value for min length of line. By default, it's 70."
  (interactive)
  (let (
        $p1 $p2
        ($blanks-regex "\n[ \t]*\n")
        ($minlen (if @min-length
                     @min-length
                   (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column))))
    (if (and  @begin @end)
        (setq $p1 @begin $p2 @end)
      (if (use-region-p)
          (progn (setq $p1 (region-beginning) $p2 (region-end)))
        (save-excursion
          (if (re-search-backward $blanks-regex nil "move")
              (progn (re-search-forward $blanks-regex)
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward $blanks-regex nil "move")
              (progn (re-search-backward $blanks-regex)
                     (setq $p2 (point)))
            (setq $p2 (point))))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while
            (re-search-forward " +" nil "move")
          (when (> (- (point) (line-beginning-position)) $minlen)
            (replace-match "\n" )))))))

(defun trem2-reformat-lines ( &optional @length)
  "Reformat current text block or selection into short lines or 1 long line.
When called for the first time, change to one long line. Second call change it to multiple short lines. Repeated call toggles.
If `universal-argument' is called first, use the number value for min length of line. By default, it's 70."
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
         (@length (if @length
                      @length
                    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column )))
         (is-longline-p
          (if (eq last-command this-command)
              (get this-command 'is-longline-p)
            nil))
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "move")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "move")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (progn
      (if current-prefix-arg
          (trem2-reformat-to-multi-lines $p1 $p2 @length)
        (if is-longline-p
            (trem2-reformat-to-multi-lines $p1 $p2 @length)
          (trem2-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))

(defun trem2-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'."
  (interactive)
  (let ($begin $end)
    (if (use-region-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))

(defun trem2-kill-forward-bracket-text ()
  (interactive)
  (backward-kill-sexp -1))

(defun trem2-kill-backward-bracket-text ()
  (interactive)
  (backward-kill-sexp))

(defun trem2-kill-backward ()
  "Kill selected text or char backward or bracket pair."
  (interactive)
  (cond
   ((use-region-p)
    (kill-region (region-beginning) (region-end)))
   ((or (looking-back (regexp-opt trem2-left-brackets))
	(looking-back (regexp-opt trem2-right-brackets)))
    (condition-case nil
	(progn
	  (backward-char)
	  (trem2-delete-pair))
      ((error) (delete-char 1))))
   (t (delete-char -1))))
  
(defun trem2-kill-forward ()
  "Kill selected text or char backward or bracket pair."
  (interactive)
  (cond
   ((use-region-p)
    (kill-region (region-beginning) (region-end)))
   ((or (looking-at (regexp-opt trem2-left-brackets))
	(looking-at (regexp-opt trem2-right-brackets)))
    (condition-case nil
	(trem2-delete-pair)
      ((error) (delete-char 1))))
   (t (delete-char 1))))
  
(defun trem2-delete-pair ()
  (interactive)
  (let ((!spos (point)))
    (trem2-goto-matching-bracket)
    (if (< (point) !spos)
	(progn
	  (delete-pair)
	  (goto-char (- !spos 2)))
      (progn
	(goto-char !spos)
	(delete-pair)))))

(defun trem2-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.
• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun trem2-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.
• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
    (end-of-line)))

(defun trem2-insert-space ()
  (interactive)
  (insert " "))

(defun trem2-change ()
  "Kill forward and exit CMD mode"
  (interactive)
  (trem2-kill-forward)
  (trem2-global-mode -1))

(defun trem2-help-map ()
  "Display help for trem2's single keystroke keymap"
  (interactive)
  (which-key-show-full-keymap 'trem2-help-mode-map))

(defun trem2-help-map-2 ()
  "Display help for trem2's double keystroke keymap"
  (interactive)
  (which-key-show-full-keymap 'trem2-help-spc-map))

(defun trem2-split-line-and-quit ()
  (interactive)
  (split-line)
  (trem2-global-mode -1))

(defun trem2-toggle-highlight ()
  (interactive)
  (let ((!overlays (overlays-at (point))))
    (if !overlays
	(progn
	  (message "found overlay")
	  (mapcar  #'delete-overlay !overlays))
      (overlay-put (make-overlay (region-beginning)
				 (region-end))
		   'face 'highlight)))
  (deactivate-mark))

(defvar trem2-key-pairs (cl-mapcar 'cons
			"qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"	
			"йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"))

(defun trem2-bind-rus (m k d)
  (let ((c (cdr (assoc (aref k 0) trem2-key-pairs))))
    (when c
      (define-key m (kbd (char-to-string c)) d))))

;;;;;;;;;;;;;;
;; BINDINGS ;;
;;;;;;;;;;;;;;

(defun trem2-bind-mode-map (key command)
  "Binds a command in the trem-mode-map"
  ;; bind to actual keymaps
  (define-key trem2-mode-map (kbd key) command)
  (trem2-bind-rus trem2-mode-map key command)
  ;; bind to help-keymap
  (unless (string-equal key "SPC")
    (define-key trem2-help-mode-map (kbd key) command)))

(defun trem2-bind-spc-map (key command)
  "Binds a command in the trem-mode-spc"
  (define-key trem2-spc-map (kbd key) command)
  (trem2-bind-rus 'trem2-spc-map key command)
  ;; bind to help-keymap
  (define-key trem2-help-spc-map (kbd key) command))

(defmacro trem2-bind-transient (transient-keymap key command &optional !modeline-indicator)
  `(progn (trem2-bind-spc-map ,key (lambda ()
				     (interactive)
				     (set-transient-map ,transient-keymap t)
				    (command-execute ,command)))
	  (define-key ,transient-keymap ,key ,command)))

;; LEADKEY
(trem2-bind-mode-map "SPC" 'trem2-spc-map)

;; NAVIGATION
;; char/line arrow-like navigation
(trem2-bind-mode-map "i" #'previous-line)
(trem2-bind-mode-map "j" #'backward-char)
(trem2-bind-mode-map "k" #'next-line    )
(trem2-bind-mode-map "l" #'forward-char )

;; word navigation
(trem2-bind-mode-map "u" #'backward-word)
(trem2-bind-mode-map "o" #'forward-word)

;; text-object navigation
(setq-default trem2-text-object-next #'trem2-end-of-line-or-block)
(setq-default trem2-text-object-prev #'trem2-beginning-of-line-or-block)

(trem2-bind-mode-map ";" trem2-text-object-next)
(trem2-bind-mode-map "h" trem2-text-object-prev)

;; parens navigation and avy
(trem2-bind-mode-map "m" #'trem2-backward-left-bracket)
(trem2-bind-mode-map "." #'trem2-forward-right-bracket)
(trem2-bind-mode-map "," #'avy-goto-word-1)

;; scrolling 
(trem2-bind-transient trem2-scroll-map "i" #'trem2-scroll-down)
(trem2-bind-transient trem2-scroll-map "k" #'trem2-scroll-up)

;; start/end of buffer
;; TODO: g - beginning of buffer, end of buffer if at the beginning of buffer
;; TODO: SPC g = C-x C-SPC
(trem2-bind-mode-map "g" #'trem2-beginning-of-buffer)
(trem2-bind-transient trem2-eob-map "g"  #'trem2-end-of-buffer)

;; recenter
(trem2-bind-mode-map "a" #'recenter-top-bottom)

;; goto line
(trem2-bind-spc-map  ";" #'goto-line)
;; search
(trem2-bind-mode-map "n" #'helm-occur)

;; EDITING
;; kill
(trem2-bind-mode-map "f" #'trem2-kill-forward)
(trem2-bind-mode-map "s" #'trem2-kill-backward)
(trem2-bind-mode-map "r" #'trem2-kill-forward-bracket-text)
(trem2-bind-mode-map "w" #'trem2-kill-backward-bracket-text)

;; undo, copy, yank, replace selection
(trem2-bind-mode-map "t" #'undo)
(trem2-bind-mode-map "c" #'kill-ring-save)
(trem2-bind-mode-map "v" #'yank)
(trem2-bind-spc-map  "v" #'yank-pop)
(trem2-bind-mode-map "p" #'trem2-replace-selection)

;; comment/uncomment
(trem2-bind-mode-map "z" #'comment-region)
(trem2-bind-spc-map  "z" #'uncomment-region)

;; query-replace
(trem2-bind-spc-map  "r" #'query-replace)

;; multi cursors
(trem2-bind-spc-map  "m" #'mc/edit-beginnings-of-lines)

;; reformat
(trem2-bind-mode-map  "b" #'trem2-toggle-case)
(trem2-bind-transient trem2-format-map "7" #'trem2-reformat)
(trem2-bind-spc-map   "8" #'fill-paragraph)

;; registers
(trem2-bind-transient trem2-register-map "a" #'trem2-append-to-register-1)
(trem2-bind-transient trem2-register-map "n" #'trem2-paste-from-register-1)

;; MARKING
(trem2-bind-mode-map "e" #'er/expand-region)
(trem2-bind-mode-map "d" #'trem2-toggle-mark)
(trem2-bind-spc-map  "i" #'exchange-point-and-mark)

;; mark line, paragraph, and whole-buffer
(trem2-bind-mode-map "7" #'trem2-mark-line)
(trem2-bind-mode-map "8" #'mark-paragraph)
(trem2-bind-spc-map  "d" trem2-whole-buffer)
(trem2-bind-spc-map  "h" #'trem2-toggle-highlight)

;; BUFFER AND WINDOW MANAGEMENT
(trem2-bind-mode-map "1" #'make-frame)
(trem2-bind-mode-map "2" #'delete-window)
(trem2-bind-spc-map "2" #'delete-other-windows)
(trem2-bind-mode-map "3" #'other-window)
(trem2-bind-mode-map "4" #'split-window-right)
(trem2-bind-mode-map "5" #'split-window-below)


;; killing buffers
(trem2-bind-mode-map "q" #'kill-buffer)
(trem2-bind-mode-map "<f5>" #'kill-buffer-and-window)
(trem2-bind-spc-map  "q" #'kill-buffer-and-window)

;; opening and jumping
(trem2-bind-spc-map "j" #'switch-to-buffer)
(trem2-bind-spc-map "s" #'save-buffer)
(trem2-bind-spc-map "f" #'helm-find-files)

;; COMMANDS AND EVALUATION
(trem2-bind-mode-map "x" #'helm-M-x)
(trem2-bind-transient trem2-repeat-map "x" #'repeat)
(trem2-bind-spc-map "c" trem2-eval-buffer)
(trem2-bind-spc-map "e" trem2-eval-region)
(trem2-bind-spc-map "p" #'trem2-shell-pipe-lines-or-regions)

;; SHELLS AND TERMINALS
(trem2-bind-spc-map "t" #'eshell)
(trem2-bind-spc-map "y" #'(lambda () (interactive) (vterm)))

;; ENTER INSERT MODE
(trem2-bind-mode-map "y" #'trem2-append-at-eol-space)
(trem2-bind-mode-map "6" #'trem2-append-at-eol)
(trem2-bind-mode-map "/" #'trem2-change)
(trem2-bind-mode-map "9" #'trem2-open-below)
(trem2-bind-mode-map "0" #'trem2-open-above)
(trem2-bind-spc-map  "9" #'trem2-split-line-and-quit)

;; HELP
(trem2-bind-mode-map "`" #'trem2-help-map)
(trem2-bind-spc-map "`" #'trem2-help-map-2)

;;;###autoload
(defun trem2-setup-keybinds (&key jiran)
  "Set up default trem2 keybindings for normal mode."
  (global-subword-mode 1)
  (when jiran
    (progn
      (trem2-bind-mode-map "k" #'previous-line)
      (trem2-bind-mode-map "," #'next-line    )
      (trem2-bind-mode-map "i" #'avy-goto-word-1)
      ;; scrolling:
      (trem2-bind-transient trem2-scroll-map "k" #'trem2-scroll-down)
      (trem2-bind-transient trem2-scroll-map "," #'trem2-scroll-up))))

(provide 'trem2)
;;; trem2.el ends here
