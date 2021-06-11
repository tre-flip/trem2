;;; trem2.el --- my own modal mode

;; Author: treflip <TODO@TODO.net>
;; Version: 0.1
;; URL: https://github.com/tre-flip/trem3.el
;; Package-Requires: ((emacs "25.1"))
;; MIT License

;;; Commentary:
;; Read the source. Based on  xah-fly-keys, ryo-modal, modalka, kakoune.el.

;;; Code:
(require 'atp)
(require 'cl-lib)
;; (require 'seq)
;; (require 'multiple-cursors)
(require 'avy)
(require 'helm)
(require 'helm-gtags)

;;;;;;;;;;;;;;;
;; VARIABLES ;;
;;;;;;;;;;;;;;;

(defface trem3-visual-hlt
  '((t :background "yellow" :extend t))
  "Used for highlighting text.")

(defvar trem3--mode-line-message " CMD")

(defvar trem3-excluded-modes nil)

(setq-default trem3-eval-buffer #'eval-buffer)
(setq-default trem3-eval-region #'eval-region)
(setq-default trem3-shell "bash")

;; expected to execute "clear" or analogs in terminal modes
(defvar-local trem3-whole-buffer #'mark-whole-buffer)

;;;;;;;;;;;;;;;;;;;;;
;; MODE DEFINITION ;;
;;;;;;;;;;;;;;;;;;;;;

(defgroup trem3 nil
  "Introduce ergonomic modal editing"
  :group  'editing
  :tag    "Trem3"
  :prefix "trem3-"
  )

;;;###autoload
(defvar trem3-mode-map (make-sparse-keymap)
  "This is Trem3 mode map, used to translate your keys.")

(defvar trem3-cursor-type 'box)

;;;###autoload
(define-minor-mode trem3-mode
  "Toggle the `trem3-mode' minor mode.
With a prefix argument ARG, enable `trem3-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'."
  nil " CMD" trem3-mode-map
  (if trem3-mode
      (progn (setq-local cursor-type trem3-cursor-type)
	     (atp-mode 1))
    (setq-local cursor-type (default-value 'cursor-type))
    (atp-mode -1)))

(defun trem3--maybe-activate ()
  "Activate `trem3-mode' if current buffer is not minibuffer or blacklisted.
This is used by `trem3-global-mode'."
  (unless (or (minibufferp)
              (member major-mode trem3-excluded-modes))
    (trem3-mode 1)))

;;;###autoload
(define-globalized-minor-mode trem3-global-mode
  trem3-mode
  trem3--maybe-activate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL KEYMAP DEFINITIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a map for a lead key
(define-prefix-command 'trem3-spc-map)

;; the following keymaps are meant to be transient
(defvar trem3-nav-map (make-sparse-keymap)
  "A keymap for navigation.")
(defvar trem3-repeat-map (make-sparse-keymap)
  "A keymap for repetition.")
(defvar trem3-scroll-map (make-sparse-keymap)
  "A keymap for scrolling.")
(defvar trem3-mark-map (make-sparse-keymap)
  "A keymap for fast marking")
(defvar trem3-format-map (make-sparse-keymap)
  "A keymap for text formatting")
(defvar trem3-eob-map (make-sparse-keymap)
  "A keymap for end-of-buffer command (transient)")
(defvar trem3-register-map (make-sparse-keymap)
  "A keymap for fast advanced editing primitives, like reformat-line, reformat-word etc.")

(defvar trem3-help-mode-map (make-sparse-keymap)
  "A keymap for displaying bindings without cyrillic characters.")
(defvar trem3-help-spc-map (make-sparse-keymap)
  "A keymap for displaying bindings without cyrillic characters.")

;;;;;;;;;;;;;;;;;;;
;; REGEX HELPERS ;;
;;;;;;;;;;;;;;;;;;;

(defvar trem3-brackets nil "string of left/right brackets pairs.")
(setq trem3-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠")

(defvar trem3-left-brackets '("\""  "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")


  "List of left bracket chars.")

(defvar trem3-right-brackets '("\"" ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")

(defvar trem3-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq trem3-punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")


;;;;;;;;;;;;;;;;
;; NAVIGATION ;;
;;;;;;;;;;;;;;;;

(defun trem3-cycle-buffer-bounds ()
  "Jumps to:
Beginning of buffer when called first time.
End of buffer on subsequent call.
Initial position before first jump on next subsequent call."
  (interactive)
  (when (not (eq last-command this-command))
    (put this-command 'state 0))
  (cond
     ;; on first call navigate to the beginnig of buffer
     ((equal 0 (get this-command 'state))
      (beginning-of-buffer)
      (put this-command 'state 1))
     ;; on second subsequent call navigate to the end of buffer
     ((equal 1 (get this-command 'state))
      (pop-global-mark)
      (end-of-buffer)
      (put this-command 'state 2))
     ;; on third subsequent call navigate back to starting position
     ((equal 2 (get this-command 'state))
      (pop-global-mark)
      (put this-command 'state 0))))

;; scrolling 
(defvar trem3-scroll-length 2
  "Amount of lines, scrolled per one scroll command.")

(defun trem3-scroll-up ()
  "Scroll up by `trem3-scroll-length' lines."
  (interactive)
  (scroll-up trem3-scroll-length))

(defun trem3-scroll-down ()
  "Scroll down by `trem3-scroll-length' lines."
  (interactive)
  (scroll-down trem3-scroll-length))

(defun trem3-beginning-of-line-or-paragraph ()
  "Move cursor to beginning of line text. 
If the cursor is at the beginning of line text, call beginning-of-line
If the cursor is at the beginning of line call backward-paragraph."
  (interactive)
  (when (not (eq last-command this-command))
    (put this-command 'state 0))
  (cond
   ((or (equal (point) (line-beginning-position))
        (equal 2 (get this-command 'state)))
    (put this-command 'state 0)
    (backward-paragraph))
   ((equal 0 (get this-command 'state))
    (put this-command 'state 1)
    (beginning-of-line-text))
   ((equal 1 (get this-command 'state))
    (put this-command 'state 2)
    (beginning-of-line))))

;; (defun trem3-beginning-of-line-or-paragraph ()
;;   "Move cursor to beginning of line text. 
;; If the cursor is at the beginning of line text, call beginning-of-line
;; If the cursor is at the beginning of line call backward-paragraph."
;;   (interactive)
;;   (if (or (equal (point) (line-beginning-position))
;;           (eq last-command this-command))
;;       (backward-paragraph)
;;     (beginning-of-line-text)))

(defun trem3-end-of-line-or-paragraph ()
  "Move cursor to end of line. 
If cursor is at the end of line text, call forward-paragraph."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (forward-paragraph)
    (end-of-line)))

(defun trem3-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `trem3-punctuation-regex'"
  (interactive "p")
  (re-search-forward trem3-punctuation-regex nil t n))
(defun trem3-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `trem3-forward-punct'"
  (interactive "p")
  (re-search-backward trem3-punctuation-regex nil t n))

(defun trem3-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `trem3-left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt trem3-left-brackets) nil t))

(defun trem3-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `trem3-right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt trem3-right-brackets) nil t))

;;;;;;;;;;;;
;; INSERT ;;
;;;;;;;;;;;;

(defun trem3-append-at-eol-space ()
  "Go to end of line, format it to just one space at the end if it's not blank and leave CMD mode."
  (interactive)
  (end-of-line)
  (unless (looking-back "^[ \t]*$") 
    (just-one-space))
  (trem3-global-mode -1))

(defun trem3-append-at-eol ()
  "Go to end of line and exit trem3."
  (interactive)
  (end-of-line)
  (trem3-global-mode -1))

(defun trem3-open-above ()
  "Go to beginning of line, then open line above and exit trem3."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (trem3-global-mode -1))

(defun trem3-open-below ()
  "Go to end of line, then newline-and-indent and exit trem3."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (trem3-global-mode -1))

(defun trem3-append-split-line ()
  "Split line and exit trem3."
  (interactive)
  (split-line)
  (trem3-global-mode -1))

;;;;;;;;;;;
;; SHELL ;;
;;;;;;;;;;;

(defun trem3-shell-pipe ()
  "Run a shell command on highlighted thing replace thing with its output.
Things are identified and highlighted by `atp-mode'."
  (interactive)
  (atp-apply #'(lambda (beg end)
		 (shell-command-on-region beg end trem3-shell nil 1))))

;;;;;;;;;;;;;
;; MARKING ;;
;;;;;;;;;;;;;

(defun trem3-mark-line ()
  "Select current line, or select next line if called again."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

(defun trem3-toggle-mark ()
  "Set mark if it's inactive, deactivate it if it's active."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
      (command-execute #'set-mark-command)))

;;;;;;;;;;;;;
;; EDITING ;;
;;;;;;;;;;;;;

;; enable indentation advice provided by atp
(atp-enable-thing-indentation)

(defun trem3-delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem3-toggle-highlight ()
  (interactive)
  (let ((overlays (cl-remove-if (lambda (ov) (not (overlay-get ov 'trem3-hlt)))
				(overlays-at (point)))))
    (if overlays
	(progn
	  (mapcar #'delete-overlay overlays)
	  (message "Removed overlay."))
      (atp-apply #'(lambda (beg end)
		     (interactive)
		     (let ((ov (make-overlay beg end)))
		       (overlay-put ov 'trem3-hlt t)
		       (overlay-put ov 'face 'trem3-visual-hlt))))))
  (deactivate-mark))

(defun trem3-reformat-whitespaces-to-one-space (@begin @end)
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

(defun trem3-space-to-newline ()
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

(defun trem3-change-bracket-pairs ( @from-chars @to-chars)
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

(defun trem3-delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun trem3-reformat-to-multi-lines ( &optional @begin @end @min-length)
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

(defun trem3-clean-empty-lines ()
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

(defun trem3-kill-forward-bracket-text ()
  (interactive)
  (backward-kill-sexp -1))

(defun trem3-kill-backward-bracket-text ()
  (interactive)
  (backward-kill-sexp))

(defun trem3-kill-backward ()
  "Kill selected text or char backward or bracket pair."
  (interactive)
  (cond
   ((use-region-p)
    (kill-region (region-beginning) (region-end)))
   ((looking-back "\\s(")
    (backward-char)
    (condition-case nil
	(delete-pair 1)
      (scan-error (delete-char 1))))
   ((looking-back "\\s\"")
    (if (nth 3 (syntax-ppss))
	(condition-case nil
	    (progn (backward-char)
		   (delete-pair 1))
	  (scan-error (delete-char 1)))
      (condition-case nil
	  (delete-pair -1)
	(scan-error (delete-char -1)))))
   ((looking-back "\\s)")
    (condition-case nil
	(delete-pair -1)
      (scan-error (delete-char -1))))
   (t (delete-char -1))))

(defun trem3-kill-forward ()
  "Kill selected text or char forward or bracket pair."
  (interactive)
  (cond
   ((use-region-p)
    (kill-region (region-beginning)
		 (region-end)))
   ((looking-at "\\s(")
    (condition-case nil
	(delete-pair 1)
      (scan-error (delete-char 1))))
   ((looking-at "\\s\"")
    (if (nth 3 (syntax-ppss))
	(condition-case nil
	    (progn (forward-char)
		   (delete-pair -1))
	  (scan-error (delete-char -1)))
      (condition-case nil
	  (delete-pair 1)
	(scan-error (delete-char 1)))))
   ((looking-at "\\s)")
    (condition-case nil
	(progn (forward-char 1)
	       (delete-pair -1))
      (scan-error (delete-char -1))))
   (t (delete-char 1))))

(defun trem3-change ()
  "Kill hightlighted thing and exit trem3.
Thing is identified by `atp-mode'"
  (interactive)
  (let ((atp-include-newline nil))
    (atp-update-thing)
    (atp-apply #'delete-region))
  (trem3-global-mode -1))

;;;;;;;;;;
;; HELP ;;
;;;;;;;;;;

(defun trem3-help-map ()
  "Display help for trem3's single keystroke keymap via `which-key'"
  (interactive)
  (which-key-show-full-keymap 'trem3-help-mode-map))

(defun trem3-help-map-2 ()
  "Display help for trem3's double keystroke keymap via `which-key'"
  (interactive)
  (which-key-show-full-keymap 'trem3-help-spc-map))


;;;;;;;;;;;;;;
;; BINDINGS ;;
;;;;;;;;;;;;;;

(defvar trem3-key-pairs (cl-mapcar 'cons
			"qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"	
			"йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"))

(defun trem3-bind-rus (m k d)
  (let ((c (cdr (assoc (aref k 0) trem3-key-pairs))))
    (when c
      (define-key m (kbd (char-to-string c)) d))))

(defun trem3-bind-mode-map (key command)
  "Binds a command in the trem-mode-map"
  ;; bind to actual keymaps
  (define-key trem3-mode-map (kbd key) command)
  (trem3-bind-rus trem3-mode-map key command)
  ;; bind to help-keymap
  (unless (string-equal key "SPC")
    (define-key trem3-help-mode-map (kbd key) command)))

(defun trem3-bind-spc-map (key command)
  "Binds a command in the trem-mode-spc"
  (define-key trem3-spc-map (kbd key) command)
  (trem3-bind-rus 'trem3-spc-map key command)
  ;; bind to help-keymap
  (define-key trem3-help-spc-map (kbd key) command))

;; DEPRECATED, don't use
(defmacro trem3-bind-transient (transient-keymap key command &optional !modeline-indicator)
  `(progn (trem3-bind-spc-map ,key (lambda ()
				     (interactive)
				     (set-transient-map ,transient-keymap t)
				    (command-execute ,command)))
	  (define-key ,transient-keymap ,key ,command)))

;; LEADKEY
(trem3-bind-mode-map "SPC" 'trem3-spc-map)

;; NAVIGATION
;; char/line arrow-like navigation
(trem3-bind-mode-map "i" #'previous-line)
(trem3-bind-mode-map "j" #'backward-char)
(trem3-bind-mode-map "k" #'next-line    )
(trem3-bind-mode-map "l" #'forward-char )

;; word navigation
(trem3-bind-mode-map "u" #'backward-word)
(trem3-bind-mode-map "o" #'forward-word)

;; line/paragraph navigation
(trem3-bind-mode-map ";" #'trem3-end-of-line-or-paragraph)
(trem3-bind-mode-map "h" #'trem3-beginning-of-line-or-paragraph)

;; parens, punctuation navigation and avy
(trem3-bind-mode-map "m" #'trem3-backward-left-bracket)
(trem3-bind-mode-map "." #'trem3-forward-right-bracket)
(trem3-bind-spc-map "u" #'trem3-backward-punct)
(trem3-bind-spc-map "o" #'trem3-forward-punct)
(trem3-bind-mode-map "," #'avy-goto-word-1)

;; scrolling 
(trem3-bind-transient trem3-scroll-map "i" #'trem3-scroll-down)
(trem3-bind-transient trem3-scroll-map "k" #'trem3-scroll-up)

;; start/end of buffer
(trem3-bind-mode-map "8" #'trem3-cycle-buffer-bounds)

;; recenter
(trem3-bind-mode-map "a" #'recenter-top-bottom)

;; goto line
(trem3-bind-spc-map  ";" #'goto-line)
;; search
(trem3-bind-mode-map "n" #'helm-occur)

;; EDITING
;; kill
(trem3-bind-mode-map "f" #'trem3-kill-forward)
(trem3-bind-mode-map "s" #'trem3-kill-backward)
(trem3-bind-mode-map "r" #'trem3-kill-forward-bracket-text)
(trem3-bind-mode-map "w" #'trem3-kill-backward-bracket-text)

;; undo, copy, yank, replace selection
(trem3-bind-mode-map "t" #'undo)
(trem3-bind-mode-map "c" #'atp-copy)
(trem3-bind-mode-map "v" #'yank)
(trem3-bind-spc-map  "v" #'yank-pop)
(trem3-bind-mode-map "p" #'atp-replace-from-kill-ring)

;; comment/uncomment
(trem3-bind-mode-map "z" #'atp-toggle-comment)

;; query-replace
(trem3-bind-spc-map  "r" #'query-replace)

;; multi cursors
(trem3-bind-spc-map  "m" #'mc/edit-beginnings-of-lines)

;; reformat
(trem3-bind-mode-map  "b" #'atp-toggle-case)
(trem3-bind-transient trem3-format-map "7" #'trem3-reformat)
(trem3-bind-spc-map   "8" #'fill-paragraph)

;; kill ring
(trem3-bind-transient trem3-register-map "n" #'helm-show-kill-ring)

;; MARKING
(trem3-bind-mode-map "e" #'atp-kill)
(trem3-bind-mode-map "d" #'trem3-toggle-mark)

(trem3-bind-spc-map  "i" #'(lambda () (interactive) (message "trem3: 'SPC i' is vacant.")))

;; mark line, paragraph, and whole-buffer
(trem3-bind-mode-map "7" #'trem3-mark-line)
(trem3-bind-mode-map "g" #'atp-mark)
(trem3-bind-spc-map  "h" #'trem3-toggle-highlight)

;; BUFFER AND WINDOW MANAGEMENT
(trem3-bind-mode-map "1" #'make-frame)
(trem3-bind-mode-map "2" #'delete-window)
(trem3-bind-spc-map "2" #'delete-other-windows)
(trem3-bind-mode-map "3" #'other-window)
(trem3-bind-mode-map "4" #'split-window-right)
(trem3-bind-mode-map "5" #'split-window-below)


;; killing buffers
(trem3-bind-mode-map "q" #'kill-buffer)
(trem3-bind-mode-map "<f5>" #'kill-buffer-and-window)
(trem3-bind-spc-map  "q" #'kill-buffer-and-window)

;; opening and jumping
(trem3-bind-spc-map "j" #'switch-to-buffer)
(trem3-bind-spc-map "s" #'save-buffer)
(trem3-bind-spc-map "f" #'helm-find-files)

;; COMMANDS AND EVALUATION
(trem3-bind-mode-map "x" #'helm-M-x)
(trem3-bind-mode-map "6" #'repeat)
(trem3-bind-spc-map "c" (lambda () (interactive) (command-execute trem3-eval-buffer)))
(trem3-bind-spc-map "e" (lambda () (interactive) (atp-apply trem3-eval-region)))
(trem3-bind-spc-map "p" #'trem3-shell-pipe)

;; SHELLS AND TERMINALS
(trem3-bind-spc-map "t" #'eshell)
(trem3-bind-spc-map "y" #'(lambda () (interactive) (vterm)))

;; ENTER INSERT MODE
(trem3-bind-mode-map "y" #'trem3-append-at-eol)
;; (trem3-bind-mode-map "6" #'trem3-append-at-eol)
(trem3-bind-mode-map "/" #'trem3-change)
(trem3-bind-mode-map "9" #'trem3-open-below)
(trem3-bind-mode-map "0" #'trem3-open-above)
(trem3-bind-spc-map  "9" #'trem3-append-split-line)

;; HELP
(trem3-bind-mode-map "`" #'trem3-help-map)
(trem3-bind-spc-map "`" #'trem3-help-map-2)

;;;###autoload
(defun trem3-setup-keybinds (&key jiran)
  "Set up default trem3 keybindings for normal mode."
  (global-subword-mode 1)
  (when jiran
    (progn
      (trem3-bind-mode-map "k" #'previous-line)
      (trem3-bind-mode-map "," #'next-line    )
      (trem3-bind-mode-map "i" #'avy-goto-word-1)
      ;; scrolling:
      (trem3-bind-transient trem3-scroll-map "k" #'trem3-scroll-down)
      (trem3-bind-transient trem3-scroll-map "," #'trem3-scroll-up))))

(provide 'trem3)
;;; trem3.el ends here
