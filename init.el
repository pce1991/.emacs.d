(setq user-full-name "Patrick Collin Eye")

(set-frame-parameter nil 'fullscreen 'fullboth)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(if (eq (length (window-list)) 1)
    (progn
        (split-window-right)
        (balance-windows)))

(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(load "package")
(package-initialize)

;;(shell)

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ))

(blink-cursor-mode 0)

(setq redisplay-dont-pause t)

(defun set-window-undedicated-p (window flag)
 "Never set window dedicated."
 flag)

(advice-add 'set-window-dedicated-p :override #'set-window-undedicated-p)

(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-hl-line-mode t)


(add-hook 'csharp-mode-hook 'electric-pair-mode)
(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)

(defvar eye/packages '(ace-isearch
                       ace-window
                       avy
		       clojure-mode
		       company
                       dictionary
                       helm
                       helm-swoop
                       irony
		       magit
                       multiple-cursors
		       org
		       paredit
                       smartscan
                       yasnippet
                       zeal-at-point)
  "Default packages")


(defun eye/packages-installed-p ()
  (loop for pkg in eye/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (eye/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg eye/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix) 

(show-paren-mode 1)
(setq show-paren-delay 0) 

(global-linum-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

;; only want this to show things already defined...
;;(add-hook 'after-init-hook 'global-company-mode)

; YA-Snippet
;(global-set-key (kbd "C-c s") 'company-yasnippet)

(put 'erase-buffer 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FONTS AND COLORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  '(global-linum-mode t)
 '(linum-delay t)
 '(linum-eager nil)
 '(windmove-wrap-around t))

;; Most important: types, variables, function-name
(custom-set-faces '(default ((t (:inherit nil :stipple
  nil :background "Black" :foreground "Green" :inverse-video
  nil :box nil :strike-through nil :overline nil :underline
  nil :slant normal :weight normal :height 98 :width
  normal :foundry "unknown" :family "Liberation Mono"))))
  '(error ((t (:foreground "Red" :weight bold))))
  '(font-lock-builtin-face ((t (:foreground "DodgerBlue"))))
  '(font-lock-comment-face ((t (:foreground "gray50"))))
  '(font-lock-constant-face ((t (:foreground "Yellow"))))
  '(font-lock-function-name-face ((t (:foreground "Red"))))
  '(font-lock-keyword-face ((t (:foreground "Purple"))))
  '(font-lock-negation-char-face ((t (:foreground "Red"))))
  '(font-lock-string-face ((t (:foreground "White"))))
  '(font-lock-type-face ((t (:foreground "Orange"))))
  '(font-lock-variable-name-face ((t (:foreground "Yellow"))))
  '(highlight ((t (:background "gray20")))))

(font-lock-add-keywords
 'c-mode
 '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(require 'linum)

(defcustom linum-disabled-modes-list '(eshell-mode erc-mode org-mode wl-summary-mode compilation-mode text-mode dired-mode twittering-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum)

(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
	      (string-match "*" (buffer-name)))
    (linum-mode 1)))
(provide 'linum-off)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default c-basic-offset 4)
;indent switch statements
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))


(defvar pce-project-run-fn nil)

(defun pce-define-project (name directory compile-fn run-fn)
  (let ((class-variables (list (cons nil (list (cons 'pce-project-directory directory)
                                               (cons 'pce-project-compile-fn compile-fn)
                                               (cons 'pce-project-run-fn run-fn))))))
    (dir-locals-set-class-variables name class-variables))

  (add-to-list 'safe-local-variable-values (cons 'pce-project-directory directory))
  (add-to-list 'safe-local-variable-values (cons 'pce-project-compile-fn compile-fn))
  (add-to-list 'safe-local-variable-values (cons 'pce-project-run-fn run-fn))
  (dir-locals-set-directory-class directory name))

(defun pce-compile-project ()
  (interactive)
  (if (get-buffer "*compilation*")
      (kill-buffer "*compilation*"))

  (if (and pce-project-directory pce-project-compile-fn)
      (let ((working-directory default-directory)
            (prev-compile-command compile-command))
        (cd pce-project-directory)
        (setq compilation-read-command nil)
        (setq compile-command (funcall pce-project-compile-fn pce-project-directory))

        (call-interactively 'compile)

        (setq compile-command prev-compile-command)
        (cd working-directory))))

(defun pce-run-project ()
  (interactive)
  (if pce-project-run-fn
      (funcall pce-project-run-fn pce-project-directory nil)))

(defun pce-run-project-debug ()
  (interactive)
  (if pce-project-run-fn
      (funcall pce-project-run-fn pce-project-directory t)))

;ORG MODE
(require 'org)
(setq org-log-done t)
(setq org-startup-truncated nil)

(add-hook 'org-mode-hook (lambda () (visual-line-mode 1) (line-move-visual 1)))

(setq create-lockfiles nil)

;;HELM
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq shackle-mode 1)
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align 'bottom :ratio 0.2)))

(set-face-attribute 'helm-selection nil
                    :background "white"
                    :foreground "black")



;; Define something other than j and l! (define-key helm-mode-map [tab] 'a-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;BINDINGS BINDINGS BINDINGS BIDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; think of useful prefix commands, things that make sense, maybe rebind to prefix keys? to k and m?
;; think of most commonly used keybindings and place them on least stressful keys.
;; Navigation keys: left, right for char and word, line, sentences, delimeters,
;;                                                       (want to jump to beginning or end easily)
;;                  up down for lines, blocks,
;;                  Delete forward, back, within
;;                  Comment/uncomment
;;                  Mark, mark-like-this
;;                  Kill-ring, kill, copy, paste, zap to char, undo
;;                  Repeat previous command
;;                  change buffer, frame
;;                  open/save file
;;                  Replace string (prompt and unprompted)
;;                  Search, jump-to-char (on line and frame)
;;                  Go to line
;;                  M-^ delete-indentation
;;                  Define/execute macro, save macro
;;                  Transpose line,character (forward and back), and word
;;                  Center/Top/Bottom line
;;                  begining-of-defun/end-of-defun
;;                  forwrad-list/backward-list
;;                  smartscan
;;                  Switch buffer


;;(define-key key-translation-map [?\C-h] [?\C-?])
;;(global-set-key [(hyper h)] 'help-command)

(defun beginning-of-line-custom ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun jump-to-open-bracket ()
  ;; avy jump to brackets only.
  (interactive)
  (avy-goto-char ?\{))

(defun jump-to-close-bracket ()
  ;; avy jump to brackets only.
  (interactive)
  (avy-goto-char ?\}))


 (defun isearch-yank-regexp (regexp)
    "Pull REGEXP into search regexp." 
    (let ((isearch-regexp nil)) ;; Dynamic binding of global.
      (isearch-yank-string regexp))
    (isearch-search-and-update))

(defun forward-bracket ()
  (interactive)
  ;;(isearch-forward-regexp nil nil)
  ;; Oh, this is searching for the regex itself in the buffer
  ;; not the string that matches it!
  ;;(isearch-forward 1 1)
  ;;(isearch-yank-regexp (regexp-quote "{\\|}"))
  ;;(isearch-yank-string (string ?{ ?\\ ?| ?}))
  (isearch-forward nil 1)
  (isearch-yank-string "{")
  )



(defun backward-bracket ()
  (interactive)
  (isearch-backward nil 1)
  (isearch-yank-string "}")
  ;;(isearch-backward 1 1)
  ;;(isearch-yank-string "{|}")
  )


(defun project-grep ()
  (interactive)
  (cd pce-project-directory)
  (call-interactively 'grep-find))

;my own minor mode to overwrite any key bindings I dislike. 
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

    ;; RIGHT HAND
    ;; TOP
    (define-key map (kbd "C-f") 'goto-line)
    (define-key map (kbd "M-f") 'project-grep)
    (define-key map (kbd "M-G") 'goto-line)
    (define-key map (kbd "M-c") 'capitalize-backwards)
    (define-key map (kbd "C-r") 'repeat)
    (define-key map (kbd "M-r") 'replace-string)
    (define-key map (kbd "M-R") 'query-replace)
    ;; MID
    (define-key map (kbd "C-s") 'forward-char)
    (define-key map (kbd "M-s") 'forward-word)

    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "M-n") 'end-of-defun)
    (define-key map (kbd "C-M-n") 'gud-next)
    (define-key map (kbd "C-x C-n") 'next-error)

    (define-key map (kbd "C-M-s") 'gud-step)
    (define-key map (kbd "C-x C-b") 'gud-break)
    (define-key map (kbd "C-x C-c") 'gud-break)

    (define-key map (kbd "C-t") 'previous-line)
    (define-key map (kbd "M-t") 'beginning-of-defun)
    (define-key map (kbd "C-x C-t") 'previous-error)
    (define-key map (kbd "C-M-t") 'gud-previous)
    
    
    (define-key map (kbd "C-h") 'backward-char) 
    (define-key map (kbd "M-h") 'backward-word)
    ;;BOTTOM
    (define-key map (kbd "C-b") 'transpose-chars)
    (define-key map (kbd "M-b") 'transpose-words)
    (define-key map (kbd "M-l") 'transpose-lines)
    ;; THUMB

    (keyboard-translate ?\C-m ?\H-m)
    (define-key map [?\H-m] 'comment-region)
    (define-key map (kbd "M-m") 'uncomment-region)
    

    ;; LEFT HAND
    ;; TOP
    (define-key map (kbd "C-p") 'other-window)
    ;; C-0-9 for ace window
    ;; (define-key map (kbd "C-1") '(ace-window 1))
    ;; (define-key map (kbd "C-2") (lambda () (ace-window 2)))
    ;; (define-key map (kbd "C-3") (lambda () (ace-window 3)))
    ;; (define-key map (kbd "C-4") (lambda () (ace-window 4)))
    ;;(define-key map (kbd "M-p") 'other-frame)
    (define-key map (kbd "M-p") 'ace-window)
    (define-key map (kbd "C-M-p") 'switch-to-buffer)

    ;; MID
    (define-key map (kbd "C-a") 'beginning-of-line-custom)

    (define-key map (kbd "C-o") 'isearch-backward)
    (define-key map (kbd "M-o") 'backward-bracket)
    (define-key isearch-mode-map "\C-o" 'isearch-repeat-backward)
    (define-key isearch-mode-map "\M-o" 'isearch-repeat-backward)

    (define-key map (kbd "C-e") 'isearch-forward)
    (define-key map (kbd "M-e") 'forward-bracket)
    (define-key isearch-mode-map "\C-e" 'isearch-repeat-forward)
    (define-key isearch-mode-map "\M-e" 'isearch-repeat-forward)
    
    
    (define-key map (kbd "C-u") 'end-of-line)
    (define-key map (kbd "M-u") 'forward-sentence) ;; gets rebound to c-end-of-statement after c-mode activated
    (keyboard-translate ?\C-i ?\H-i)
    (global-set-key [?\H-i] 'universal-argument)

    ;; BOTTOM
    (define-key map (kbd "C-z") 'avy-goto-char-2)
    (define-key map (kbd "M-z") 'avy-goto-word-1)

    (define-key map (kbd "C-j") 'jump-to-open-bracket)
    (define-key map (kbd "M-j") 'jump-to-close-bracket)
    
    (define-key map (kbd "C-q") 'undo)

    (define-key map (kbd "M-RET") 'indent-new-comment-line)

    (define-key map (kbd "C-<tab>") 'hs-show-block)
    (define-key map (kbd "M-<tab>") 'hs-hide-block)
    (define-key map (kbd "C-x <tab>") 'hs-hide-all)

    ;; need all and previous...
    ;; how often do I need to do this instead of just string replace?
    (define-key map (kbd "M-SPC") 'mark-next-like-this)

    

    (define-key map (kbd "<right>") 'gud-next)
    (define-key map (kbd "<down>") 'gud-step)
    (define-key map (kbd "<up>") 'gud-break)
    (define-key map (kbd "<left>") 'gud-break)

    ;; GUD-BINDINGS (arrows and Function keys)
   map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :global t
  :init-value t
  :lighter "my-keys")

;; I want a minor mode to have different bindings depending on the major-mode
(defun my-c-mode-hook ()
  (let ((oldmap (cdr (assoc 'my-keys minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "M-u") 'c-end-of-statement)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(my-keys . ,newmap) minor-mode-overriding-map-alist)))

(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; (defvar shell-minor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-t") ') ;; gets rebound to c-end-of-statement after c-mode activated
;;     map)
;;   "shell-minor-mode-map")

(defun c-bindings ()
  (define-key my-keys-minor-mode-map (kbd "M-u") 'c-end-of-statement))

;; (defun org-bindings ()
;;   (define-key my-keys-minor-mode-map (kbd "M-u") 'forward-sentence))

;; (defun shell-bindings ()
;;   (define-key my-keys-minor-mode-map (kbd "C-t") 'comint-previous-prompt)
;;   (define-key my-keys-minor-mode-map (kbd "C-n") 'comint-next-prompt))

(my-keys-minor-mode 1)
(add-hook 'c-mode-common-hook 'c-bindings)
;; (add-hook 'org-mode-hook 'org-bindings)
;; (add-hook 'shell-mode-hook 'shell-bindings)


;; maybe put this as a modal mode, but make sure navigation stuff is unaffected...
;; just use function keys?
(defvar gud-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; break at line
    ;; break at function
    ;; print var/expression
    ;; watch var
    ;; run
    ;; step
    ;; next
    ;; continue
    ;; quit
    map)
  "gud-minor-mode-map")

(define-minor-mode gud-minor-mode-map
  :init-value t)

;(add-hook 'gud-minor-mode-map )

;(global-set-key (kbd "C-?") 'dictionary-search)


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\â”‚\Â·*\n" "\W*â”‚\Â·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "M-q") (fill-paragraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;LISP STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it. 
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(global-set-key (kbd "C-;") #'comment-or-uncomment-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reverse-words (beg end)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region beg end) "\\b"))))

(defun reverse-chars (beg end)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (string-to-list (delete-and-extract-region beg end)))))

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer


(defun capitalize-backwards ()
  (interactive)
;;  (backward-subword 1)
  (backward-word 1)
  (capitalize-word 1))

;;; Ideally it'd do all this without actually opening the buffers so that I could just continue with my work
(defun save-word ()
  "Will look up the current word in gcide, will save it and add a link to the org dictionary."
  (interactive)
  (let ((word (current-word)))	
    (dictionary-search word)
    (write-file (concat "~/writing/dictionary/" word))
    (kill-buffer word)
    (find-file "~/writing/dictionary/dictionary.org")
    (end-of-buffer)
    (org-insert-link (concat "~/writing/dictionary/" word)
		     (concat "~/writing/dictionary/" word)
		     (capitalize word))
    (write-file "~/writing/dictionary/dictionary.org")
    (kill-buffer "dictionary.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS SPECIFIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pce-compile-etcetera-linux (directory)
  (concat "ninja -f " directory "/build.ninja"))

(defun pce-run-etcetera-linux (directory debug)
  (let ((exec-relative-path "build/etcetera_linux"))
    (if debug
        (progn
          (gdb (concat "gdb -i=mi -cd ~/etcetera " "./build/etcetera_linux")))
      (progn
        (shell-command "~/etcetera/build/etcetera_linux")))))

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "C-x m") 'pce-compile-project)
  (global-set-key (kbd "C-x C-r") 'pce-run-project-debug)
  (pce-define-project 'etcetera "~/etcetera" 'pce-compile-etcetera-linux 'pce-run-etcetera-linux)
  (cd "~/"))
