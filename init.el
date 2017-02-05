(setq user-full-name "Patrick Collin Eye")

(setq debug-on-error t)

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

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ))

(blink-cursor-mode 0)

(setq redisplay-dont-pause t)

(global-auto-revert-mode 1)

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

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".c" ".cpp"))
    ("\\.vert\\'" (".frag"))
    ("\\.frag\\'" (".vert"))
    ))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(add-hook 'c-initialization-hook (lambda ()
    (define-key c-mode-base-map [(meta o)] 'ff-get-other-file)))


(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)

(add-hook 'c++-mode-hook 'electric-pair-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . c++-mode))

(which-function-mode t)

(defvar eye/packages '(ace-window
                       avy
                       ivy
		       org
                       swiper)
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
;(add-hook 'prog-mode-hook 'subword-mode)
(global-subword-mode 1)

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(put 'erase-buffer 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default mode-line-format '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                " "
                mode-line-misc-info
                ;; And the modes, which I don't really care for anyway
                " " mode-line-modes mode-line-end-spaces))

(custom-set-variables
 '(global-linum-mode t)
 '(linum-delay t)
 '(linum-eager nil)
 '(windmove-wrap-around t))

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
          (lambda () (c-set-offset 'case-label '+)))

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

(setq compilation-skip-threshold 2)

;ORG MODE
(require 'org)
(setq org-log-done t)
(setq org-startup-truncated nil)

(add-hook 'org-mode-hook (lambda () (visual-line-mode 1) (line-move-visual 1)))

(setq create-lockfiles nil)

(setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;BINDINGS BINDINGS BINDINGS BIDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-key key-translation-map [?\C-h] [?\C-?])
;;(global-set-key [(hyper h)] 'help-command)

(defun beginning-of-line-custom ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))


 (defun isearch-yank-regexp (regexp)
    "Pull REGEXP into search regexp." 
    (let ((isearch-regexp nil)) ;; Dynamic binding of global.
      (isearch-yank-string regexp))
    (isearch-search-and-update))

(defun project-grep ()
  (interactive)
  (let ((working-directory default-directory))
        (cd pce-project-directory)
        (call-interactively 'grep-find)
        (cd working-directory)))

(defun date ()
  (interactive)
  (format-time-string "%d.%m.%Y"))
    
(defun timestamp ()
  (interactive)
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defun pce-todo ()
  (interactive)
  (insert (concat "@pce: ")))

;; customize for C
(setq punctuation '("(" ")"
                    "{" "}"
                    "[" "]"
                    ","
                    "."
                    "!"
                    "->"
                    "&&"
                    "||"
                    "&"
                    "*"
                    ))

(defun forward-punct ()
  (interactive)
  (search-forward-regexp punctuation-regex))

(defun backward-punct ()
  (interactive)
  (search-backward-regexp punctuation-regex))

(defun next-punct ()
  (interactive)
  (forward-punct)
  (forward-punct)
  (backward-punct))

(defun previous-punct ()
  (interactive)
  (backward-punct)
  (backward-punct)
  (forward-punct))

(defun jump-to-next-brace ()
  (interactive)
  (avy-goto-char-in-line ?\{)
  )
                      
(setq punctuation-regex (regexp-opt punctuation))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

    ;; RIGHT HAND
    ;; TOP
    (define-key map (kbd "C-x C-g") 'ff-find-other-file)
    (define-key map (kbd "C-f") 'goto-line)
    (define-key map (kbd "M-f") 'project-grep)
    (define-key map (kbd "M-g") 'avy-goto-line)
    (define-key map (kbd "M-c") 'capitalize-backwards)
    (define-key map (kbd "C-r") 'repeat)
    (define-key map (kbd "M-r") 'replace-string)
    (define-key map (kbd "M-R") 'query-replace)
    ;; MID
    (define-key map (kbd "C-s") 'forward-char)
    (define-key map (kbd "M-s") 'forward-word)


    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "M-n") 'forward-paragraph)
    ;;(define-key map (kbd "C-M-n") 'gud-next)
    (define-key map (kbd "C-x C-n") 'next-error)

    
    (define-key map (kbd "C-x C-b") 'gud-break)
    (define-key map (kbd "C-x C-c") 'pce-todo)

    (define-key map (kbd "C-t") 'previous-line)
    (define-key map (kbd "M-t") 'backward-paragraph)
    (define-key map (kbd "C-M-t") 'avy-goto-line-above)
    (define-key map (kbd "C-x C-t") 'previous-error)
    
    
    (define-key map (kbd "C-h") 'backward-char) 
    (define-key map (kbd "M-h") 'backward-word)
    (define-key map (kbd "C-M-h") 'avy-goto-char-2)
    ;;BOTTOM
    (define-key map (kbd "C-b") 'switch-to-buffer)
    (define-key map (kbd "M-b") 'transpose-words)
    (define-key map (kbd "M-l") 'transpose-lines)
    ;; THUMB

    (keyboard-translate ?\C-m ?\H-m)
    (define-key map [?\H-m] 'comment-region)
    (define-key map (kbd "M-m") 'uncomment-region)

    (define-key map (kbd "C-<return>") 'switch-to-buffer)
    

    ;; LEFT HAND
    ;; TOP
    (define-key map (kbd "C-p") 'other-window)
    (define-key map (kbd "M-p") 'ace-window)
    (define-key map (kbd "C-M-p") 'switch-to-buffer)

    ;; MID
    (define-key map (kbd "C-a") 'beginning-of-line-custom)

    (define-key map (kbd "C-o") 'avy-goto-char-2-above)
    (define-key map (kbd "M-o") 'isearch-backward)
    (define-key map (kbd "C-M-o") 'swiper)
    (define-key isearch-mode-map "\M-o" 'isearch-repeat-backward)

    (define-key map (kbd "C-e") 'avy-goto-char-2-below)
    (define-key map (kbd "M-e") 'isearch-forward)
    (define-key map (kbd "C-M-e") 'swiper)
    (define-key isearch-mode-map "\M-e" 'isearch-repeat-forward)
    
    (define-key map (kbd "C-u") 'end-of-line)
    (define-key map (kbd "M-u") 'forward-sentence) ;; gets rebound to c-end-of-statement after c-mode activated
    (define-key map (kbd "C-M-u") 'end-of-defun)
    (define-key map (kbd "C-M-a") 'beginning-of-defun)
    (keyboard-translate ?\C-i ?\H-i)
    (global-set-key [?\H-i] 'universal-argument)

    ;; BOTTOM
    (define-key map (kbd "C-z") 'avy-goto-char-in-line)
    (define-key map (kbd "M-z") 'avy-goto-word-1)

    (define-key map (kbd "C-k") 'zap-to-char)
    (define-key map (kbd "M-k") 'kill-line)
    
    (define-key map (kbd "C-j") 'forward-sexp)
    (define-key map (kbd "M-j") 'backward-sexp)
    
    (define-key map (kbd "C-q") 'undo)
    (define-key map (kbd "C-x C-q") 'undo)

    (define-key map (kbd "M-RET") 'indent-new-comment-line)

    (define-key map (kbd "C-<tab>") 'hs-show-block)
    (define-key map (kbd "M-<tab>") 'hs-hide-block)
    (define-key map (kbd "C-x C-<tab>") 'hs-hide-all)
    (define-key map (kbd "C-x M-<tab>") 'hs-show-all)

    ;; need all and previous...
    ;; how often do I need to do this instead of just string replace?
    (define-key map (kbd "M-SPC") 'mark-next-like-this)

    (define-key map (kbd "<right>") 'gud-next)
    (define-key map (kbd "<down>") 'gud-step)
    ;; (define-key map (kbd "<up>") 'gud-break)
    ;; (define-key map (kbd "<left>") 'gud-break)

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

(add-hook 'minibuffer-setup-hook
          (lambda () (my-keys-minor-mode -1)
            (define-key ivy-minibuffer-map (kbd "C-t") 'ivy-previous-line)
            (define-key ivy-minibuffer-map (kbd "M-o") 'swiper-avy)
            (define-key ivy-minibuffer-map (kbd "M-e") 'swiper-avy)
            (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)))

(add-hook 'minibuffer-exit-hook (lambda () (my-keys-minor-mode 1)))

(ivy-mode 1)

(defun c-bindings ()
  (define-key my-keys-minor-mode-map (kbd "M-u") 'c-end-of-statement))

(my-keys-minor-mode 1)
(add-hook 'c-mode-common-hook 'c-bindings)

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
  (backward-subword 1)
  ;(backward-word 1)
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

(defun search-tags-org ()
  (interactive)
  ;; search the whole document for every # and list what follows in a buffer along with count
  ;; When one is selected jump between instances of it.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS SPECIFIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pce-compile-etcetera-linux (directory)
  (concat "time python2 " directory "/build.py"))

(defun pce-run-etcetera-linux (directory debug)
  (let ((exec-relative-path "build/etcetera_linux"))
    (gdb (concat "gdb --quiet -i=mi -cd ~/etcetera " "./build/etcetera_linux"))))

(defun pce-compile-etcetera-windows (directory)
  (concat "python " directory "/build.py"))

(defun pce-run-etcetera-windows (directory debug)
  (let ((exec-relative-path "build/etcetera_linux"))
    (shell-command "~/etcetera/build/etcetera_linux")))

(defun pce-debug-preprocessor ()
  (interactive)
  (let ((exec-relative-path "build/preprocssor"))
    (gdb (concat "gdb --quiet -i=mi -cd ~/etcetera " "./build/preprocessor"))))

(custom-set-faces
        '(default ((t (:background "Black" :foreground "#49FF00" :weight normal :height 100 :width normal))))
        '(error ((t (:foreground "Red" :weight bold))))
        '(font-lock-builtin-face ((t (:foreground "#bf55ff"))))
        '(font-lock-comment-face ((t (:foreground "gray50"))))
        '(font-lock-constant-face ((t (:foreground "Yellow"))))
        '(font-lock-function-name-face ((t (:foreground "#ff1420"))))
        '(font-lock-keyword-face ((t (:foreground "#cf55ff"))))
        '(font-lock-negation-char-face ((t (:foreground "#ff0923"))))
        '(font-lock-string-face ((t (:foreground "White"))))
        '(font-lock-type-face ((t (:foreground "DodgerBlue"))))
        '(font-lock-variable-name-face ((t (:foreground "Yellow"))))
        '(font-lock-which-func-face ((t (:foreground "Yellow"))))
        '(highlight ((t (:background "gray20"))))
        '(which-func ((t (:foreground "blue")))))

(cond ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :family "Meslo LG S" :height 100)
       (global-set-key (kbd "C-x m") 'pce-compile-project)
       (global-set-key (kbd "C-x C-r") 'pce-run-project-debug)
       (pce-define-project 'etcetera "~/etcetera" 'pce-compile-etcetera-linux 'pce-run-etcetera-linux)
       (cd "~/"))

      ((eq system-type 'windows-nt)

       (set-face-attribute 'default nil :family "Consolas" :height 100)
       
       (global-set-key (kbd "C-x m") 'pce-compile-project)
       (global-set-key (kbd "C-x C-r") 'pce-run-project)
       (pce-define-project 'etcetera "~/etcetera" 'pce-compile-etcetera-windows 'pce-run-etcetera-windows))

      ((eq system-type 'darwin)

       (set-face-attribute 'default nil :family "Menlo" :height 100)))
