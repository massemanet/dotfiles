;;; init --- an init file.
;;; Commentary:
;;; an init file.
;;; Code:
;;; an init file.

;; init straight
(let* ((bootstrap-path "straight/repos/straight.el/bootstrap.el")
       (bootstrap-file (expand-file-name bootstrap-path user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq custom-safe-themes
      '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
        default))

;; select packages
(straight-use-package '(flycheck :fork "massemanet/flycheck"))
(straight-use-package 'auctex)
(straight-use-package 'auto-complete)
(straight-use-package 'bazel)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'erlang)
(straight-use-package 'expand-region)
(straight-use-package 'flycheck-popup-tip)
(straight-use-package 'macrostep)
(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'magit-popup)
(straight-use-package 'magit-todos)
(straight-use-package 'markdown-mode)
(straight-use-package 'markdown-toc)
(straight-use-package 'multiple-cursors)
(straight-use-package 'nyan-mode)
(straight-use-package 'protobuf-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'smart-mode-line)
(straight-use-package 'string-inflection)
(straight-use-package 'web-mode)
(straight-use-package 'yaml-mode)

;; my paths
(add-to-list 'load-path (concat user-emacs-directory "masserlang"))

;; needed to make flycheck happy
(require 'ediff)
(require 'flycheck)
(require 'flycheck-popup-tip)
(require 'ispell)
(require 'magit)
(require 'masserlang)
(require 'nyan-mode)
(require 'smart-mode-line)
(require 'straight)

;; turn on good shit
(column-number-mode)
(delete-selection-mode 1)
(flycheck-popup-tip-mode)
(global-flycheck-mode)
(global-font-lock-mode)
(ido-mode)
(nyan-mode)
(show-paren-mode)
(transient-mark-mode)

;; turn off bad shit
(if (featurep 'tool-bar) (tool-bar-mode -1))
(if (featurep 'tooltip)  (tooltip-mode  -1))
(if (featurep 'menu-bar) (menu-bar-mode -1))

;; faces etc
;;(message "%s" (face-all-attributes 'font-lock-type-face))
(set-face-foreground font-lock-type-face "yellow green")
(set-face-attribute 'mode-line nil :height 190)
(sml/apply-theme 'dark)
(sml/setup)

;; configs
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(setq
 default-input-method                "rfc1345"
 ediff-window-setup-function         'ediff-setup-windows-plain
 flycheck-emacs-lisp-load-path       'inherit
 inhibit-startup-screen              t
 ispell-program-name                 "aspell"
 ispell-really-aspell                t
 ispell-silently-savep               t
 magit-define-global-key-bindings    nil
 max-lisp-eval-depth                 40000
 ring-bell-function                  'ignore
 scroll-down-aggressively            0.1
 scroll-up-aggressively              0.1
 visible-bell                        nil)

;; keybindings
(global-unset-key (kbd "C-x C-z"))

;;; immediates. be frugal with these.
(global-set-key (kbd "C-/")  'ispell-word)
(global-set-key (kbd "C-\"") 'mc/mark-all-like-this)
(global-set-key (kbd "C-'")  'mc/hide-unmatched-lines-mode)
(global-set-key (kbd "C->")  'er/expand-region)
(global-set-key (kbd "C-<")  'er/contract-region)
(global-set-key (kbd "C-j")  'scroll-down)
(global-set-key (kbd "C-z")  'undo) ; be like a mac
(global-set-key (kbd "M-z")  'undo) ; if screen eats C-z

;;; C-x for user specified.
(global-set-key (kbd "C-S-v")     'scroll-down-command)

(global-set-key (kbd "C-x #")     'server-edit)
(global-set-key (kbd "C-x '")     'expand-abbrev)
(global-set-key (kbd "C-x (")     'kmacro-start-macro)
(global-set-key (kbd "C-x )")     'kmacro-end-macro)
(global-set-key (kbd "C-x *")     'calc-dispatch)
(global-set-key (kbd "C-x +")     'balance-windows)
(global-set-key (kbd "C-x ,")     'beginning-of-buffer)
(global-set-key (kbd "C-x -")     'shrink-window-if-larger-than-buffer)
(global-set-key (kbd "C-x .")     'end-of-buffer)
(global-set-key (kbd "C-x 0")     'delete-window)
(global-set-key (kbd "C-x 1")     'delete-other-windows)
(global-set-key (kbd "C-x 2")     'split-window-below)
(global-set-key (kbd "C-x 3")     'split-window-right)
;;              (kbd "C-x 4")     PREFIX(other window)
;;              (kbd "C-x 5")     PREFIX(frame)
;;              (kbd "C-x 6")     PREFIX(2 column)
;;              (kbd "C-x 8")     PREFIX(unicode)
(global-set-key (kbd "C-x ;")     'eval-expression)
(global-set-key (kbd "C-x C-SPC") 'pop-global-mark)
(global-set-key (kbd "C-x C-[")   'previous-error)
(global-set-key (kbd "C-x C-]")   'next-error)
(global-set-key (kbd "C-x C-b")   'ibuffer-list-buffers)
(global-set-key (kbd "C-x C-c")   'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-e")   'eval-last-sexp)
(global-set-key (kbd "C-x C-f")   'find-file)
;;              (kbd "C-x C-k")   PREFIX(kmacro)
(global-set-key (kbd "C-x C-q")   'query-replace)
(global-set-key (kbd "C-x C-r")   'revert-buffer)
(global-set-key (kbd "C-x C-s")   'save-buffer)
(global-set-key (kbd "C-x C-t")   'transpose-lines)
(global-set-key (kbd "C-x C-u")   'string-inflection-all-cycle)
(global-set-key (kbd "C-x C-v")   'find-alternate-file)
(global-set-key (kbd "C-x C-w")   'write-file)
(global-set-key (kbd "C-x C-x")   'execute-extended-command)
(global-set-key (kbd "C-x ESC")   'insert-tilde)
(global-set-key (kbd "C-x O")     'switch-to-previous-buffer)
;;              (kbd "C-x RET")   PREFIX(coding system)
(global-set-key (kbd "C-x SPC")   'rectangle-mark-mode)
(global-set-key (kbd "C-x TAB")   'fixup-buffer)
(global-set-key (kbd "C-x [")     'flycheck-previous-error)
(global-set-key (kbd "C-x ]")     'flycheck-next-error)
(global-set-key (kbd "C-x a")     'align-regexp)
(global-set-key (kbd "C-x b")     'switch-to-buffer)
(global-set-key (kbd "C-x d")     'dired)
(global-set-key (kbd "C-x e")     'kmacro-end-and-call-macro)
(global-set-key (kbd "C-x f")     'xref-find-definition)
(global-set-key (kbd "C-x g")     'goto-line)
(global-set-key (kbd "C-x h")     'mark-whole-buffer)
(global-set-key (kbd "C-x i")     'insert-file)
(global-set-key (kbd "C-x k")     'kill-buffer)
(global-set-key (kbd "C-x l")     'count-lines-page)
(global-set-key (kbd "C-x m")     'isearch-forward-regexp)
(global-set-key (kbd "C-x n")     'forward-list)
(global-set-key (kbd "C-x o")     'prev-window)
(global-set-key (kbd "C-x p")     'backward-list)
(global-set-key (kbd "C-x q")     'fill-paragraph)
;;              (kbd "C-x r")     Prefix(rectangle)
(global-set-key (kbd "C-x s")     'just-one-space)
(global-set-key (kbd "C-x t")     'transpose-words)
(global-set-key (kbd "C-x u")     'undo)
(global-set-key (kbd "C-x v")     'scroll-down)
(global-set-key (kbd "C-x w")     'kill-ring-save)
(global-set-key (kbd "C-x x")     'switch-mark-and-point)
(global-set-key (kbd "C-x y")     'yank-pop)

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-n") 'next-history-element)
  (define-key map (kbd "C-p") 'previous-history-element))

;; hooks
(add-hook
 'after-init-hook
 (lambda ()
   (set-exec-path)
   (load-theme 'tsdh-dark)))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (flyspell-mode)))

(add-hook
 'ediff-mode-hook
 (lambda ()
   (set-face-attribute 'ediff-fine-diff-B nil :background "#055505")))

;; utilities
(defun switch-mark-and-point ()
  "Switch mark and point."
  (interactive)
  (exchange-point-and-mark t))

(defun set-exec-path ()
  "Set up Emacs' variable `exec-path' and PATH environment variable."
  (interactive)
  (let* ((shell-path (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))
         (path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" shell-path)))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun ido-kill-emacs-hook ()
  "Ido is annoying."
  (declare-function ido-save-history "ext:")
  (ignore-errors (ido-save-history)))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun fixup-buffer ()
  "Fixup buffer."
  (interactive)
  ;;  (font-lock-ensure)
  ;;  (font-lock-fontify-buffer)
  (font-lock-flush)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (whitespace-cleanup-region (point-min) (point-max)))

(defun prev-window ()
  "Select previous window."
  (interactive)
  (select-window (previous-window (selected-window) nil nil)))

(provide 'init)
;;; init.el ends here
