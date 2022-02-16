;;; init --- an init file.
;;; Commentary:
;;; an init file.
;;; Code:
;;; an init file.

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

;; select packages
(straight-use-package 'doom-modeline)
(straight-use-package '(flycheck :fork "massemanet/flycheck"))
(straight-use-package 'flycheck-popup-tip)
(straight-use-package 'erlang)
(straight-use-package 'company-erlang)
(straight-use-package 'json-mode)
(straight-use-package 'auctex)
(straight-use-package 'json-reformat)
(straight-use-package 'json-snatcher)
(straight-use-package 'macrostep)
(straight-use-package 'yaml-mode)
(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'magit-popup)
(straight-use-package 'magit-todos)
(straight-use-package 'markdown-mode)
(straight-use-package 'markdown-toc)
(straight-use-package 'nyan-mode)
(straight-use-package 'rainbow-delimiters)

;; needed to make flycheck happy
(require 'straight)
(require 'nyan-mode)
(require 'flycheck)
(require 'flycheck-popup-tip)
(require 'doom-modeline)
(require 'ediff)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; turn on good shit
(show-paren-mode t)
(transient-mark-mode t)
(global-font-lock-mode t)
(delete-selection-mode 1)
(ido-mode t)
(nyan-mode 1)
(global-flycheck-mode)
(flycheck-popup-tip-mode)
(doom-modeline-mode)

(fset 'yes-or-no-p 'y-or-n-p)

;; turn off bad shit
(if (featurep 'tool-bar)   (tool-bar-mode   -1))
(if (featurep 'tooltip)    (tooltip-mode    -1))
(if (featurep 'menu-bar)   (menu-bar-mode   -1))

;; configs
(setq-default indent-tabs-mode nil)
(setq
 indent-tabs-mode               nil
 ediff-window-setup-function    'ediff-setup-windows-plain
 inhibit-startup-screen         t
 visible-bell                   nil
 default-input-method           "rfc1345"
 max-lisp-eval-depth            40000
 scroll-down-aggressively       0.1
 scroll-up-aggressively         0.1)

;; keybindings
(global-set-key (kbd "C-j")     `scroll-down)
(global-set-key (kbd "C-x !")   `shell-command)
(global-set-key (kbd "C-x %")   `query-replace)
(global-set-key (kbd "C-x ,")   'beginning-of-buffer)
(global-set-key (kbd "C-x .")   'end-of-buffer)
(global-set-key (kbd "C-x ;")   'eval-expression)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-y") `yank-pop)
(global-set-key (kbd "C-x O")   `switch-to-previous-buffer)
(global-set-key (kbd "C-x T")   `transpose-words)
(global-set-key (kbd "C-x [")   'flycheck-previous-error)
(global-set-key (kbd "C-x ]")   'flycheck-next-error)
(global-set-key (kbd "C-x a")   'align-regexp)
(global-set-key (kbd "C-x g")   `goto-line)
(global-set-key (kbd "C-x n")   `forward-list)
(global-set-key (kbd "C-x o")   'prev-window)
(global-set-key (kbd "C-x p")   `backward-list)
(global-set-key (kbd "C-x q")   `fill-paragraph)
(global-set-key (kbd "C-x t")   `transpose-lines)
(global-set-key (kbd "C-x v")   `scroll-down)
(global-set-key (kbd "C-x w")   `kill-ring-save)
(global-set-key (kbd "C-x {")   `previous-error)
(global-set-key (kbd "C-x }")   `next-error)
(global-set-key (kbd "C-z")     'undo) ; be like a mac
(global-set-key (kbd "M-z")     'undo) ; if screen eats C-z

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-n")   'next-history-element)
  (define-key map (kbd "C-p")   'previous-history-element))

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-n")   'next-history-element)
  (define-key map (kbd "C-p")   'previous-history-element))

;; hooks
(add-hook
 'after-init-hook
 (lambda ()
   (set-exec-path)
   (load-theme 'tsdh-dark)
   (local-loader
    '(hippierl masserlang fdlcap)
    (list "~/git/" user-emacs-directory))))

(add-hook
 'ediff-mode-hook
 (lambda ()
   (set-face-attribute 'ediff-fine-diff-B nil :background "#055505")))

;; utilities
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

(defun prev-window ()
  "Select previous window."
  (interactive)
  (select-window (previous-window (selected-window) nil nil)))

(defun local-loader (features paths)
  "Look in PATHS for FEATURES to add to load path."
  (mapc
   (lambda (feature)
     (mapc
      (lambda (path)
	(let ((fp (concat path (symbol-name feature))))
	  (if (file-exists-p fp)
	      (add-to-list 'load-path fp))))
      paths))
   features))

(provide 'init)
;;; init ends here
