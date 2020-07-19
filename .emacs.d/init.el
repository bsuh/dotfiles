;;; package --- Summary

;;; Commentary:
;;; My initialization file

;;; Code:

;; Replace some variables temporarily during startup for faster startup
(defvar gc-cons-threshold-backup gc-cons-threshold)
(defvar file-name-handler-alist-backup file-name-handler-alist)
(setq gc-cons-threshold (* 100 1024 1024))
(setq file-name-handler-alist nil)
(run-with-idle-timer 5 nil (lambda ()
                             (setq gc-cons-threshold gc-cons-threshold-backup)
                             (setq file-name-handler-alist file-name-handler-alist-backup)))

;; straight package manager
(setq straight-check-for-modifications '(find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'ace-jump-mode)
(straight-use-package 'ag)
(straight-use-package 'alchemist)
(straight-use-package 'cmake-mode)
(straight-use-package 'company)
(straight-use-package 'csharp-mode)
(straight-use-package 'deft)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'elixir-mode)
(straight-use-package 'erlang)
(straight-use-package 'evil)
(straight-use-package 'evil-surround)
(straight-use-package 'flx-ido)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-credo)
(straight-use-package 'flycheck-flow)
(straight-use-package 'fsharp-mode)
(straight-use-package 'fzf)
(straight-use-package 'gcmh)
(straight-use-package 'go-mode)
(straight-use-package 'groovy-mode)
(straight-use-package 'helm)
(straight-use-package 'highlight-symbol)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'omnisharp)
(straight-use-package 'org-journal)
(straight-use-package 'org-plus-contrib)
(straight-use-package 'protobuf-mode)
(straight-use-package 'rainbow-mode)
(straight-use-package 'rg)
(straight-use-package 'smex)
(straight-use-package 'tide)
(straight-use-package 'web-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'zenburn-theme)

;; for melpa package exploration
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; delay garbage collection during usage
(require 'gcmh)
(gcmh-mode 1)

(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
            ))

;; extended command interface
(global-set-key (kbd "M-x") 'smex)
(eval-after-load 'evil
  '(define-key evil-normal-state-map (kbd "SPC") 'smex))

;; gui tabs
(defun mac-new-tab ()
  (interactive)
  (make-frame))
(if (eq system-type 'darwin)
    (progn
      (setq mac-frame-tabbing t)
      (global-set-key (kbd "M-}") 'mac-next-tab)
      (global-set-key (kbd "M-{") 'mac-previous-tab)
      (global-set-key (kbd "M-t") 'mac-new-tab)
      (global-set-key (kbd "M-w") 'evil-quit)
      (eval-after-load 'org
        '(progn
           (define-key org-mode-map (kbd "M-}") nil)
           (define-key org-mode-map (kbd "M-{") nil)))
      (eval-after-load 'nxml-mode
        '(progn
           (define-key nxml-mode-map (kbd "M-}") nil)
           (define-key nxml-mode-map (kbd "M-{") nil)))))

;; fzf file opener
(setenv "FZF_DEFAULT_COMMAND" "ag --hidden --ignore .git -g \"\"")
(eval-after-load 'evil
  '(define-key evil-ex-map "e " 'fzf-git))

;; buffer switcher
(eval-after-load 'evil
  '(define-key evil-ex-map "b " 'helm-mini))

;; buffer kill
(eval-after-load 'evil
  '(define-key evil-ex-map "bd " 'kill-buffer))

;; ace jump
(eval-after-load 'evil
  '(define-key evil-normal-state-map "'" 'ace-jump-mode))

;; evil mode enable/disable
(eval-after-load 'evil
  '(progn
     (define-key evil-motion-state-map "\t" nil)
     (evil-set-initial-state 'magit-submodule-list-mode 'emacs)
     (evil-set-initial-state 'deft-mode 'emacs)
     (evil-set-initial-state 'git-commit-mode 'normal)
     ))

;; auto completion
(eval-after-load 'company
  '(progn
     (define-key company-active-map "\C-n" 'company-select-next)
     (define-key company-active-map "\C-p" 'company-select-previous)))

;; higlight current symbol
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-symbol-mode)
            (highlight-symbol-nav-mode)))

;; html
(add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(eval-after-load 'flycheck
  '(flycheck-add-mode 'html-tidy 'web-mode))

;; javascript && jsx
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              (tide-setup)
              (eldoc-mode)
              (flycheck-mode)
              (flycheck-select-checker 'javascript-eslint)
              )))
(eval-after-load 'flycheck
  '(flycheck-add-mode 'javascript-eslint 'web-mode))

; https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; css
(add-hook 'css-mode-hook 'rainbow-mode)

;; c#
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (c-set-style "c#")
            (omnisharp-mode)))

;; f#
(add-hook 'fsharp-mode-hook
          (lambda ()
            (setq indent-line-function 'indent-relative)))

;; go
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)))

;; magit
(defalias 'magit-file-log 'magit-log-buffer-file)
(eval-after-load 'magit-status
  '(progn
     (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
     (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
     (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
     (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)))
(eval-after-load 'magit-refs
  '(progn
     (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
     (remove-hook 'magit-refs-sections-hook 'magit-insert-remote-branches)))
(eval-after-load 'magit-git
  '(delete "refs/tags" magit-list-refs-namespaces))

;; org
(eval-after-load 'org
  '(add-to-list 'org-modules 'org-habit))

; elixir
(eval-after-load 'flycheck
  '(flycheck-credo-setup))

(provide 'init)
;;; init.el ends here
