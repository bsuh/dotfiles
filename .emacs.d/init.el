;;; package --- Summary

;;; Commentary:
;;; My initialization file

;;; Code:

;; Replace some variables temporarily during startup for faster startup
(setq gc-cons-threshold-backup gc-cons-threshold)
(setq file-name-handler-alist-backup file-name-handler-alist)
(setq gc-cons-threshold (* 100 1024 1024))
(setq file-name-handler-alist nil)
(run-with-idle-timer 1 nil (lambda ()
                             (setq gc-cons-threshold gc-cons-threshold-backup)
                             (setq file-name-handler-alist file-name-handler-alist-backup)))

(eval-when-compile
  (if (and (= emacs-major-version 24)
           (or (= emacs-minor-version 4)
               (= emacs-minor-version 5)))
      (require 'cl)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; c++/c/objective-c completion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(eval-after-load 'flycheck
  '(add-to-list 'flycheck-mode-hook #'flycheck-irony-setup))

;; don't wrap lines
(setq-default truncate-lines t)

;; show trailing whitespace
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
            (setq-local show-trailing-whitespace t)))

;; customize native UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)

;; neotree
(global-set-key [f8] 'neotree-toggle)

;; smex
(setq ido-everywhere t)
(setq ido-use-faces nil)
(ido-mode t)
(flx-ido-mode t)
(global-set-key (kbd "M-x") 'smex)

;; vim
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(require 'dired)
(global-evil-surround-mode t)
(define-key evil-ex-map "b " 'helm-mini) ; instant switch buffer shortcut
(define-key evil-ex-map "bd " 'ido-kill-buffer) ; instant kill buffer shortcut
(setenv "FZF_DEFAULT_COMMAND" "ag --hidden --ignore .git -g \"\"")
(define-key evil-ex-map "e " 'fzf-git) ; better open file
(define-key evil-normal-state-map (kbd "SPC") 'smex) ; quicker M-x access
(define-key evil-normal-state-map "'" 'ace-jump-mode)
(add-hook 'git-commit-mode-hook 'evil-normal-state)

;; auto completion
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(require 'company)
(global-company-mode t)
(define-key company-active-map "\C-n" 'company-select-next)
(define-key company-active-map "\C-p" 'company-select-previous)

;; flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; parenthesis
(setq show-paren-delay 0)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; higlight current symbol
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-symbol-mode)
            (highlight-symbol-nav-mode)))

;; C-style languages
(setq c-basic-offset 2)
(setq c-default-style "linux")

;; html
(setq web-mode-code-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-markup-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(eval-after-load 'flycheck
  '(put 'html-tidy 'flycheck-modes (append '(web-mode) (get 'html-tidy 'flycheck-modes))))

;; javascript
(setq js-indent-level 2)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(add-hook 'js-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode)
            (eldoc-mode)))

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
(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode)
            (skewer-css-mode)))

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

;; org
(setq org-agenda-files '("~/org/todo.org"))
(setq org-habit-show-habits-only-for-today nil)
(setq org-log-done 'time)
(eval-after-load 'org
  '(add-to-list 'org-modules 'org-habit))

;; color theme
(load-theme 'zenburn t)

;; custom mode line
(setq sml/line-number-format "%l")
(setq sml/mode-width 80)
(setq sml/name-width 42)
(setq sml/theme (quote automatic))
(setq custom-safe-themes '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
(require 'smart-mode-line)
(sml/setup)

;; markdown
(setq markdown-command "grip --export -")

;; automatic window size adjust
(setq golden-ratio-exclude-modes '("neotree-mode"))
(require 'golden-ratio)
(golden-ratio-mode t)

; elixir
(eval-after-load 'flycheck
  '(flycheck-credo-setup))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fzf/args "-x --margin 1,0 --print-query")
 '(git-commit-fill-column 72)
 '(git-commit-summary-max-length 50)
 '(package-selected-packages
   (quote
    (org-journal fzf protobuf-mode yaml-mode alchemist elixir-mode flycheck-credo ace-jump-mode ag company company-irony csharp-mode evil evil-surround flx-ido flycheck flycheck-irony fsharp-mode go-mode golden-ratio helm highlight-symbol irony magit markdown-mode omnisharp org-plus-contrib projectile rainbow-mode smart-mode-line smex tide web-mode zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
