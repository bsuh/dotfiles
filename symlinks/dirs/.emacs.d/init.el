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

;; package management
(defvar package-list
  '(
    ace-jump-mode
    ag
    company
    company-tern
    csharp-mode
    evil
    evil-surround
    flx-ido
    flycheck
    go-mode
    golden-ratio
    helm-projectile
    highlight-symbol
    js2-mode
    magit
    markdown-mode
    neotree
    omnisharp
    org-plus-contrib
    projectile
    rainbow-mode
    skewer-mode
    smart-mode-line
    smex
    tern
    use-package
    web-mode
    xclip
    zenburn-theme
    ))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; don't wrap lines
(setq-default truncate-lines t)

;; customize native UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)

;; neotree
(global-set-key [f8] 'neotree-toggle)

;; ido & smex
(setq ido-everywhere t)
(setq ido-use-faces nil)
(ido-mode t)
(flx-ido-mode t)
(global-set-key (kbd "M-x") 'smex)

;; vim
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(global-evil-surround-mode t)
(define-key evil-ex-map "b " 'helm-mini) ; better switch buffer
(define-key evil-ex-map "bd " 'ido-kill-buffer) ; better kill buffer
(define-key evil-ex-map "e " ; better open file using projectile or ido
  (lambda (arg)
    (interactive "P")
    (require 'projectile)
    (if (projectile-project-p)
        (helm-projectile)
      (helm-find-files arg))))
(define-key evil-normal-state-map (kbd "SPC") 'smex) ; quicker M-x access
(define-key evil-normal-state-map "'" 'ace-jump-mode)

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
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))
(add-hook 'js-mode-hook
          (lambda ()
            (js2-minor-mode)
            (tern-mode)
            (skewer-mode)
            (flycheck-mode)))

;; css
(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode)
            (skewer-css-mode)))

;; c#
(setq omnisharp-server-executable-path "~/mybins/OmniSharpServer/OmniSharp.exe")
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (c-set-style "c#")
            (omnisharp-mode)))

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
(setq sml/theme (quote dark))
(setq custom-safe-themes '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
(require 'smart-mode-line)
(sml/setup)

;; markdown
(setq markdown-command "grip --export -")

;; automatic window size adjust
(setq golden-ratio-exclude-modes '("neotree-mode"))
(require 'golden-ratio)
(golden-ratio-mode t)

;; terminal copy & paste
(require 'xclip)
(xclip-mode t)

(provide 'init)
;;; init.el ends here
