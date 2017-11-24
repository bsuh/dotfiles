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
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; show trailing whitespace
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode t)
            (setq-local show-trailing-whitespace t)))

;; smex
(global-set-key (kbd "M-x") 'smex)

;; mac keybindings
(defun mac-new-tab ()
  (interactive)
  (let ((current-frame (selected-frame))
        (new-frame (make-frame)))
    (progn
      (mac-set-frame-tab-group-property
       current-frame :frames
       (append (mac-frame-tab-group-property current-frame :frames)
               (list new-frame)))
      (mac-set-frame-tab-group-property
       nil :selected-frame new-frame))))

(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "M-}") 'mac-next-tab)
      (global-set-key (kbd "M-{") 'mac-previous-tab)
      (global-set-key (kbd "M-t") 'mac-new-tab)
      (eval-after-load 'evil
        '(global-set-key (kbd "M-w") 'evil-quit))
      (eval-after-load 'org
        '(progn
           (define-key org-mode-map (kbd "M-}") nil)
           (define-key org-mode-map (kbd "M-{") nil)))))

;; vim
(setenv "FZF_DEFAULT_COMMAND" "ag --hidden --ignore .git -g \"\"")
(eval-after-load 'evil
  '(progn
     (define-key evil-ex-map "b " 'helm-mini) ; instant switch buffer shortcut
     (define-key evil-ex-map "bd " 'ido-kill-buffer) ; instant kill buffer shortcut
     (define-key evil-ex-map "e " 'fzf-git) ; better open file
     (define-key evil-normal-state-map (kbd "SPC") 'smex) ; quicker M-x access
     (define-key evil-normal-state-map "'" 'ace-jump-mode)))
(add-hook 'git-commit-mode-hook 'evil-normal-state)

;; auto completion
(eval-after-load 'company
  '(progn
     (define-key company-active-map "\C-n" 'company-select-next)
     (define-key company-active-map "\C-p" 'company-select-previous)))

;; parenthesis
(add-hook 'prog-mode-hook 'show-paren-mode)

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

;; org
(eval-after-load 'org
  '(add-to-list 'org-modules 'org-habit))

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
 '(c-basic-offset 2)
 '(c-default-style "linux")
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" default)))
 '(evil-mode t)
 '(evil-want-C-u-scroll t)
 '(flx-ido-mode t)
 '(fzf/args "-x --margin 1,0 --print-query")
 '(git-commit-fill-column 72)
 '(git-commit-summary-max-length 50)
 '(global-company-mode t)
 '(global-evil-surround-mode t)
 '(global-flycheck-mode t)
 '(golden-ratio-mode t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(markdown-command "grip --export -")
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-habit-show-habits-only-for-today nil)
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (rg org-journal fzf protobuf-mode yaml-mode alchemist elixir-mode flycheck-credo ace-jump-mode ag company company-irony csharp-mode evil evil-surround flx-ido flycheck flycheck-irony fsharp-mode go-mode golden-ratio helm highlight-symbol irony magit markdown-mode omnisharp org-plus-contrib projectile rainbow-mode smex tide web-mode zenburn-theme)))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "nil" :slant normal :weight normal :height 141 :width normal)))))
