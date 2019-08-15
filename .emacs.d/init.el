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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)
(unless (file-exists-p package-user-dir)
  (progn
    (package-refresh-contents)
    (package-install-selected-packages)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
  (make-frame))

(if (eq system-type 'darwin)
    (progn
      (setq mac-frame-tabbing t)
      (global-set-key (kbd "M-}") 'mac-next-tab)
      (global-set-key (kbd "M-{") 'mac-previous-tab)
      (global-set-key (kbd "M-t") 'mac-new-tab)
      (eval-after-load 'evil
        '(progn
           (global-set-key (kbd "M-w") 'evil-quit)
           (evil-set-initial-state 'magit-submodule-list-mode 'emacs)))
      (eval-after-load 'org
        '(progn
           (define-key org-mode-map (kbd "M-}") nil)
           (define-key org-mode-map (kbd "M-{") nil)))))
(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "M-}") nil)
     (define-key nxml-mode-map (kbd "M-{") nil)))

;; vim
(setenv "FZF_DEFAULT_COMMAND" "ag --hidden --ignore .git -g \"\"")
(eval-after-load 'evil
  '(progn
     (define-key evil-ex-map "b " 'helm-mini) ; instant switch buffer shortcut
     (define-key evil-ex-map "bd " 'ido-kill-buffer) ; instant kill buffer shortcut
     (define-key evil-ex-map "e " 'fzf-git) ; better open file
     (define-key evil-normal-state-map (kbd "SPC") 'smex) ; quicker M-x access
     (define-key evil-normal-state-map "'" 'ace-jump-mode)
     (define-key evil-motion-state-map "\t" nil)
     ))
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
