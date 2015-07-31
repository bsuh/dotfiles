(eval-when-compile
  (if (and (= emacs-major-version 24) (= emacs-minor-version 4))
      (require 'cl)))

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(add-hook
 'after-init-hook
 (lambda ()
   ;; custom mode line
   (powerline-vim-theme)

   ;; window movement shortcuts
   (global-set-key (kbd "C-h")  'windmove-left)
   (global-set-key (kbd "C-l") 'windmove-right)
   (global-set-key (kbd "C-k")    'windmove-up)
   (global-set-key (kbd "C-j")  'windmove-down)

   ;; neotree
   (global-set-key [f8] 'neotree-toggle)

   ;; smex
   (smex-initialize)
   (global-set-key (kbd "M-x") 'smex)

   ;; auto completion
   (define-key company-active-map "\C-n" 'company-select-next)
   (define-key company-active-map "\C-p" 'company-select-previous)

   ;; vim
   (evil-mode 1)
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

   ;; higlight current symbol
   (add-hook 'prog-mode-hook
             (lambda ()
               (highlight-symbol-mode)
               (highlight-symbol-nav-mode)))

   ;; html
   (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (eval-after-load 'flycheck
     '(put 'html-tidy 'flycheck-modes (append '(web-mode) (get 'html-tidy 'flycheck-modes))))

   ;; javascript
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
   (eval-after-load 'company
     '(add-to-list 'company-backends 'company-omnisharp))
   (add-hook 'csharp-mode-hook
             (lambda ()
               (setq-local c-basic-offset 4)
               (c-set-style "c#")
               (omnisharp-mode)))

   ;; shell path for gui
   (when (memq window-system '(mac ns))
     (exec-path-from-shell-initialize))

   ;; magit
   (add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
   (add-to-list 'evil-emacs-state-modes 'magit-popup-sequence-mode)
   (add-to-list 'evil-emacs-state-modes 'magit-revision-mode)
   (add-to-list 'evil-emacs-state-modes 'magit-log-select-mode)
   (add-to-list 'evil-emacs-state-modes 'magit-cherry-mode)
   (add-to-list 'evil-emacs-state-modes 'magit-stashes-mode)
   (add-to-list 'evil-emacs-state-modes 'magit-refs-mode)
   (defalias 'magit-file-log 'magit-log-buffer-file)
   ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-list-file-prefix "~/.saves.auto-saves-")
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(c-basic-offset 2)
 '(c-default-style "linux")
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("7c89d1df5a1dd624983f6d107aced89a4b3d787b20997e5c6cff30cc1ba1b55d" default)))
 '(evil-want-C-u-scroll t)
 '(flx-ido-mode t)
 '(global-company-mode t)
 '(global-evil-surround-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(golden-ratio-exclude-modes (quote ("neotree-mode")))
 '(golden-ratio-mode t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(mac-option-modifier (quote meta))
 '(magit-use-overlays nil)
 '(markdown-command "grip --export -")
 '(menu-bar-mode nil)
 '(omnisharp-server-executable-path "~/mybins/OmniSharpServer/OmniSharp.exe")
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-habit-show-habits-only-for-today nil)
 '(org-log-done (quote time))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(sml/line-number-format "%l")
 '(sml/mode-width 80)
 '(sml/name-width 42)
 '(sml/theme (quote dark))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(web-mode-code-indent-offset 4)
 '(web-mode-css-indent-offset 4)
 '(web-mode-markup-indent-offset 2)
 '(xclip-mode t)
 '(xclip-use-pbcopy&paste t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 140 :width normal :family "Source Code Pro")))))
