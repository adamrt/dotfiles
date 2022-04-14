;;; emacs.d --- adamrt's emacs config
;;; Code:
;;; Commentary:

;; Fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Disable UI
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))


;; Disable backups, autosaves and lock files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      backup-inhibited t
      auto-save-list-file-prefix nil)

;; Misc sanity
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t) ;; Auto refresh buffers
(set-default 'indent-tabs-mode nil) ;; Don't use tabs
(setq default-directory "~/"
      large-file-warning-threshold (* 1024 1000 100)
      inhibit-startup-message t
      line-number-mode t
      c-basic-offset 4)
;; Use setq-default when buffer local variables
(setq-default set-fill-column 80
              truncate-lines t)

(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

(global-hl-line-mode)
(yas-global-mode)

(use-package smex) ;; Smex enables recent ordering of counsel-M-x
(use-package winnow) ;; filtering ag results buffer
(use-package php-mode)
(use-package diminish)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package git-timemachine)
(use-package browse-kill-ring)
(use-package expand-region :bind ("M-m" . er/expand-region))
(use-package change-inner :bind (("M-i" . change-inner) ("M-o" . change-outer)))
(use-package persistent-scratch :config (persistent-scratch-setup-default))
(use-package syntax-subword :config (global-syntax-subword-mode) (setq syntax-subword-skip-spaces t))
(use-package which-key :init (progn (which-key-mode)) :diminish which-key-mode)
(use-package nginx-mode :init (progn (add-to-list 'auto-mode-alist '("nginx" . nginx-mode))))
(use-package rcirc :init (progn (setq rcirc-default-nick "adamrt" rcirc-server-alist '(("irc.freenode.net" :port 6697 :encryption tls :channels ("#openbsd"))))))

(use-package vterm
  :bind (("C-x t" . vterm)
         ("C-x T" . sane-term-create)))

;; (use-package sane-term
;;   :bind (("C-x t" . vterm)
;;          ("C-x T" . sane-term-create)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil))

;; gpg, ensure minibuffer input instead of fullscreen
(use-package pinentry
  :config
  (setq epg-gpg-program "gpg2")
  (setq epg-pinentry-mode 'loopback))
(pinentry-start)

;; Python formatter
(use-package blacken
  :init
  (progn
    (add-hook 'python-mode-hook '(lambda() (set-fill-column 88)))
    (add-hook 'python-mode-hook 'blacken-mode)))

;; Currently disabled as something conflict with lsp-mode and I don't
;; want to spend time to figure it out now.
;;
;; (use-package flycheck
;; :ensure t :init (global-flycheck-mode)) :config (setq
;; flycheck-disabled-checkers '(python-mypy c/c++-gcc)))

;; ..

(use-package lsp-mode
  :hook ((c-mode c++-mode python-mode go-mode web-mode js-mode rust-mode) . lsp-deferred)
  :commands lsp
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "--log=verbose" "--clang-tidy" "--enable-config" "--header-insertion-decorators=1" "--suggest-missing-includes=1" "--header-insertion=iwyu"))
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq clang-format-executable "/usr/bin/clang-format"))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'before-save-hook 'clang-format-buffer nil 'make-it-local)))


;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package lsp-ui
;;   :requires lsp-mode
;;   :config

;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-use-childframe t
;;         lsp-ui-doc-position 'top
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-sideline-enable nil
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-flycheck-list-position 'right
;;         lsp-ui-flycheck-live-reporting t
;;         lsp-ui-peek-enable t
;;         lsp-ui-peek-list-width 60
;;         lsp-ui-peek-peek-height 25)

;; (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-doc-header t)
;;   (setq lsp-ui-doc-include-signature t)
;;   (setq lsp-ui-doc-border (face-foreground 'default))
;;   (setq lsp-ui-sideline-show-code-actions t)
;;   (setq lsp-ui-sideline-delay 0.05))

(use-package go-mode
  :init
  (progn
    (setq gofmt-command "/home/adam/go/bin/goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook '(lambda() (set-fill-column 80)))
    (setq indent-tabs-mode 1)))


(use-package ag
  :config
  (add-hook 'ag-mode-hook 'toggle-truncate-lines)
  (add-hook 'ag-mode-hook 'winnow-mode)
  (setq ag-highlight-search nil ag-reuse-buffers 't))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package magit
  :bind (("C-x g" . magit-status))
  :init (delete 'Git vc-handled-backends))
(require 'magit-todos)
(magit-todos-mode)
;; Searching withing macros is warped
;; (use-package swiper :after ivy :bind (("C-s" . swiper) ("C-r" . swiper)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace-mode
  :config
  ;; (setq whitespace-line-column 80)
  (setq whitespace-style '(face empty trailing)))

(eval-after-load 'eldoc-mode
  '(if
       (fboundp 'diminish)
       (diminish 'eldoc-mode)))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(use-package prettier-js)
(setq prettier-js-args '("--print-width" "88"))

(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.vue?\\'" . prettier-js-mode)))))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 0 ;; inside <script>
        web-mode-enable-auto-indentation nil
        web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :diminish counsel-mode
  :bind (("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-x C-m" . counsel-M-x)
         ("C-c f"   . counsel-git)
         ("C-c /"   . counsel-ag)))

(use-package flx)
(use-package ivy
  :defer 0.1
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("C-s" . ivy-next-line) ;; defaults to annoying -or-history (might be default now)
         ("C-j" . ivy-immediate-done) ;; C-j opens dired at point
         ("C-m" . ivy-alt-done)) ;; Enter goes deeper in tree, not opening dired
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers "recentf"
        ivy-height 20
        ;; quiet
        ivy-display-style 'plain
        ivy-count-format "%d/%d "
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-extra-directories nil))

;; for prog modes turn on flyspell-prog-mode (checks spell only in comments)
(dolist (hook '(c-mode-hook
                c++-mode-hook
                crontab-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                go-mode-hook
                javascript-mode-hook
                lisp-mode-hook
                nginx-mode-hook
                nxml-mode-hook
                perl-mode-hook
                php-mode-hook
                python-mode-hook
                ruby-mode-hook
                shell-mode-hook
                yaml-mode
                LaTeX-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(setq org-directory "~/sync/org/"
      org-agenda-files (list org-directory)
      org-default-notes-file "~/sync/org/organizer.org"
      org-clock-persist 'history)

;; org-capture-templates
;; '(("w" "Work" entry (file+headline "work.org" "Tasks") "* TODO %?\n  %i\n %t\n %a")
;;   ("e" "Even" entry (file+headline "even.org" "Tasks") "* TODO %?\n  %i\n %t\n %a")
;;   ("p" "Personal" entry (file+headline "personal.org" "Tasks") "* TODO %?\n  %i\n %t\n %a")
;;   ("j" "Journal" entry (file+datetree "journal.org") "* %?\nEntered on %U\n  %i\n  %a")))
(org-clock-persistence-insinuate)
(define-key org-mode-map (kbd "C-j") (lambda () (interactive) (join-line -1)))

(use-package dash)
(setq dired-listing-switches "-alh")
(define-key dired-mode-map (kbd "-") 'dired-up-directory) ;; use - instead of ^ for dired-up-directory
(define-key dired-mode-map (kbd "k") 'dired-do-delete) ;; Kill files

;; omit useless files
(require 'dired-x)
(setq-default dired-omit-files-p t)

;; (eval-when-compile (require 'cl))
(define-minor-mode keep-buffers-mode
  "when active, killing protected buffers results in burying them instead.
Some may also be erased, which is undo-able."
  :init-value nil
  :global t
  :group 'keep-buffers
  :lighter ""
  :version "1.4"
  (if keep-buffers-mode
      ;; Setup the hook
      (add-hook 'kill-buffer-query-functions 'keep-buffers-query)
    (remove-hook 'kill-buffer-query-functions 'keep-buffers-query)))

(defcustom keep-buffers-protected-alist
  '(("\\`\\*scratch\\*\\'" . erase)
    ("\\`\\*Messages\\*\\'" . nil))
  "an alist '((\"regex1\" . 'erase) (\"regex2\" . nil))
CAR of each cons cell is the buffer matching regexp.  If CDR is
not nil then the matching buffer is erased then buried.
If the CDR is nil, then the buffer is only buried."
  :type '(alist)
  :group 'keep-buffers
  )


;;;###autoload
(defun keep-buffers-query ()
  "The query function that disable deletion of buffers we protect."
  (let ((crit (dolist (crit keep-buffers-protected-alist)
                (when (string-match (car crit) (buffer-name))
                  (return crit)))))
    (if crit (progn (when (cdr crit) (erase-buffer)) (bury-buffer) nil) t)))

(push '("\\`*scratch" . nil) keep-buffers-protected-alist)
(keep-buffers-mode 1)

(defun cleanup-buffer-or-region ()
  "Un-tabify, indent and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(autoload 'forward-to-word "misc" "Move forward until
    encountering the beginning of a word. With argument, do this that
    many times.")
(autoload 'dired-jump "dired")

;; config
(global-set-key (kbd "C-x C-l") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; window management
(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-c p") 'ag-project)
(global-set-key (kbd "C-x p") 'ag)
(global-set-key (kbd "C-c P") 'ag-project-regexp)

(global-set-key (kbd "C-c n") 'cleanup-buffer-or-region)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "7397cc72938446348521d8061d3f2e288165f65a2dbb6366bb666224de2629bb" "9f1d0627e756e58e0263fe3f00b16d8f7b2aca0882faacdc20ddd56a95acb7c2" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "0710b0bdd59c8a7aacf0640591b38fcad5978a0fcfff3fdd999e63499ada8e3e" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" default))
 '(package-selected-packages
   '(magit-todos dap-mode rust-mode csv-mode ac-capf company-box atom-one-dark-theme material-theme modus-themes solarized-theme ample-theme ample-zen-theme zenburn-theme yasnippet yaml-mode winnow which-key wgrep-ag web-mode vterm use-package undo-tree terraform-mode syntax-subword smex smartparens sane-term prettier-js pinentry php-mode persistent-scratch nordless-theme nord-theme nhexl-mode nginx-mode magit grayscale-theme google-c-style go-mode glsl-mode git-timemachine flycheck flx dumb-jump dockerfile-mode diminish counsel company clang-format change-inner browse-kill-ring blacken ag)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight normal :height 158 :width normal)))))
