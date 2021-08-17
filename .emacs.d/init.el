;;; emacs.d --- adamrt's emacs config
;;; Code:
;;; Commentary:

;;; Packages
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(font . "Hack-18")))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Adjust garbage collection thresholds during startup, and thereafter
;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Disable UI
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; Paths
(setq exec-path (append exec-path '("/usr/local/go/bin")))
(setq exec-path (append exec-path '("~/go/bin")))
(setq exec-path (append exec-path '("~/.fzf/bin")))
(setq exec-path (append exec-path '("~/.local/bin")))

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
(set-fill-column 80)
(setq default-directory "~/")
(setq large-file-warning-threshold (* 1024 1000 100))
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq tab-width 4) ; or any other preferred value
(setq-default truncate-lines t)

;; Minor modes
(show-paren-mode 1)
(global-hl-line-mode)

;; gpg, ensure minibuffer input instead of fullscreen
;; (setenv "GPG_AGENT_INFO" nil)
(use-package pinentry)
(setq epg-gpg-program "gpg2")
(setq epg-pinentry-mode 'loopback)
(pinentry-start)

(use-package browse-kill-ring)
(use-package change-inner :bind (("M-i" . change-inner) ("M-o" . change-outer)))
(use-package diminish)
(use-package expand-region :bind ("M-m" . er/expand-region))
(use-package git-timemachine)
(use-package persistent-scratch :config (persistent-scratch-setup-default))
(use-package smartparens :diminish smartparens-mode :config (progn (require 'smartparens-config) (smartparens-global-mode 1) (show-paren-mode t)))
(use-package smex) ;; Smex enables recent ordering of counsel-M-x
(use-package syntax-subword :config (global-syntax-subword-mode) (setq syntax-subword-skip-spaces t))
(use-package undo-tree :diminish undo-tree-mode :init (global-undo-tree-mode))
(use-package which-key :init (progn (which-key-mode)) :diminish which-key-mode)

(use-package yaml-mode)
(use-package nginx-mode :init (progn (add-to-list 'auto-mode-alist '("nginx" . nginx-mode))))
(use-package dockerfile-mode)
(use-package php-mode)
(use-package terraform-mode :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
(use-package blacken
  :init
  (progn
    (add-hook 'python-mode-hook '(lambda() (set-fill-column 88)))
    (add-hook 'python-mode-hook 'blacken-mode)))

;; This keycord overlaps ag
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (setq projectile-project-search-path '("~/src/"))
;;   (projectile-mode +1))

(use-package go-mode
  :init
  (progn
    (setq gofmt-command "/home/adam/go/bin/goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook '(lambda() (set-fill-column 80)))
    (setq indent-tabs-mode 1)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-python-flake8-executable "python3.9"
        flycheck-disabled-checkers '(python-mypy)))

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

(use-package winnow) ;; filtering ag results buffer

(use-package magit
  :bind (("C-x g" . magit-status))
  :init (delete 'Git vc-handled-backends))

(use-package rcirc :init (progn (setq rcirc-default-nick "adamrt" rcirc-server-alist '(("irc.freenode.net" :port 6697 :encryption tls :channels ("#openbsd"))))))
(use-package sane-term :bind (("C-x t" . sane-term) ("C-x T" . sane-term-create)))
;; Searching withing macros is warped
;; (use-package swiper :after ivy :bind (("C-s" . swiper) ("C-r" . swiper)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face empty trailing lines-tail)))

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
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                nxml-mode-hook
                crontab-mode-hook
                perl-mode-hook
                javascript-mode-hook
                nginx-mode-hook
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

(eval-when-compile (require 'cl))
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
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
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

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c p") 'ag-project)
(global-set-key (kbd "C-x p") 'ag)
(global-set-key (kbd "C-c P") 'ag-project-regexp)
(global-set-key (kbd "C-c n") 'cleanup-buffer-or-region)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c g") 'dumb-jump-go)
(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#b6b6b6" :background "#2e2e2e"))))
 '(font-lock-doc-face ((t (:foreground "color-67"))))
 '(font-lock-string-face ((t (:foreground "#868686"))))
 '(whitespace-line ((t (:background "gray20" :foreground "brightblack")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("532a85b472fe3fe4b5791f8d06727066b2678f404a63fb0d51c6360d88f8781e" default)))


(use-package grayscale-theme)
(load-theme 'grayscale)
