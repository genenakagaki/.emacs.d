(cd user-emacs-directory)

;; Setup user
(setq user-full-name "Gene Nakagaki"
      user-mail-address "gene.nakagaki@gmail.com")

;; Prevent startup message from showing up
(setq inhibit-startup-message t)

;; Turn off some features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq confirm-kill-emacs 'yes-or-no-p)

;;; Setup how backup behaves.

;; Write backups in a specified folder
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, event when they're in version control
(setq vc-make-backup-files t)

;;; Setup for collaboration.
;;; Don't write lock-files. These are for when multiple users use the same file at once
; Don't write lock-files. These are for when multiple users use the same file at once
(setq create-lockfiles nil)

;;; Setup for custom settings.

;; Keep emacs custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (copy-file (expand-file-name "sample/custom.el" user-emacs-directory) custom-file))
(load custom-file)

;;; Setup package manager with 'package.el'.
(require 'package)

;; Add package repos
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;;; Setup package configuration with 'use-package'.
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'use-package)

;; logs stuff that takes long, and handles package configuration errors.
;;
;; logs stuff that takes long in the `*Messages*' buffer in the following conditions:
;; - package takes longer than 0.1s to load
;; - `:config' blocks takes longer than 0.1s to execute
;;
;; handles package configuration errors by:
;; - Emacs will not stop loading on error
;; - errors will be reported in a "*Warnings* popup buffer
(setq use-package-verbose t)

;; Install packages automatically if not already present on the system.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always load package 
(setq use-package-always-demand t)

;; prevent appending "-hook" on `:hook' keyword
(setq use-package-hook-name-suffix nil)

;; show how many packages were loaded, what stage of initialization they've reached, and how much aggregate time they've spent (roughly)
(setq use-package-compute-statistics t)

(use-package use-package-ensure-system-package)

;; Is using MacOS?
(defun gn/macos-p ()
  (equal system-type 'darwin))

(when (and (gn/macos-p) (display-graphic-p))
  ;; Make environment variables from the user's shell available
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

  ;; Set the key specific to MacOS
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option))

;; This works for copying, but not pasting for some reason

(setq select-enable-clipboard t)

;; Override the paste function to use MacOS paste function
;; (when (gn/macos-p)
;;   (setq interprogram-paste-function
;;         (lambda ()
;;           (shell-command-to-string "pbpaste"))))

(setq debug-on-error t)

;; vim emulation
(use-package evil
  :after goto-chg
  :init
  (setq
   evil-want-integration t
   ;; This needs to be nil in order for 'evil-collection' to work
   evil-want-keybinding nil
   ;; Setup undo system 
   evil-undo-system 'undo-redo
   )
  :config
  (evil-mode 1))

(use-package goto-chg)

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Setup keybinding configuration tool
(use-package general
  :after evil)

;; Enables number increment and decrements
(use-package evil-numbers
  :after (evil general))

;; Enables search of highlighted word in visual mode with * key
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

;; Enables easier surrounding with vim
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package avy
  :after evil)

(use-package which-key
  :config
  (which-key-mode))

(use-package hydra)

(defvar gn/leader-key "SPC")

(general-def 'n 'override
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(general-def '(n i)
  ;; Make similar experience with MacOS
  "M-a" 'mark-whole-buffer)

(general-def 'i 'override
  ;; Copy
  "M-c" 'evil-yank
  ;; Paste 
  "M-v" 'evil-paste-after)

(general-def '(n v)
  "C-a" 'evil-numbers/inc-at-pt
  "C-x" 'evil-numbers/dec-at-pt)

(general-def '(n m)
  "s" 'avy-goto-char-2)

;;; Appearance
(use-package doom-themes
  :config
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t 

   ;; make comments brighter
   doom-one-brighter-comments t
   doom-one-comment-bg nil)

  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(general-def '(n i)
  "M-w" 'kill-current-buffer
  "M-q" 'save-buffers-kill-terminal)

(general-def '(n i) 'override
  "M-o" 'find-file
  "M-e" 'switch-to-buffer
  "M-s" 'save-buffer)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(defun gn/search-only-visible-text ()
  (setq-local search-invisible nil))

;;; Setup text insepctions
(use-package flycheck
  :config
  (global-flycheck-mode)

  ;; Use the load-path of the current Emacs session for syntax checking
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun gn/disable-emacs-lisp-flycheck ()
  (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

;; Enable Vertico
(use-package vertico
  :config
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (savehist-mode))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Provides an orderless completion style
(use-package orderless
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (general-def 'n vertico-map
    "?" #'minibuffer-completion-help
    "M-RET" #'minibuffer-force-complete-and-exit
    "M-TAB" #'minibuffer-complete)
  )

;; Provides helpful annotations for completion candidates in the minibuffer
(use-package marginalia
  :config
  (marginalia-mode))

(use-package company
  :config
  (global-company-mode t))

(use-package yasnippet
  :ensure yasnippet-snippets
  :general
  (general-def 'i 
    "TAB" 'yas-insert-snippet)
  :config
  (yas-global-mode 1))

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(defvar gn/preview-file (expand-file-name "emacs-preview/src/preview-content.html"
                                          user-emacs-directory))

(defun gn/preview-image (image-url)
  "Preview IMAGE-URL image."
  (with-temp-file gn/preview-file
    (progn (insert "<img src=\"" image-url "\"/>"))))

;; Highlight the matching parenthesis
(show-paren-mode t)

;; Color the brackets 
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook)

;; Adds easier shortcut for editing Lisp. 
(use-package paredit
  :ghook ('(prog-mode-hook) #'enable-paredit-mode)
  :general
  (general-def 'i paredit-mode-map
    ;; Add matching closing parenthesis.
    "(" 'paredit-open-round
    "[" 'paredit-open-square
    "{" 'paredit-open-curly
    "<" 'paredit-open-angled)
  (general-def 'n paredit-mode-map
    :prefix gn/leader-key
    "dw" #'paredit-splice-sexp
    "s" #'paredit-forward-slurp-sexp
    "S" #'paredit-backward-slurp-sexp
    "b" #'paredit-forward-barf-sexp
    "B" #'paredit-backward-barf-sexp
    "gl" #'paredit-forward
    "gh" #'paredit-backward
    "gj" #'paredit-forward-down
    "gk" #'paredit-backward-up)
  :config 
  :diminish nil)

(general-def '(n v) emacs-lisp-mode-map
  "M-/" 'comment-dwim)

(general-def 'n emacs-lisp-mode-map
  "M-RET" 'eval-defun)

(general-def 'v emacs-lisp-mode-map
  "M-RET" 'eval-region)

(use-package cider
  :ghook
  'clojure-mode-hook
  'clojurescript-mode-hook)

(general-def 'n clojure-mode-map
  "M-RET" 'cider-eval-last-sexp)

(general-def '(n i) clojure-mode-map
  "M-RET" 'cider-eval-defun-at-point)

(setq js-indent-level 2)

(use-package magit)

(general-def 'n magit-status-mode-map
  ;; Magit binds the M-w to another command, so change it back to my keybinding
  "M-w" 'kill-current-buffer)

(general-def '(n i) with-editor-mode-map
  ;; Make the M-w similar to the "close" behavior, but 'kill-current-buffer' breaks the magit process, so adjust for it 
  "M-w" 'with-editor-cancel
  "M-RET" 'with-editor-finish)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun gn/resolve-org-yasnippet-conflict ()
  "This functions resolves the conflict between Org mode and yasnippet.
This functions should be added to the 'org-mode-hook'."
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))

(defun gn/open-task-inbox ()
  "Opens the task inbox file. This is where you put all the tasks."
  (interactive)
  (find-file (concat org-roam-directory "/todo.org")))

(defun gn/org-dwim-at-point ()
  (interactive)
  (message "gn/org-dwim-at-point")
  (let* ((element (org-element-at-point))
         (context (org-element-context)))
    (message "org-element-at-point")
    (pp element)
    (message "org-element-context")
    (pp context)
    (message "org-element-contents")
    (pp (org-element-contents element))
    (message "org-element-type")
    (pp (org-element-type element))

    (pcase (-first-item element)
      ('paragraph (let* ((parent (org-element-property :parent paragraph)))
                    (message "at paragraph")
                    )))))

(defun gn/org-fold-lines ()
  ;; This needs to be nil on order for 'toggle-truncate-lines' to work.
  (setq truncate-partial-width-windows nil)

  ;; Fold long lines.
  ;; This variable is buffer local, so it needs to be set for every buffer
  (setq truncate-lines nil))

(use-package org
  :gfhook 
  #'gn/org-fold-lines
  #'gn/search-only-visible-text
  :config
  (setq
   ;; Adjust indent to heading.
   org-startup-indented t

   ;; Set org-roam directory
   org-directory "~/org-roam/"

   ;; Open src window in current window
   org-src-window-setup "current-window"

   ;; Add the org todo state changes and timestamps into the property
   org-log-into-drawer "LOGBOOK"

   ;; Add information to property when todo state changed to DONE
   org-log-done 'time

   ;; Remove clock times that are less than a minute
   org-clock-out-remove-zero-time-clocks t
   )

  ;; Disable flycheck for emacs literate configuration
  (general-add-hook 'org-src-mode-hook
                    '(gn/disable-emacs-lisp-flycheck))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING" "IN-REVIEW" "|" "DONE" "CANCELLED(c)")
          (sequence "WAITING(w!)" "|" "DONE")
          (sequence "DELEGATED(d)" "|" "DONE")
          (sequence "|" "CANCELLED")
          ))
  (setq org-todo-keyword-faces
        '(("TODO" . "#f1d1a2")
          ("WAITING" . "#da8548")
          ("DELEGATED" . "#da8548")
          ("IN-REVIEW" . "#da8548")
          ))

  (require 'org-clock)

  (general-add-hook 'org-after-todo-state-change-hook
                    (lambda ()
                      (let* ((todo-clocking? (and (org-clocking-p)
                                                  (< (point) org-clock-marker)
                                                  (> (org-with-wide-buffer (org-entry-end-position))
                                                     org-clock-marker))))
                        (if (s-equals? org-state "DOING")
                            (org-clock-in)
                          (when todo-clocking?
                            (org-clock-out))))))

  )

(use-package evil-org
  :after evil org
  :ghook 'org-mode-hook
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-db-location (concat org-roam-directory "/org-roam.db"))
  (setq org-roam-dailies-directory "journal")
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template "${gn-node-display}")

  (setq org-roam-capture-templates
        '(("d" "default"
            plain "%?"
            :target (file+head "./node/%<%Y%m%d%H%M%S>.org"
                               "
#+language: en
#+title: ${title}

* {{{title}}}")
            :immediate-finish
            :jump-to-captured)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default"
           plain "*?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "
#+language: en
#+title: %<%Y-%m-%d>

* Daily routine

** Morning meditation

** Evening meditation

* Self monitoring record
")
           :immediate-finish
           :jump-to-captured))))


(cl-defmethod org-roam-node-gn-node-display ((node org-roam-node))
  "Method used to display the org-roam node in the minibuffer."
  (let ((title (org-roam-node-title node))
        (file-title (org-roam-node-file-title node)))
    (if (string= title file-title)
        title
      (concat file-title ": " title))))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam ;; or :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun gn/orgroam-force-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  (org-roam-db-clear-all)
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

(defhydra gn/hydra-org-headline (:color pink :hint nil)
  "
| Navigation^^           | TODO^^           |
|------------------------+------------------|
| _j_: next headline     |  |
| _k_: previous headline | ^^               |
| _h_: parent headline   | ^^               |
| ^^                     | ^^               |
| ^^                     | ^^               |
| ^^                     | ^^               |
"
  ;; Navigation
  ("j" org-next-visible-heading)
  ("k" org-previous-visible-heading)
  ("h" outline-up-heading)

  ;; Todo stuff
  ("J" org-shiftup)
  ("K" org-shiftdown)
  ("H" org-shiftleft)
  ("L" org-shiftright)

  ;; Quit
  ("q" nil "quit")
  ("<escape>" nil "quit"))

(general-def 'n org-mode-map
  ;; General org-mode usage
  "RET" 'org-ctrl-c-ctrl-c
  "M-h" 'org-metaleft
  "M-H" 'org-shiftmetaleft
  "M-l" 'org-metaright
  "M-L" 'org-shiftmetaright)

;; Source mode map
(general-def 'n org-src-mode-map
  "M-o" 'find-file
  "M-e" 'switch-to-buffer
  "M-s" 'save-buffer
  "M-w" 'org-edit-src-abort
  "M-q" 'save-buffers-kill-terminal)

(use-package request)

(defun gn/preview-plantuml-image (encoded-plantuml-code)
  (let* ((image-url (concat plantuml-server-url "/png/" encoded-plantuml-code)))
    (message image-url)
    (gn/preview-image image-url)))

(defun gn/plantuml-preview ()
  "Encodes PLANTUML-CODE to a string that can be used to generate PlantUML diagrams."
  (interactive)
  (when (eq major-mode 'plantuml-mode)
    (let* ((plantuml-code (buffer-string))
           (request-url (concat plantuml-server-url "/coder")))
      (request request-url
        :type "POST"
        :data plantuml-code
        :headers '(("Content-Type" . "text/plain"))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (gn/preview-plantuml-image data))))
      )))

(use-package plantuml-mode
  :gfhook
  #'gn/plantuml-preview-on-save
  :config
  (setq plantuml-server-url "http://localhost:4700")
  (setq plantuml-exec-mode 'server)
  (setq plantuml-indent-level 4)

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (general-add-hook 'after-save-hook
                    'gn/plantuml-preview))

(general-def '(n i) plantuml-mode-map
  "M-RET" 'gn/plantuml-preview)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.yml\\'" . yaml-mode)))

(use-package docker)

(setq dired-dwim-target t)

(general-def 'n 'override
  :prefix gn/leader-key

  "o" '(:ignore t :wk "Open")
  "og" 'magit-list-repositories
  "on" '(org-roam-node-find :wk "Org roam node")
  "or" '(org-roam-graph :wk "Org roam graph")
  "ot" '(gn/open-task-inbox :wk "Task inbox")

  "i" '(:ignore t :wk "Insert")
  "is" '(yas-insert-snippet :wk "Insert snippet")

  "TAB" '(:ignore t :wk "Toggle")

  ";" '(pp-eval-expression :wk "Eval expression")
  )

(general-def 'n org-mode-map
  :prefix gn/leader-key
  ;; Insert
  "in" '(org-roam-node-insert :wk "Org roam node")
  "ii" '(org-id-store-link :wk "Insert ID")
  "il" '(org-insert-link :wk "Insert link")

  ;; Toggle
  "TAB TAB" 'gn/hydra-org-headline/body
  "TAB l" 'org-toggle-link-display
  "TAB n" #'org-narrow-to-subtree
  "TAB w" #'widen)

(general-def 'n paredit-mode-map
  :prefix gn/leader-key
  "s" #'paredit-forward-slurp-sexp
  "S" #'paredit-backward-slurp-sexp
  "b" #'paredit-forward-barf-sexp
  "B" #'paredit-backward-barf-sexp)
