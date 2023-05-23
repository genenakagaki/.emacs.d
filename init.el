(cd user-emacs-directory)

;; Prevent startup message from showing up
(setq inhibit-startup-message t)

;; Turn off some features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; vim emulation
(use-package evil
  :ensure goto-chg
  :demand t
  :init
  (setq evil-want-integration t)
  ;; This needs to be nil in order for 'evil-collection' to work
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Setup keybinding configuration tool
(use-package general
  :after evil
  :demand t
  :config
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
    "M-v" 'evil-paste-after))

;; Enables number increment and decrements
(use-package evil-numbers
  :after general
  :demand t
  :general
  (general-def '(n v)
    "C-a" 'evil-numbers/inc-at-pt
    "C-x" 'evil-numbers/dec-at-pt))

;; Enables search of highlighted word in visual mode with * key
(use-package evil-visualstar
  :after evil
  :demand t
  :config
  (global-evil-visualstar-mode))

;; Enables easier surrounding with vim
(use-package evil-surround
  :after evil
  :demand t
  :config (global-evil-surround-mode 1))

(use-package avy
  :after evil
  :general
  (general-def '(n m)
    "s" 'avy-goto-char-2))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package hydra
  :demand t)

;;; Appearance
(use-package doom-themes
  :demand t
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

(general-def 'n 
  "M-o" 'find-file
  "M-e" 'switch-to-buffer
  "M-s" 'save-buffer
  "M-w" 'kill-current-buffer
  "M-q" 'save-buffers-kill-terminal)

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
  :demand t
  :config
  (global-company-mode t))

(use-package yasnippet
  :demand t
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

;; Highlight the matching parenthesis
(show-paren-mode t)

;; Color the brackets 
(use-package rainbow-delimiters
  :ghook ('prog-mode-hook #'rainbow-delimiters-mode))

;; Adds easier shortcut for editing Lisp. 
(use-package paredit
  :ghook ('(prog-mode-hook) #'enable-paredit-mode)
  :general
  (general-def 'i paredit-mode-map
    ;; Add matching closing parenthesis.
    "(" 'paredit-open-round
    "[" 'paredit-open-square
    "{" 'paredit-open-curly
    "<" 'paredit-open-angle)
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

(use-package magit
  :general
  (general-def 'n magit-status-mode-map
    ;; Magit binds the M-w to another command, so change it back to my keybinding
    "M-w" 'kill-current-buffer)
  (general-def '(n i) with-editor-mode-map
    ;; Make the M-w similar to the "close" behavior, but 'kill-current-buffer' breaks the magit process, so adjust for it 
    "M-w" 'with-editor-cancel
    "M-RET" 'with-editor-finish)
  (general-def 'n 'override
    :prefix gn/leader-key
    "og" 'magit-status)
  :config
  (setq magit-refresh-status-buffer nil))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun gn/resolve-org-yasnippet-conflict ()
  "This functions resolves the conflict between Org mode and yasnippet.
This functions should be added to the 'org-mode-hook'."
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))

;;;###autoload
(defun gn-org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.
If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

(defun gn/org-fold-lines ()
  ;; This needs to be nil on order for 'toggle-truncate-lines' to work.
  (setq truncate-partial-width-windows nil)

  ;; Fold long lines.
  ;; This variable is buffer local, so it needs to be set for every buffer
  (setq truncate-lines nil))

(use-package org
  :ensure org-contrib
  :gfhook 
  #'gn/fold-lines
  #'gn/search-only-visible-text
  :config
  ;; Adjust indent to heading.
  (setq org-startup-indented t)

  ;; Disable flycheck for emacs literate configuration

  (general-add-hook 'org-src-mode-hook
                    '(gn/disable-emacs-lisp-flycheck))

  ;; Hydra for headline navigation and modification
  (defhydra gn/hydra-org-headline (:color pink :hint nil)
    "
| Navigation^^           | TODO^^           |
|------------------------+------------------|
| _j_: next headline     | _t_: toggle TODO |
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
    ("t" org-todo)

    ;; Quit
    ("q" nil "quit")
    ("<escape>" nil "quit")
    ))

(use-package evil-org
  :after evil org
  :ghook 'org-mode
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

(use-package org-roam
  :after org

  :config
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-dailies-directory "journal")
  (setq org-roam-db-location (concat org-roam-directory "/org-roam.db"))
  (org-roam-db-autosync-mode))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam ;; or :after org
  :demand t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(general-def 'n org-mode-map
  ;; General org-mode usage
  "RET" 'gn-org/dwim-at-point
  "M-h" 'org-metaleft
  "M-H" 'org-shiftmetaleft
  "M-l" 'org-metaright
  "M-L" 'org-shiftmetaright

  ;; insert
  :prefix gn/leader-key
  "i" '(:ignore t :which-key "insert")
  "in" 'org-roam-node-insert
  "ii" 'org-id-store-link
  "il" 'org-insert-link

  )

(general-def 'n org-mode-map
  :prefix gn/leader-key
  "TAB" '(:ignore t :which-key "Toggle")
  "TAB TAB" 'gn/hydra-org-headline/body
  "TAB l" 'org-toggle-link-display
  "TAB n" #'org-narrow-to-subtree
  "TAB w" #'widen)

;; Source mode map
(general-def 'n org-src-mode-map
  "M-o" 'find-file
  "M-e" 'switch-to-buffer
  "M-s" 'save-buffer
  "M-w" 'org-edit-src-abort
  "M-q" 'save-buffers-kill-terminal)

(use-package plantuml-mode)
