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

;; Confirm before exiting emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Show line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Highlight current line
(global-hl-line-mode t)

(server-start)

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

(setq debug-on-error nil)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (message "indent done"))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(defun gn/sticky-window/toggle ()
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p (selected-window)))))

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
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))

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

(defun gn/open-config-file ()
  (interactive)
  (find-file (expand-file-name "init.org" user-emacs-directory)))

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun gn/delete-current-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

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

(use-package symbol-overlay
  :ghook 'prog-mode-hook)

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
  )

;; Provides helpful annotations for completion candidates in the minibuffer
(use-package marginalia
  :config
  (marginalia-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind 
  :config
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) 
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package yasnippet
  :ensure yasnippet-snippets
  :config
  (yas-global-mode 1))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix ".")

  :config
  (setq tempel-path (expand-file-name "templates.eld" user-emacs-directory))

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (general-add-hook '(conf-mode-hook prog-mode-hook text-mode-hook org-mode-hook)
                    'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(defvar gn/preview-file (expand-file-name "emacs-preview/src/emacs/preview/data.cljs"
                                          user-emacs-directory))

(defun gn/preview-image (image-url)
  "Preview IMAGE-URL image."
  (with-temp-file gn/preview-file
    (progn
      (insert "(ns emacs.preview.data)

(def image-data \"" image-url "\")

(def org-data nil)"))))

;; Highlight the matching parenthesis
(show-paren-mode t)

;; Color the brackets 
(use-package rainbow-delimiters
  :ghook 'prog-mode-hook)

(defun gn/paredit-add-space-for-delimiter-p (endp delimiter)
  nil)

;; Adds easier shortcut for editing Lisp. 
(use-package paredit
  :ghook ('(prog-mode-hook) #'enable-paredit-mode)
  :config
  (setq paredit-space-for-delimiter-predicates '(gn/paredit-add-space-for-delimiter-p))
  :diminish nil)

(defun gn/eval-region (start end)
  (interactive "r")
  (eval-region start end t))

(use-package parseedn)

(use-package cider
  :ghook
  'clojure-mode-hook
  'clojurescript-mode-hook)

(use-package clj-refactor)

(use-package flycheck-clj-kondo
  :config
  (require 'flycheck-clj-kondo))

(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

(use-package magit)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun gn/resolve-org-yasnippet-conflict ()
  "This functions resolves the conflict between Org mode and yasnippet.
This functions should be added to the 'org-mode-hook'."
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))

(defun gn/org-log-element-at-point ()
  (message "
Logging org elements at point...")
  (let* ((element (org-element-at-point))
         (context (org-element-context)))
    (message "
org-element-at-point")
    (pp element)
    (message "
org-element-context")
    (pp context)
    (message "
org-element-contents")
    (pp (org-element-contents element))
    (message "
org-element-type")
    (pp (org-element-type element))
    ))

(defun gn/org-dwim-at-point ()
  (interactive)

  (let* ((context (org-element-context))
         (element-type (org-element-type context))
         (debug-p nil))
    (when debug-p
      (message "

Running gn/org-dwim-at-point function...")
      (gn/org-log-element-at-point))
    (pcase element-type 
      ;; ('paragraph (let* ((parent (org-element-property :parent context)))
      ;;               (when (and (eq (org-element-type parent) 'item)
      ;;                          (org-element-property :checkbox parent))
      ;;                 ;; toggle checkbox if checkbox
      ;;                 (org-ctrl-c-ctrl-c))))
      ('src-block (org-edit-special))
      ('example-block (org-edit-special))
      ('link (org-open-at-point))

      (element (org-ctrl-c-ctrl-c))
      )
    (when debug-p
      (message "gn/org-dwim-at-point finished"))))

(pcase 'test
  ((and 'testaa)
   (message "hello"))
  )

(defun gn/org-fold-lines ()
  (turn-on-visual-line-mode)


  ;; This needs to be nil on order for 'toggle-truncate-lines' to work.
  ;; (setq truncate-partial-width-windows nil)

  ;; Fold long lines.
  ;; This variable is buffer local, so it needs to be set for every buffer
  ;; (setq truncate-lines nil)
  )

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

   ;; Set org agenda
   org-agenda-files `(,(concat org-directory "/todo.org"))

   ;; Open src window in current window
   org-src-window-setup 'current-window

   ;; Add the org todo state changes and timestamps into the property
   org-log-into-drawer "LOGBOOK"

   ;; Add information to property when todo state changed to DONE
   org-log-done 'time

   ;; Remove clock times that are less than a minute
   org-clock-out-remove-zero-time-clocks t


   ;; https://github.com/abo-abo/swiper/issues/986
   ;; Use the search interface instead of the default
   org-goto-interface 'outline-path-completion
   ;; This needs to be nil for incremental search
   org-outline-path-complete-in-steps nil
   )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  ;; Disable flycheck for emacs literate configuration
  (general-add-hook 'org-src-mode-hook
                    '(gn/disable-emacs-lisp-flycheck))


  (require 'org-clock)

  )

(use-package ob-async
  :config
  (require 'ob-async))

(use-package org-download
  :config

  ;; Download to a directory
  (setq org-download-method 'directory)
  ;; Don't include heading name in the download directory path
  (setq-default org-download-heading-lvl nil)
  ;; The directory to put downloaded images
  ;; Put images in the same directory by default
  (setq-default org-download-image-dir nil)
  ;; The timestamp appended to the filename
  (setq org-download-timestamp "%Y%m%d-%H%M%S-")

  ;; Drag-and-drop to `dired`
  (general-add-hook 'dired-mode-hook 'org-download-enable)
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

* Description")
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
           :jump-to-captured)))

  ;; emacs preview
  (load (expand-file-name "emacs-preview/src/ox-edn.el" user-emacs-directory))
  ;; (general-add-hook 'org-mode-hook 
  ;;                   (lambda ()
  ;;                     (general-add-hook 'after-save-hook 'gn/ox-export-as-edn)))
  )


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

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING" "IN-REVIEW" "|" "DONE")
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

(defun gn-org/on-todo-change ()
  (let* ((clocking-todo-state-changed-p
          (and (org-clocking-p)
               (< (point) org-clock-marker (org-with-wide-buffer (org-entry-end-position))))))
    (if (s-equals? org-state "DOING")
        (when (not (org-clocking-p))
          (org-clock-in))
      (when clocking-todo-state-changed-p
        (org-clock-out)))))

(general-add-hook
 'org-after-todo-state-change-hook
 #'gn-org/on-todo-change)

(defun gn/open-task-inbox ()
  "Opens the task inbox file. This is where you put all the tasks."
  (interactive)
  (find-file (concat org-roam-directory "/todo.org")))

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
  :config
  (setq plantuml-server-url "http://localhost:4700")
  (setq plantuml-exec-mode 'server)
  (setq plantuml-indent-level 4)

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (general-add-hook (list 'after-save-hook 'plantuml-mode-hook) 
                    'gn/plantuml-preview)


  )

(use-package know-your-http-well)


(use-package verb
  :mode ("\\.org\\'" . org-mode)
  )

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.yml\\'" . yaml-mode)))

(use-package docker)

(setq dired-dwim-target t)

(general-def '(n i v) 'override
  "M-z" 'evil-force-normal-state)

(defvar gn/leader-key "SPC")

(general-def 'n 'override
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "<down>" 'evil-next-visual-line
  "<up>" 'evil-previous-visual-line)

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

(general-def '(n i)
  "M-w" 'kill-current-buffer
  "M-q" 'save-buffers-kill-terminal)

(general-def '(n i) 'override
  "M-o" 'find-file
  "M-e" 'switch-to-buffer
  "M-s" 'save-buffer)

(general-def 'n 'override
  :prefix gn/leader-key

  "o" '(:ignore t :wk "Open")
  "og" 'magit-list-repositories
  "on" '(org-roam-node-find :wk "Org roam node")
  "or" '(org-roam-graph :wk "Org roam graph")
  "ot" '(gn/open-task-inbox :wk "Task inbox")
  "oc" '(gn/open-config-file :wk "Config file")

  "i" '(:ignore t :wk "Insert")
  "is" '(yas-insert-snippet :wk "Insert snippet")

  "r" '(:ignore t :wk "Run")

  "t" '(:ignore t :wk "Toggle")

  ";" '(pp-eval-expression :wk "Eval expression")
  )

(general-def 'n org-mode-map
  :prefix gn/leader-key
  ;; Insert
  "in" '(org-roam-node-insert :wk "Insert org-roam node")
  "ii" '(org-id-store-link :wk "Insert node ID")
  "iI" '(org-download-clipboard :wk "Insert clipboard screenshot")
  "il" '(org-insert-link :wk "Insert link")

  ;; Toggle
  "tt" 'gn/hydra-org-headline/body
  "tl" 'org-toggle-link-display
  "ti" 'org-toggle-inline-images
  "tn" #'org-narrow-to-subtree
  "tw" #'widen

  ;; Run
  "rr" '(verb-send-request-on-point-other-window :wk "Send request")
  )

(general-def 'n verb-response-body-mode-map
  "oh" '(verb-toggle-show-headers :wk "HTTP headers"))

(general-def 'n vertico-map
  "?" #'minibuffer-completion-help
  "M-RET" #'minibuffer-force-complete-and-exit
  "M-TAB" #'minibuffer-complete)

(general-def 'i 
  "C-n" #'completion-at-point)

;; (general-def '(n i) tempel-map
;;   "RET" #'tempel-next
;;   "S-RET" #'tempel-previous)

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

(general-def '(n v) emacs-lisp-mode-map
  "M-/" 'comment-dwim)

(general-def '(n i) emacs-lisp-mode-map
  "M-RET" 'eval-defun)

(general-def 'v emacs-lisp-mode-map
  "M-RET" 'gn/eval-region)

(general-def 'n clojure-mode-map
  "M-RET" 'cider-eval-last-sexp)

(general-def '(n i) clojure-mode-map
  "M-RET" 'cider-eval-defun-at-point)

(general-def 'v clojure-mode-map
  "M-RET" 'cider-eval-region)

(general-def 'n magit-status-mode-map
  ;; Magit binds the M-w to another command, so change it back to my keybinding
  "M-w" 'kill-current-buffer)

(general-def '(n i) with-editor-mode-map
  ;; Make the M-w similar to the "close" behavior, but 'kill-current-buffer' breaks the magit process, so adjust for it 
  "M-w" 'with-editor-cancel
  "M-RET" 'with-editor-finish)

(defhydra gn-org/hydra (:color pink :hint nil)
  "
| Motion^^                  | Agenda^^ |
|---------------------------+----------|
| _<down>_ : next headline  |          |
| _<up>_: previous headline |          |
| _<left>_: parent headline |          |
| _f_: goto headline        |          |
| _c_: goto clocking todo^^ |          |
  "
  ;; Navigation
  ("<down>" org-next-visible-heading)
  ("<up>" org-previous-visible-heading)
  ("<left>" outline-up-heading)
  ("f" org-goto)

  ;; Todo stuff
  ("c" org-clock-goto)

  ;; Quit
  ("q" nil "quit")
  ("<escape>" nil "quit"))

(general-def 'n org-mode-map
  ;; General org-mode usage
  "S-SPC" 'gn-org/hydra/body
  "RET" 'gn/org-dwim-at-point
  "M-h" 'org-metaleft
  "M-H" 'org-shiftmetaleft
  "M-l" 'org-metaright
  "M-L" 'org-shiftmetaright
  "C-M-g" 'gn/org-dwim-at-point)

;; Source mode map
(general-def 'n org-src-mode-map
  "M-o" 'find-file
  "M-e" 'switch-to-buffer
  "M-s" 'save-buffer
  "M-w" 'org-edit-src-abort
  "M-q" 'save-buffers-kill-terminal)

(general-def '(n i) plantuml-mode-map
  "M-RET" 'gn/plantuml-preview)
