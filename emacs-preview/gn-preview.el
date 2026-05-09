;;; gn-preview.el --- Preview things in Emacs in the browser -*- lexical-binding: t; -*-

;; Author: Gene Nakagaki <gene.nakagaki@gmail.com>
;; Maintainer: Gene Nakagaki <gene.nakagaki@gmail.com>
;; Keywords: org, markdown, plantuml

;;; Commentary:

;; This library is a tool that previews stuff
;; opened in Emacs to the browser

;;; Code:

(require 'ox-md)
(require 'org-roam-export)

(defvar gn-preview/file (expand-file-name "emacs-preview/preview.md"
                                          user-emacs-directory))


;;; Custom PlantUML babel export
(defvar org-babel-default-header-args:plantuml
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a plantuml source block.")

(defcustom org-plantuml-jar-path nil
  "Path to the plantuml.jar file."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-execute:plantuml (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	     (out-file (or (cdr (assoc :file params))
		               (error "PlantUML requires a \":file\" header argument")))
	     (cmdline (cdr (assoc :cmdline params)))
	     (in-file (org-babel-temp-file "plantuml-"))
	     (java (or (cdr (assoc :java params)) ""))
	     (cmd (if (not org-plantuml-jar-path)
		          (error "`org-plantuml-jar-path' is not set")
		        (concat "java " java " -jar "
			            (shell-quote-argument
			             (expand-file-name org-plantuml-jar-path))
			            (if (string= (file-name-extension out-file) "svg")
			                " -tsvg" "")
			            (if (string= (file-name-extension out-file) "eps")
			                " -teps" "")
			            " -p " cmdline " < "
			            (org-babel-process-file-name in-file)
			            " > "
			            (org-babel-process-file-name out-file)))))
    (unless (file-exists-p org-plantuml-jar-path)
      (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
    (with-temp-file in-file (insert (concat "@startuml\n" body "\n@enduml")))
    (message "%s" cmd) (org-babel-eval cmd "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:plantuml (session params)
  "Return an error because plantuml does not support sessions."
  (error "Plantuml does not support sessions"))


(defun gn-preview/backlinks-at-point ()
  (interactive)
  (let* ((backlinks (car (org-roam-backlinks-get (org-roam-node-at-point))))
         )
    (print (org-roam-backlink-source-node backlinks))
    )
  )


(defun gn-preview/org-md-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (let ((outfile (expand-file-name "emacs-preview/preview.md"
                                   user-emacs-directory)))
    (org-export-to-file 'md outfile async subtreep visible-only)))

(defun gn-preview ()
  "Previews currently opened file."
  (interactive)
  (gn-preview/org-md-export-to-markdown))


(defun gn-preview/image (image-url)
  "Preview IMAGE-URL image."
  (with-temp-file gn-preview/file
    (progn
      (insert "---
  layout: home
  ---

  ![An image]("image-url")"))))


(provide 'gn-preview)

;;; gn-preview.el ends here
