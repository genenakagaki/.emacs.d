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


(provide 'gn-preview)

;;; gn-preview.el ends here
