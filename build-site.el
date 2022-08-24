;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)
(package-install 'haskell-mode)
(package-install 'yaml-mode)
(package-install 'vimrc-mode)
(package-install 'esxml)

;; Load the publishing system
(require 'ox-publish)
(require 'esxml)

(setq make-backup-files nil)

(defun ig/site-header (info)
  (let* ((file (plist-get info :output-file)))
    (concat
     (sxml-to-xml
      `(div
        (div (@ (class "navbar"))
             (ul (@ (class "nav"))
                 (li (a (@ (class "first-element") (href "/")) "Homepage") " ")
                 (li (a (@ (href "/emacs-configuration/index.html")) "Emacs") " ")
                 (li (a (@ (href "/dotfiles/index.html")) "Dotfiles") " "))))))))

;; Customize the HTML output
(setq org-html-preamble  #'ig/site-header
      org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"/static/css/miligram.min.css\" /><link rel=\"stylesheet\" href=\"/static/css/style.css\" />")

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
             :base-directory "./content"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
             :with-author nil           ;; Don't include author name
             :with-creator t            ;; Include Emacs and Org versions in footer
             :with-toc t                ;; Include a table of contents
             :section-numbers nil       ;; Don't include section numbers
             :time-stamp-file nil)))    ;; Don't include time stamp in file

;; Generate the site output
(org-publish-all t)

(message "Build complete!")
