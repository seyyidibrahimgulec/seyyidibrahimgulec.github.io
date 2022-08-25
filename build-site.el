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

(defun ig/site-footer (info)
  (concat
   (sxml-to-xml
    `(footer (@ (class "footer center pt-5"))
             (a (@ (href "https://www.linkedin.com/in/seyyidibrahimgulec/") (target "_blank"))
                (span (@ (class "fa-brands fa-linkedin fa-2x") )))
             (a (@ (href "https://github.com/seyyidibrahimgulec") (target "_blank"))
                (span (@ (class "fa-brands ml-10 fa-github fa-2x"))))
             (p "Made with " '(*RAW-STRING* ,(plist-get info :creator)))))))

(setq org-html-preamble  #'ig/site-header
      org-html-postamble #'ig/site-footer
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'site-html
      org-html-html5-fancy nil
      org-html-htmlize-output-type 'css
      org-html-htmlize-font-prefix "org-"
      org-html-self-link-headlines t
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-validation-link nil
      org-html-doctype "html5"
      org-html-head "<link rel=\"stylesheet\" href=\"/static/css/miligram.min.css\" /><link rel=\"stylesheet\" href=\"/static/css/style.css\" /><script src=\"https://kit.fontawesome.com/13e97c0bd7.js\" crossorigin=\"anonymous\"></script>")

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
             :base-directory "./content"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
             :with-author nil           ;; Don't include author name
             :with-creator nil          ;; Don't include Emacs and Org versions in footer
             :with-toc t                ;; Include a table of contents
             :section-numbers nil       ;; Don't include section numbers
             :time-stamp-file nil)))    ;; Don't include time stamp in file

;; Generate the site output
(org-publish-all t)

(message "Build complete!")
