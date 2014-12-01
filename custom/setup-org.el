;; ORG MODE
(require 'org)

(setq org-directory (expand-file-name "~/ownCloud/org")
      org-agenda-files (list (expand-file-name "todo.org" org-directory)
			     (expand-file-name "thesis.org" org-directory)
			     (expand-file-name "baby.org" org-directory)
			     )
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      ;; org-completion-use-ido t
      org-yank-adjusted-subtrees t)
;; (setq org-directory (expand-file-name "~/Dropbox/Org")
;;       org-agenda-files (list (expand-file-name "work.org" org-directory)
;;                           (expand-file-name "thesis.org" org-directory)
;;                           (expand-file-name "home.org" org-directory))
;;       org-default-notes-file (expand-file-name "notes.org" org-directory)
;;       org-completion-use-ido t
;;       org-yank-adjusted-subtrees t)
(make-directory org-directory :with-parents)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C") 'org-capture)

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("website-pages"
	 :base-directory "~/ownCloud/website/org/"
	 :base-extension "org"
	 :publishing-directory "~/ownCloud/website/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t
	 :auto-sitemap t
	 :sitemap-filename "sitemap.org"
	 :sitemap-title "Sitemap"
	 :export-creator-info nil
	 :export-author-info nil
	 :auto-postamle nil
	 :table-of-contents t
	 :section-numbers nil
	 :html-postamble "    <p class=\"postamble\">Last Updated %d.</p> "
	 :style-include-default nil)
	("website-static"
	 :base-directory "~/ownCloud/website/org/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|ogg\\|swf"
	 :publishing-directory "~/ownCloud/website/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("website"
	 :components ("website-pages" "website-static"))))

(provide 'setup-org)
