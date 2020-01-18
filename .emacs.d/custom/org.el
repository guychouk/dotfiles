(use-package gnuplot)
(use-package org
  :pin org
  :ensure org-plus-contrib
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  :config
  (require 'ob-js)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell . t)
				 (gnuplot . t)
				 (js . t))))
(use-package org-bullets
  :requires org
  :config
  (org-bullets-mode 1))
