;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

;; Config settings are available in an org-mode
;; file. This function call loads them.
(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "ed4c48eb91d07c2e447b445e2491ef17e9b326d43a60022297fd56af4749e772" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "8e51e44e5b079b2862335fcc5ff0f1e761dc595c7ccdb8398094fb8e088b2d50" "c2efd2e2e96b052dd91940b100d86885337a37be1245167642451cf6da5b924a" "65f35d1e0d0858947f854dc898bfd830e832189d5555e875705a939836b53054" "819d24b9aba8fcb446aecfb59f87d1817a6d3eb07de7fdec67743ef32194438b" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "d1af5ef9b24d25f50f00d455bd51c1d586ede1949c5d2863bef763c60ddf703a" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "667e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default))
 '(ede-project-directories '("/home/jaymorgan/workspace/cristallo/energy-estimation"))
 '(org-agenda-files '("~/Dropbox/Notes/tasks.org"))
 '(package-selected-packages
   '(org-make-toc helm-lsp web-mode html-mode docker mu4e-alert doom-modeline julia-repl quelpa-use-package fzf org-latex focus ace-window lsp-julia quelpa atom-one-dark-theme one-dark-theme php-mode org-ref ox-gfm ox-pandoc ox-md esqlite calibre-mode olivetti use-package-ensure-system-package helm-ag pdf-tools blacken black which-key slime projectile powerline markdown-mode magit linum-relative julia-mode imenu-list hydra htmlize helm git-gutter eyebrowse evil-collection disable-mouse diminish base14-theme adaptive-wrap))
 '(powerline-display-hud t)
 '(send-mail-function 'smtpmail-send-it)
 '(vterm-kill-buffer-on-exit t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Remove the GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
