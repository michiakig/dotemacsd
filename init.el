;; package.el stuff
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(clojure-mode
    color-theme-solarized
    haskell-mode
    htmlize
    magit
    paredit
    smex
    sml-mode
    solarized-theme
    tuareg))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;; basic stuff
(tool-bar-mode -1) ; hide toolbar
(scroll-bar-mode -1) ; hide scrollbar
(setq initial-buffer-choice t) ; open *scratch* on launch
(setq ring-bell-function 'ignore)
(load-theme 'solarized-dark t)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . "~/.saves")))
;; mac os x specific stuff
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (set-default-font "Menlo")
    (let ((path-from-shell
           (replace-regexp-in-string "[[:space:]\n]*$" ""
                                     (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-mode t)
;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; emacsclient
(server-start)
;; org-mode
(setq org-src-fontify-natively t)
(setq org-export-htmlize-output-type 'css)
;; SML mode
(setq sml-default-arg "-Ccontrol.poly-eq-warn=false -Cprint.depth=100")
(setq sml-indent-level 3)

;; ProofGeneral.
;; C-c C-n to submit form. C-c C-u to retract. C-c C-ret to sumbit up to point
;; (load-file "~/.emacs.d/ProofGeneral-4.2/generic/proof-site.el")
