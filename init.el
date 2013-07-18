;; package.el stuff
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(
    clojure-mode
    color-theme-solarized
    haskell-mode
    htmlize
    magit
    paredit
    smex
    sml-mode
    solarized-theme
    tuareg
    glsl-mode
    ))
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
(add-hook 'sml-mode-hook
          (lambda ()
            (local-set-key (kbd "M-SPC") 'set-mark-command)))

;; Proof General.
;; C-c C-n to submit form. C-c C-u to retract. C-c C-ret to sumbit up to point
(when (file-exists-p "~/.emacs.d/ProofGeneral-4.2/generic/proof-site.el")
  (load-file "~/.emacs.d/ProofGeneral-4.2/generic/proof-site.el")
  ;; locally bind Coq shortcuts above to make it easier to step through large files
  (add-hook 'coq-mode-hook
            '(lambda ()
               (local-set-key (kbd "<f7>") 'proof-assert-next-command-interactive)
               (local-set-key (kbd "<f6>") 'proof-undo-last-successful-command)
               (local-set-key (kbd "<f5>") 'proof-goto-point)
               (local-set-key (kbd "S-<right>")
                              'proof-assert-next-command-interactive)
               (local-set-key (kbd "S-<left>") 'proof-undo-last-successful-command)
               (local-set-key (kbd "S-<down>") 'proof-goto-point))))
(load-file "~/.emacs.d/sh.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "green4" :inverse-video nil :underline nil :slant normal :weight bold))) t)
 '(diff-removed ((t (:inherit diff-changed :foreground "#d70000" :inverse-video nil :underline nil :slant normal :weight bold))) t)
 '(font-lock-type-face ((t (:foreground "#708183"))))
 '(proof-eager-annotation-face ((t (:background "#708183" :foreground "#042028"))))
 '(proof-error-face ((t (:background "#708183" :foreground "#042028"))))
 '(proof-locked-face ((t (:background "#708183" :foreground "#042028"))))
 '(tuareg-font-lock-governing-face ((t (:foreground "#728a05" :weight bold))))
 '(tuareg-font-lock-operator-face ((t (:foreground "#708183")))))
;; On OS X, Cmd-space (ie M-SPC) is far too valuable "real-estate" for
;; Spotlight, so use it for set-mark instead, which works nicely with
;; held-down spacebar remapped to control
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; paredit-mode on for lisp
(autoload 'enable-paredit-mode "paredit" "" t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(add-to-list 'load-path "/Users/aki/source/org-mode/lisp/")
(add-to-list 'load-path "/Users/aki/source/ob-sml/")
(add-hook 'org-mode-hook (lambda () (visual-line-mode)))

(setq scheme-program-name "gsc")

(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
