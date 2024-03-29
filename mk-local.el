
;; my customizations on Jack Rusher's excellect emacs config
;; see https://github.com/jackrusher/dotemacs
;; This lives in ~/.emacs/lisp/

;; set initial window size to left half of screen
(custom-set-variables
 '(default-frame-alist
    (quote
     ((width . 158)
      (height . 100)
      (top . 0)
      (left . 0)))))

;; use find-file to open files
(global-set-key (kbd "s-o") 'find-file)

;; extra packages
(use-package counsel-projectile   :ensure t)
(use-package projectile           :ensure t)


(add-hook 'focus-out-hook (lambda ()
                            (save-some-buffers t)))

(require 'counsel-projectile)
(global-set-key (kbd "s-F") 'counsel-projectile-ag)

;; I never want to print the current buffer.
;; I’d rather get a fuzzy-searchable list of function definitions
(global-set-key (kbd "s-p") 'imenu)

;; Search in project with ag
(require 'counsel-projectile)
(global-set-key (kbd "s-F") 'counsel-projectile-ag)

;; Fuzzy-find files in project as you type
(require 'projectile)
(global-set-key (kbd "s-t") 'projectile-find-file)
(projectile-mode +1)
(setq projectile-indexing-method 'hybrid)


;; moving between open buffers
(global-set-key (kbd "C-s-<left>") 'previous-buffer)
(global-set-key (kbd "C-s-<right>") 'next-buffer)

(global-set-key (kbd "s-d") 'mc/mark-next-like-this)

;; aggressive indent mode for LISPs
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(use-package flycheck-clj-kondo :ensure t)
(use-package clojure-mode :ensure t :config (require 'flycheck-clj-kondo))
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)


(setq cider-save-file-on-load 't)

;; don't annoy me
(setq magit-save-repository-buffers 'dontask)
(setq global-auto-revert-mode 't)
(setq cider-prompt-for-symbol nil)

;; Disable pretty lambda and function symbols as it breaks formatting
(global-prettify-symbols-mode 0)

;; Cursor
(setq-default cursor-type 'bar)

;; Web Mode
(use-package web-mode :ensure t)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(global-set-key (kbd "M-s-.") 'web-mode-element-close)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))
(defun shift-right ()
  (interactive)
  (shift-region 2))
(defun shift-left ()
  (interactive)
  (shift-region -2))
(global-set-key (kbd "s-]") 'shift-right)
(global-set-key (kbd "s-[") 'shift-left)
;; Nextjournal-specific functions

(global-set-key (kbd "C-c C-g") 'magit)

(setq geiser-racket-binary "/Applications/Racket v7.8/bin/racket")

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))

(define-key clojure-mode-map  (kbd "<M-return>") 'clerk-show)
(define-key markdown-mode-map (kbd "<M-return>") 'clerk-show)


;; from https://github.com/corgi-emacs/corgi-packages/blob/449e5120e4743127659643e4b1e89d037c5b9bcf/corgi-clojure/corgi-clojure.el#L119-L132
(defun corgi/around-cider--choose-reusable-repl-buffer (_command _params)
  "Redefine cider--choose-reusable-repl-buffer to something more
sensible. If any dead REPL buffers exist when creating a new one
then simply delete them first. Return nil so `cider-create-repl'
creates a new one. Don't unnecessarily bother the user."
  (seq-do #'kill-buffer
          (seq-filter (lambda (b)
                        (with-current-buffer b
                          (and (derived-mode-p 'cider-repl-mode)
                               (not (process-live-p (get-buffer-process b))))))
                      (buffer-list)))
  nil)

(advice-add #'cider--choose-reusable-repl-buffer :around #'corgi/around-cider--choose-reusable-repl-buffer)

;; (setq exec-path (append exec-path '("/opt/homebrew/bin")))


(setq lsp-headerline-breadcrumb-enable nil)

(use-package exec-path-from-shell)

(progn
  (setenv "SHELL" "/opt/homebrew/bin/fish")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "JAVA_HOME")))

(use-package diff-hl)
(global-diff-hl-mode)

(defun toggle-presentation-mode ()
  "Toggle between 15 point and 26 point type in the current frame."
  (interactive)
  (let* ((font (face-attribute 'default :font))
         (size (font-get font :size)))
    (set-frame-font (font-spec :size (if (= size 15) 26 15)))))
