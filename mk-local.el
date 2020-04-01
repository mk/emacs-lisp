;; my customizations on Jack Rusher's excellect emacs config
;; see https://github.com/jackrusher/dotemacs
;; This lives in ~/.emacs/lisp/

;; use find-file to open files
(global-set-key (kbd "s-o") 'find-file)

;; extra packages
(use-package counsel-projectile   :ensure t)
(use-package projectile           :ensure t)

(require 'counsel-projectile)
(global-set-key (kbd "s-F") 'counsel-projectile-ag)

;; I never want to print the current buffer.
;; I’d rather get a fuzzy-searchable list of function definitions
(global-set-key (kbd "s-p") 'imenu)

;; Fuzzy-find files in project as you type
(require 'projectile)
(global-set-key (kbd "s-t") 'projectile-find-file)
(projectile-mode +1)

;; Search in project with ag
(require 'counsel-projectile)
(global-set-key (kbd "s-F") 'counsel-projectile-ag)

;; Fuzzy-find files in project as you type
(require 'projectile)
(global-set-key (kbd "s-t") 'projectile-find-file)
(projectile-mode +1)

;; clj-kondo
(use-package flycheck :ensure t)
(use-package flycheck-clj-kondo :ensure t)
(use-package clojure-mode :ensure t :config (require 'flycheck-clj-kondo))


(defun cider-thing-at-point (thing &optional bounds)
  "Return the thing at point as a string, otherwise nil.
THING being a valid argument for bounds-of-thing-at-point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (when-let* ((b (or (and (equal (char-after) ?\()
                          (member (char-before) '(?\' ?\, ?\@))
                          ;; hide stuff before ( to avoid quirks with '( etc.
                          (save-restriction
                            (narrow-to-region (point) (point-max))
                            (bounds-of-thing-at-point thing)))
                     (and
                      (eq thing 'sexp)
                      (member (char-after) '(?\) ?\} ?\]))
                      (bounds-of-thing-at-point 'list))
                     (bounds-of-thing-at-point thing))))
    (funcall (if bounds #'list #'buffer-substring-no-properties)
             (car b) (cdr b))))

(defun cider-list-at-point (&optional bounds)
  "Return the list at point as a string, otherwise nil.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (cider-thing-at-point 'list bounds))


(defun cider-eval-list-at-point (&optional output-to-current-buffer)
  "Evaluate the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (let ((b (cider-list-at-point 'bounds)))
    (cider-eval-region (car b) (cadr b))))

(defun nextjournal/cider-pprint-register (register)
  (interactive (list (register-read-with-preview "Eval register: ")))
  (cider--pprint-eval-form (get-register register)))


(defun cider-interactive-cljs-eval (form &optional callback bounds additional-params)
  "copied from sider but only sending form to :cljs repl"
  (let ((form  (or form (apply #'buffer-substring-no-properties bounds)))
        (start (car-safe bounds))
        (end   (car-safe (cdr-safe bounds))))
    (when (and start end)
      (remove-overlays start end 'cider-temporary t))
    (unless (and cider-interactive-eval-override
                 (functionp cider-interactive-eval-override)
                 (funcall cider-interactive-eval-override form callback bounds))
      (cider-map-repls :cljs
        (lambda (connection)
          (cider--prep-interactive-eval form connection)
          (cider-nrepl-request:eval
           form
           (or callback (cider-interactive-eval-handler nil bounds))
           ;; always eval ns forms in the user namespace
           ;; otherwise trying to eval ns form for the first time will produce an error
           (if (cider-ns-form-p form) "user" (cider-current-ns))
           (when start (line-number-at-pos start))
           (when start (cider-column-number-at-pos start))
           additional-params
           connection))))))

;;nextjournal specific functions
(defun nextjournal/new-clojure-scratch-buffer (scratch-name)
  (interactive "sScratch file name: ")
  (let* ((scratch-file (expand-file-name (concat "journal/scratch/" scratch-name ".clj")
                                         (projectile-project-root)))
         (template-file (expand-file-name "journal/scratch/templates/clojure.clj" (projectile-project-root))))
    (copy-file template-file
               scratch-file)
    (find-file scratch-file)))

(defun nextjournal/new-clojurescript-scratch-buffer (scratch-name)
  (interactive "sScratch file name: ")
  (let* ((scratch-file (expand-file-name (concat "journal/scratch/" scratch-name ".cljs")
                                         (projectile-project-root)))
         (template-file (expand-file-name "journal/scratch/templates/clojurescript.cljs" (projectile-project-root))))
    (copy-file template-file
               scratch-file)
    (find-file scratch-file)))


(defun nextjournal/eval-defun-at-point-and-refresh ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-interactive-cljs-eval "(do (require '[re-frame.core]) (re-frame.core/dispatch [:refresh]))"))

(defun nextjournal/add-nextjournal-cljs-repl-type ()
  (when (not (seq-some (lambda (entry) (eq 'nextjournal (car entry))) cider-cljs-repl-types))
    (add-to-list 'cider-cljs-repl-types '(nextjournal "(do (require 'com.nextjournal.journal.repl) (com.nextjournal.journal.repl/wait-for-figwheel) (com.nextjournal.journal.repl/editor-repl))" nil))))


(eval-after-load "cider"
  '(progn
     (setq cider-auto-track-ns-form-changes nil)
     (setq cider-use-overlays nil)
     (defadvice cider--choose-reusable-repl-buffer (around auto-confirm compile activate)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest args) t))
                ((symbol-function 'completing-read) (lambda (prompt collection &rest args) (car collection))))
        ad-do-it))

    (nextjournal/add-nextjournal-cljs-repl-type)))

;; I like this keybinding from Lighttable
(eval-after-load 'clojure-mode
  '(progn
     ;; shift+enter to eval form and refresh
     (define-key clojure-mode-map (kbd "<S-return>") 'nextjournal/eval-defun-at-point-and-refresh)))

