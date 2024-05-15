;; Use ESC key to switch to command mode
;; (define-key key-translation-map (kbd "ESC") (kbd "<insert>"))

;; set up buffer local variable for M-ijkl motion in insert mode
(defvar-local use-M-ijkl-in-insert-mode t
"use the M-i, M-j to move in insert mode"
)

(defun xah-keys-have-priority ()
  "Try to ensure that xah keybindings retain priority over other minor modes"
  (interactive)
  (unless (eq (caar minor-mode-map-alist) 'xah-fly-keys)
    (let ((mykeys (assq 'xah-fly-keys minor-mode-map-alist)))
      (assq-delete-all 'xah-fly-keys minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(defun setup-M-ijkl-in-insert-mode ()
  (interactive)
  (if (and use-M-ijkl-in-insert-mode (not (eq major-mode 'vterm-mode)))
      (progn
        (define-key xah-fly-key-map (kbd "M-i") 'previous-line)
        (define-key xah-fly-key-map (kbd "M-k") 'next-line)
        (define-key xah-fly-key-map (kbd "M-j") 'backward-char)
        (define-key xah-fly-key-map (kbd "M-l") 'forward-char)
        )
    (define-key xah-fly-key-map (kbd "M-i") nil)
    (define-key xah-fly-key-map (kbd "M-k") nil)
    (define-key xah-fly-key-map (kbd "M-j") nil)
    (define-key xah-fly-key-map (kbd "M-l") nil)
    )
  )

(add-hook 'xah-fly-insert-mode-activate-hook 'setup-M-ijkl-in-insert-mode)

;; Switch by default to insert mode for some major modes
(defun my-switch-to-default-mode ()
  (interactive)
  (cond
   ((eq major-mode 'matlab-mode) (xah-fly-command-mode-activate))
   ((eq major-mode 'python-mode) (xah-fly-command-mode-activate))
   ((eq major-mode 'org-mode) (xah-fly-command-mode-activate))
   ((eq major-mode 'emacs-lisp-mode) (xah-fly-command-mode-activate))
   ((eq major-mode 'matlab-shell-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'grep-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'ivy-occur-grep-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'ripgrep-search-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'eshell-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'inferior-python-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'matlab-navigate-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'dired-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'ibuffer-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'magit-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'magit-status-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'magit-popup-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'pdf-view-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'vterm-mode) (xavier/vterm-setup-xah-command-mode))
   ((eq major-mode 'org-agenda-mode) (xah-fly-insert-mode-activate))
   ;; ((eq major-mode 'ediff-mode) (xah-fly-insert-mode-activate))
   (t nil))
  )

(defun my-switch-to-default-mode-1 (dummy)
  (my-switch-to-default-mode)
  )

(defun my-switch-to-default-mode-1-opt-1 (dummy1 &optional dummy2)
  (my-switch-to-default-mode)
  )

(defun my-switch-to-default-mode-opt-2 (&optional dummy1 dummy2)
  (my-switch-to-default-mode)
  )

(defun my-switch-to-default-mode-opt-1 (&optional dummy)
  (my-switch-to-default-mode)
  )

(defvar my-window-keymap (make-sparse-keymap))
(define-key my-window-keymap (kbd "M-i") 'my-windmove-up)
(define-key my-window-keymap (kbd "M-k") 'my-windmove-down)
(define-key my-window-keymap (kbd "M-j") 'my-windmove-left)
(define-key my-window-keymap (kbd "M-l") 'my-windmove-right)

(defun my-window-switch-setup ()
  (interactive)
  (my-switch-to-default-mode)
  (set-transient-map my-window-keymap t)
  )

(add-hook 'my-windmove-hook #'my-window-switch-setup)
;; (add-hook 'buffer-list-update-hook #'my-switch-to-default-mode)

;; Extra keybindings for specific major modes in command mode
(defun my-bindkey-xfk-command-mode ()
  "Define keys for `xah-fly-command-mode-activate-hook'"
  (interactive)
  (cond
   ((eq major-mode 'magit-mode) (define-key xah-fly-key-map "q" 'magit-bury-buffer))
   ((eq major-mode 'matlab-mode) (define-key xah-fly-key-map (kbd "æ") 'matlab-debug-keymap))
   ((eq major-mode 'python-mode) (define-key xah-fly-key-map (kbd "æ") 'python-debug-keymap))
   ((eq major-mode 'julia-mode) (define-key xah-fly-key-map (kbd "æ") 'julia-debug-keymap))
   ((eq major-mode 'org-mode) (progn (define-key xah-fly-key-map (kbd "æ") 'org-extra-keymap)
                                     (define-key xah-fly-key-map (kbd "I") 'org-shiftup)
                                     (define-key xah-fly-key-map (kbd "K") 'org-shiftdown)
                                     (define-key xah-fly-key-map (kbd "J") 'org-shiftleft)
                                     (define-key xah-fly-key-map (kbd "L") 'org-shiftright)
                                     ))
   ((eq major-mode 'org-link-mode) (define-key xah-fly-key-map (kbd "æ") 'org-extra-keymap))
   ((eq major-mode 'python-mode) (define-key xah-fly-key-map (kbd "æ") 'elpy-command-map))
   ((eq major-mode 'dired-mode) (define-key xah-fly-key-map (kbd "æ") 'dired-command-map))
   ((eq major-mode 'latex-mode) (define-key xah-fly-key-map (kbd "æ") 'latex-extra-keymap))
   ((eq major-mode 'LaTeX-mode) (define-key xah-fly-key-map (kbd "æ") 'latex-extra-keymap))
   ((eq major-mode 'matlab-shell-mode) (define-key xah-fly-key-map (kbd "æ") 'matlab-shell-debug-keymap))
   ((eq major-mode 'vterm-mode) (xavier/vterm-setup-xah-command-mode))
   (t nil)))
  
(add-hook 'xah-fly-command-mode-activate-hook 'my-bindkey-xfk-command-mode)
  
;; Add vi-like delete and switch to insert mode

(defmacro create-func-with-insert-mode-switch (func)
  "Given a function, create another function with name prefixed
by my- which switches to insert mode after execution"
  `(defun ,(intern (concat "my-" (symbol-name func))) ()
     (interactive)
     (,func)
     (xah-fly-insert-mode-activate)
     )
  )

(create-func-with-insert-mode-switch xah-kill-word)
(create-func-with-insert-mode-switch xah-backward-kill-word)
(create-func-with-insert-mode-switch xah-beginning-of-line-or-block)
(create-func-with-insert-mode-switch end-of-line)
(create-func-with-insert-mode-switch kill-line)
(create-func-with-insert-mode-switch right-char)
(create-func-with-insert-mode-switch left-char)
(create-func-with-insert-mode-switch forward-word)
(create-func-with-insert-mode-switch backward-word)
(create-func-with-insert-mode-switch xah-cut-line-or-region)
(create-func-with-insert-mode-switch xah-cut-line-or-region)
(create-func-with-insert-mode-switch yank)
(create-func-with-insert-mode-switch end-of-buffer)

(defun my-delete-char ()
  (interactive)
  (delete-char 1)
  (xah-fly-insert-mode-activate)
  )
(defun my-delete-backward-char ()
  (interactive)
  (delete-char -1)
  (xah-fly-insert-mode-activate)
  )

;; Insert blank line above and below
(defun my-insert-blank-line-below ()
  "insert blank line below"
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (xah-fly-insert-mode-activate)
  (indent-for-tab-command)
  )

(defun my-insert-blank-line-above ()
  "insert blank line above"
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (xah-fly-insert-mode-activate)
  (indent-for-tab-command)
  )


(define-prefix-command 'vi-type-delete-and-insert-keymap)
(define-key vi-type-delete-and-insert-keymap "r" #'my-xah-kill-word)
(define-key vi-type-delete-and-insert-keymap "e" #'my-xah-backward-kill-word)
(define-key vi-type-delete-and-insert-keymap "d" #'my-delete-char)
(define-key vi-type-delete-and-insert-keymap "s" #'my-delete-backward-char)
(define-key vi-type-delete-and-insert-keymap "h" #'my-xah-beginning-of-line-or-block)
(define-key vi-type-delete-and-insert-keymap "ø" #'my-end-of-line)
(define-key vi-type-delete-and-insert-keymap "t" #'my-kill-line)
(define-key vi-type-delete-and-insert-keymap "l" #'my-right-char)
(define-key vi-type-delete-and-insert-keymap "j" #'my-left-char)
(define-key vi-type-delete-and-insert-keymap "u" #'my-backward-word)
(define-key vi-type-delete-and-insert-keymap "o" #'my-forward-word)
(define-key vi-type-delete-and-insert-keymap "x" #'my-xah-cut-line-or-region)
(define-key vi-type-delete-and-insert-keymap "v" #'my-yank)
(define-key vi-type-delete-and-insert-keymap "z" #'my-end-of-buffer)
(define-key vi-type-delete-and-insert-keymap "i" #'my-insert-blank-line-above)
(define-key vi-type-delete-and-insert-keymap "k" #'my-insert-blank-line-below)

(defun add-vi-delete-and-switch-to-insert-mode-bindings ()
  (interactive)
  (define-key xah-fly-key-map "a" 'vi-type-delete-and-insert-keymap)
  )

(add-hook 'xah-fly-command-mode-activate-hook 'add-vi-delete-and-switch-to-insert-mode-bindings)

;; utility: versions of xah-fly-command-mode-activate with arguments
(defun my-xah-fly-command-mode-activate-2 (&optional a b)
  "Version that takes argument - to be used in advice"
  (xah-fly-command-mode-activate)
  )
(defun my-xah-fly-command-mode-activate-1 (&optional a)
  "Version that takes argument - to be used in advice"
  (xah-fly-command-mode-activate)
  )

;; Advice some functions
(advice-add 'find-file :after 'my-switch-to-default-mode-1-opt-1)
(advice-add 'quit-window :after 'my-switch-to-default-mode-opt-2)
(advice-add 'xah-close-current-buffer :after 'my-switch-to-default-mode)
;; (advice-remove 'kill-buffer 'my-switch-to-default-mode-1-opt)

;; extra setting for yas
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") #'yas-expand)

;; extra setting for company
(define-key company-active-map (kbd "M-i") 'company-select-previous-or-abort)
(define-key company-active-map (kbd "M-k") 'company-select-next-or-abort)
;; (add-hook 'company-completion-started-hook #'my-xah-fly-insert-mode-activate-1)
;; (add-hook 'company-completion-cancelled-hook #'my-switch-to-default-mode-1)
;; (add-hook 'company-completion-finished-hook  #'my-switch-to-default-mode-1)

;; extra setting for matlab functions
(advice-add #'matlab-jump-to-file-at-line :after #'my-xah-fly-command-mode-activate-1)
(advice-add #'matlab-navigate-dbstack :after #'xah-fly-insert-mode-activate)
(advice-add #'switch-to-matlab :after #'xah-fly-insert-mode-activate)

;; extra setting for magit
(advice-add #'magit-status :after #'xah-fly-insert-mode-activate)
(defun my-setup-magit ()
  (xah-fly-insert-mode-activate)
  (define-key magit-file-section-map "a" nil)
  (define-key magit-mode-map "k" 'next-line)
  (define-key magit-mode-map "i" 'previous-line)
  (define-key magit-mode-map "," 'magit-delete-thing)
  )
(add-hook 'magit-mode-hook #'my-setup-magit)

;; Ivy settings
(with-eval-after-load 'ivy-init
  (define-key ivy-minibuffer-map (kbd "M-i") 'previous-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'next-line)
  (define-key ivy-minibuffer-map (kbd "M-I") 'ivy-insert-current)
  (advice-add 'ivy--switch-buffer-action :after 'my-switch-to-default-mode-1)
  (advice-add 'ivy--switch-buffer-other-window-action :after 'my-switch-to-default-mode-1)
  (advice-add 'ivy--kill-buffer-action :after 'my-switch-to-default-mode-1)
  (advice-add 'swiper--action :after 'my-switch-to-default-mode-1)
  (advice-add 'next-error-no-select :after 'my-switch-to-default-mode-opt-1)
  (add-hook 'ivy-occur-grep-mode-hook #'setup-ivy-occur-grep-mode)
  )

;; Multiple cursor settings
(defvar my-mc-keymap (make-sparse-keymap))
(define-key my-mc-keymap (kbd "k") 'mc/mark-next-like-this)
(define-key my-mc-keymap (kbd "i") 'mc/unmark-next-like-this)
(defun my-mc-start ()
  (interactive)
  (set-transient-map my-mc-keymap t)
  )
(add-hook 'multiple-cursors-mode-disabled-hook #'xah-fly-command-mode-activate)
(add-hook 'multiple-cursors-mode-enabled-hook #'xah-keys-have-priority)

;; Scroll up/down settings
(defvar my-scroll-keymap (make-sparse-keymap))
(define-key my-scroll-keymap (kbd "i") 'scroll-down-command)
(define-key my-scroll-keymap (kbd "k") 'scroll-up-command)
(define-key my-scroll-keymap (kbd "<return>") 'my-exit-scroll-keymap)

(defun my-exit-scroll-keymap ()
  (interactive)
  (out-of-scroll)
  )

(defun my-scroll-start ()
  (interactive)
  (fset 'out-of-scroll (set-transient-map my-scroll-keymap t))
  )

(add-hook 'xah-fly-command-mode-activate-hook
          (lambda () (define-key xah-fly-key-map "n" 'my-scroll-start))
          )
(add-hook 'xah-fly-insert-mode-activate-hook
          (lambda () (define-key xah-fly-key-map "n" nil))
          )

;; extra setting for backward search
(define-key isearch-mode-map  (kbd "<home>") 'isearch-repeat-backward)
(define-key isearch-mode-map  (kbd "<tab>") 'isearch-repeat-forward)

;; pdf-vew settings
(defun setup-pdf-view ()
  (interactive)
  (define-key pdf-view-mode-map "\C-s" 'isearch-forward)
  (define-key pdf-view-mode-map "w" 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map "h" 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map "H" 'describe-mode)
  (define-key pdf-view-mode-map "g" 'pdf-view-goto-page)
  (define-key pdf-view-mode-map "i" 'previous-line)
  (define-key pdf-view-mode-map "k" 'next-line)
  (define-key pdf-view-mode-map "j" 'image-backward-hscroll)
  (define-key pdf-view-mode-map "l" 'image-forward-hscroll)
  (define-key pdf-view-mode-map "<" 'image-bob)
  (define-key pdf-view-mode-map "z" 'image-eob)
  (define-key pdf-view-mode-map "a" 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map "\C-f" 'pdf-links-isearch-link)
  (define-key pdf-view-mode-map "3" 'delete-other-windows)
  (define-key pdf-view-mode-map "1" 'pdf-annot-add-highlight-markup-annotation)
  (require 'pdf-links)
  (define-key pdf-links-minor-mode-map "f" nil)
  (define-key pdf-view-mode-map "f" 'isearch-forward)
  )

(add-hook 'pdf-view-mode-hook 'setup-pdf-view)

;; Dired settings
(define-prefix-command 'dired-command-map)
(defun setup-dired ()
  (interactive)
  (define-key dired-mode-map "3" 'delete-other-windows)
  (define-key dired-mode-map "i" 'previous-line)
  (define-key dired-mode-map "f" 'swiper-isearch)
  (define-key dired-mode-map "k" 'next-line)
  (define-key dired-mode-map "e" 'dired-toggle-read-only)
  (define-key dired-command-map "e" 'dired-toggle-read-only)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (xah-fly-insert-mode-activate)
  )

(add-hook 'dired-mode-hook 'setup-dired)
(advice-add 'dired-find-file :after 'my-switch-to-default-mode)
(advice-add 'dired-jump :after 'my-switch-to-default-mode-opt-2)
(advice-add 'wdired-finish-edit :after 'my-switch-to-default-mode)

(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map "f" 'swiper-isearch)))

;; agenda setting
(defun setup-agenda-mode ()
  (interactive)
  (define-key org-agenda-mode-map "i" 'org-agenda-previous-line)
  (define-key org-agenda-mode-map "k" 'org-agenda-next-line)
  (xah-fly-insert-mode-activate)
  )

(add-hook 'org-agenda-finalize-hook 'setup-agenda-mode)

;; org settings
(defun setup-org-mode ()
  (interactive)
  (setq use-M-ijkl-in-insert-mode nil)
  (define-key org-mode-map (kbd "M-i") 'org-metaup)
  (define-key org-mode-map (kbd "M-k") 'org-metadown)
  (define-key org-mode-map (kbd "M-j") 'org-metaleft)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-I") 'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-K") 'org-shiftmetadown)
  (define-key org-mode-map (kbd "M-J") 'org-shiftmetaleft)
  (define-key org-mode-map (kbd "M-L") 'org-shiftmetaright)
  )
(add-hook 'org-mode-hook #'setup-org-mode)

;; grep mode settings
(defun setup-grep-mode ()
  (interactive)
  (define-key grep-mode-map (kbd "i") 'previous-error-no-select)
  (define-key grep-mode-map (kbd "k") 'next-error-no-select))

(add-hook 'grep-mode-hook #'setup-grep-mode)

(defun setup-ivy-occur-grep-mode ()
  (interactive)
  (define-key ivy-occur-grep-mode-map (kbd "i") 'previous-error-no-select)
  (define-key ivy-occur-grep-mode-map (kbd "k") 'next-error-no-select))

(add-hook 'ivy-occur-grep-mode-hook #'setup-ivy-occur-grep-mode)

(defun setup-ripgrep-search-mode ()
  (interactive)
  (define-key ripgrep-search-mode-map (kbd "i") 'previous-error-no-select)
  (define-key ripgrep-search-mode-map (kbd "k") 'next-error-no-select))

(add-hook 'ripgrep-search-mode-hook #'setup-ripgrep-search-mode)


;; ibuffer settings
(defun setup-ibuffer ()
  (interactive)
  (define-key ibuffer-mode-map "i" 'previous-line)
  (define-key ibuffer-mode-map "k" 'next-line)
  (xah-fly-insert-mode-activate)
  )
(add-hook 'ibuffer-hook #'setup-ibuffer)



;; Use which-key package
(require 'which-key)
(which-key-mode)

;; Helper function
(defun my-key-convert ()
  (interactive)
  (let ((str (read-string "char: ")))
    (message (xah--qwerty-to-dvorak str))
    )
  )

(defun xah--qwerty-to-dvorak (@charstr)
  "Convert qwerty to dvorak key."
  (interactive)
  (if (> (length @charstr) 1)
      @charstr
    (let (($result (rassoc @charstr xah--dvorak-to-qwerty-kmap)))
      (if $result
          (car $result)
        @charstr
        ))))


(defun select-whole-line ()
  (interactive)
  (beginning-of-line)
  (push-mark-command nil)
  (end-of-line)
  (set-transient-map my-whole-line-select-map t)
  )

(defun move-down-one-line ()
  (interactive)
  (next-line)
  (end-of-line)
  )

(defun move-up-one-line ()
  (interactive)
  (previous-line)
  (beginning-of-line)
  )

(defvar my-whole-line-select-map (make-sparse-keymap))
(define-key my-whole-line-select-map (kbd "i") 'move-up-one-line)
(define-key my-whole-line-select-map (kbd "k") 'move-down-one-line)
(define-key xah-fly-leader-key-map (kbd "t") 'select-whole-line)

(provide 'extra-mode-bindings)
 
