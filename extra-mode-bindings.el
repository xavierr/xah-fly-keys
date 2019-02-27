;; Use ESC key to switch to command mode
;; (define-key key-translation-map (kbd "ESC") (kbd "<insert>"))

;; set up buffer local variable for M-ijkl motion in insert mode
(defvar-local use-M-ijkl-in-insert-mode t
"use the M-i, M-j to move in insert mode"
)

(defun setup-M-ijkl-in-insert-mode ()
  (interactive)
  (if use-M-ijkl-in-insert-mode
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
   ((eq major-mode 'emacs-lisp-mode) (xah-fly-command-mode-activate))
   ((eq major-mode 'matlab-shell-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'eshell-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'inferior-python-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'matlab-navigate-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'dired-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'ibuffer-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'magit-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'magit-status-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'magit-popup-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'pdf-view-mode) (xah-fly-insert-mode-activate))
   ((eq major-mode 'ediff-mode) (xah-fly-insert-mode-activate))
   (t nil))
  )

(defun my-switch-to-default-mode-1 (dummy)
  (my-switch-to-default-mode)
  )

(defun my-switch-to-default-mode-1-opt (dummy1 &optional dummy2)
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
(defun my-switch-to-default-mode-1-1-opt (&optional dummy1 dummy2)
  (my-switch-to-default-mode)
  )

;; (add-hook 'buffer-list-update-hook #'my-switch-to-default-mode)

;; Extra keybindings for specific major modes in command mode
(defun my-bindkey-xfk-command-mode ()
  "Define keys for `xah-fly-command-mode-activate-hook'"
  (interactive)
  (cond
   ((eq major-mode 'magit-mode) (define-key xah-fly-key-map "q" 'magit-bury-buffer))
   ((eq major-mode 'matlab-mode) (define-key xah-fly-key-map (kbd "æ") 'matlab-debug-keymap))
   ((eq major-mode 'python-mode) (define-key xah-fly-key-map (kbd "æ") 'elpy-command-map))
   ((eq major-mode 'dired-mode) (define-key xah-fly-key-map (kbd "æ") 'dired-command-map))
   ((eq major-mode 'matlab-shell-mode) (define-key xah-fly-key-map (kbd "æ") 'matlab-shell-debug-keymap))
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
(define-key vi-type-delete-and-insert-keymap "i" #'xah-fly-insert-mode-activate)

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
(advice-add 'find-file :after 'my-switch-to-default-mode-1-opt)
(advice-add 'quit-window :after 'my-switch-to-default-mode-1-1-opt)
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
  (define-key magit-hunk-section-map "a" nil)
  )
(add-hook 'magit-mode-hook #'my-setup-magit)

;; Ivy settings
(define-key ivy-minibuffer-map (kbd "M-i") 'previous-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'next-line)
(define-key ivy-minibuffer-map (kbd "M-I") 'ivy-insert-current)
(advice-add 'ivy--switch-buffer-action :after 'my-switch-to-default-mode-1)
(advice-add 'ivy--switch-buffer-other-window-action :after 'my-switch-to-default-mode-1)
(advice-add 'ivy--kill-buffer-action :after 'my-switch-to-default-mode-1)
(advice-add 'swiper--action :after 'my-switch-to-default-mode-1)

;; Multiple cursor settings
(defvar my-mc-keymap (make-sparse-keymap))
(define-key my-mc-keymap (kbd "k") 'mc/mark-next-like-this)
(define-key my-mc-keymap (kbd "i") 'mc/unmark-next-like-this)
(defun my-mc-start ()
  (interactive)
  (set-transient-map my-mc-keymap t)
  )
(add-hook 'multiple-cursors-mode-disabled-hook #'xah-fly-command-mode-activate)

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

;; Scroll delete/space setting
(defvar my-delete-and-space-keymap (make-sparse-keymap))
(defun my-insert-space ()
  (interactive)
  (insert " ")
  )
(define-key my-delete-and-space-keymap (kbd "SPC") #'my-insert-space)
(defun my-delete-and-space-start ()
  (interactive)
  (set-transient-map my-delete-and-space-keymap t)
  )

(add-hook 'xah-fly-command-mode-activate-hook
          (lambda () (define-key xah-fly-key-map "å" 'my-delete-and-space-start))
          )
(add-hook 'xah-fly-insert-mode-activate-hook
          (lambda () (define-key xah-fly-key-map "å" nil))
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
  (define-key pdf-view-mode-map "a" 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map "\C-f" 'pdf-links-isearch-link)
  (define-key pdf-view-mode-map "f" 'isearch-forward)
  (require 'pdf-links)
  (define-key pdf-links-minor-mode-map "f" nil)
  )

(add-hook 'pdf-view-mode-hook 'setup-pdf-view)

;; Dired settings
(define-prefix-command 'dired-command-map)
(defun setup-dired ()
  (interactive)
  (define-key dired-mode-map "i" 'previous-line)
  (define-key dired-mode-map "k" 'next-line)
  (define-key dired-command-map "e" 'dired-toggle-read-only)
  (define-key dired-mode-map "K" 'dired-do-kill-lines)
  (xah-fly-insert-mode-activate)
  )

(add-hook 'dired-mode-hook 'setup-dired)
(advice-add 'dired-find-file :after 'my-switch-to-default-mode)
(advice-add 'dired-jump :after 'my-switch-to-default-mode-1-1-opt)
(advice-add 'wdired-finish-edit :after 'my-switch-to-default-mode)

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



;; ibuffer settings
(defun setup-ibuffer ()
  (interactive)
  (define-key ibuffer-mode-map "i" 'previous-line)
  (define-key ibuffer-mode-map "k" 'next-line)
  (xah-fly-insert-mode-activate)
  )
(add-hook 'ibuffer-hook #'setup-ibuffer)

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

(define-key xah-fly-leader-key-map (kbd "h") 'my-insert-blank-line-above)
(define-key xah-fly-leader-key-map (kbd "n") 'my-insert-blank-line-below)

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

(defun reset-escape-key ()
  (interactive)
  (shell-command "xmodmap /home/xavier/.Xmodmap")
  )

(provide 'extra-mode-bindings)
 
