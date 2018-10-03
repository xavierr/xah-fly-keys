(require 'which-key)
(which-key-mode)

(defvar my-mc-keymap (make-sparse-keymap))
(define-key my-mc-keymap (kbd "k") 'mc/mark-next-like-this)
(define-key my-mc-keymap (kbd "i") 'mc/unmark-next-like-this)
(defun my-mc-start ()
  (interactive)
  (set-transient-map my-mc-keymap t)
  )


(defun my-bindkey-xfk-command-mode ()
  "Define keys for `xah-fly-command-mode-activate-hook'"
  (interactive)

  (cond

   ;; if current mode is xah-html-mode, change some key
   ((eq major-mode 'dired-mode)
    (define-key xah-fly-key-map "b" 'dired-up-directory))
    ;; more major-mode checking here
    
    ;; if nothing match, do nothing
    (t nil)))
  
(add-hook 'xah-fly-command-mode-activate-hook 'my-bindkey-xfk-command-mode)

(define-key ivy-minibuffer-map (kbd "M-i") 'previous-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'next-line)
(define-key ivy-minibuffer-map (kbd "M-I") 'ivy-insert-current)

(defun setup-pdf-view ()
  (interactive)
  (define-key pdf-view-mode-map "\C-s" 'isearch-forward)
  (define-key pdf-view-mode-map "w" 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map "h" 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map "H" 'describe-mode)
  (define-key pdf-view-mode-map "g" 'pdf-view-goto-page)
  (define-key pdf-view-mode-map "i" 'previous-line)
  (define-key pdf-view-mode-map "k" 'next-line)
  (define-key pdf-view-mode-map "a" 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map "\C-f" 'pdf-links-isearch-link)
  (define-key pdf-links-minor-mode-map "f" nil)
  )

(add-hook 'pdf-view-mode-hook 'setup-pdf-view)
;; (add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)

(provide 'extra-mode-bindings)
