(setq reset '(matlab-init
              magit-init
              ivy-init
              various-init
              yasnippet
              multiple-cursors))
(setq erase '(xah-fly-keys
              extra2
              extra-mode-bindings))
(seq-remove (lambda (ft) (seq-contains-p (append reset erase) ft)) features)
(cl-loop for ft in reset do
         (require ft))
(if nil
    (progn
      (setq xah-brackets '( "“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠"))
      (setq xah-fly-key--current-layout "qwerty")
      (load-file "/home/xavier/.emacs.d/xah-fly-keys/xah-fly-keys.el")
      (xah-fly-keys 1)
      (load-file "/home/xavier/.emacs.d/xah-fly-keys/extra2.el")
      (load-file "/home/xavier/.emacs.d/xah-fly-keys/extra-mode-bindings.el"))
  (setq xah-fly-key--current-layout "qwerty")
  (load-file "/home/xavier/.emacs.d/xah-fly-keys2/xah-fly-keys.el")
  (xah-fly-keys 1)
  (load-file "/home/xavier/.emacs.d/xah-fly-keys2/extra-mode-bindings.el"))
