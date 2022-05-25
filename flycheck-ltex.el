(require 'flycheck)

(flycheck-define-checker ltex
  "A ltex syntax checker using the ltex-cli.
"
  :command ("ltex-cli"  source)
  :error-parser ltex-error-parser
  )


(defun ltex-error-parser (output checker buffer)
  (let (
        (match (rx line-start (group-n 1 (regex ".*?")) ":"
                   (group-n 2 (regex ".*?"))
                   ":" 
                   (group-n 3 (regex ".*?"))
                   ": info:"
                   (group-n 4 (regex ".*?")  line-end ?\n)
                   (minimal-match (one-or-more not-newline)) line-end ?\n
                   (group-n 5 
                            (one-or-more (minimal-match (zero-or-more not-newline)) "Use '" (minimal-match (one-or-more not-newline)) "'\n")
                            )
                   ))
        (index 0)
        errors '()
        )
    (while (string-match match output index)
      (push (flycheck-error-new
       ;;:filename (match-string 1 output)
                    :line (string-to-number (match-string 2 output))
                    :column (string-to-number (match-string 3 output))
                    :message (concat (substring-no-properties (match-string 4 output)) (match-string 5 output)) 
                    ;:message (match-string 5 output) 
                    :level 'error
                    :buffer (current-buffer)
                    :checker 'ltex) errors)  
      (message (match-string 4 output))
      (setq index (match-end 0)))
    errors
    )
  )


(provide 'flycheck-ltex)

