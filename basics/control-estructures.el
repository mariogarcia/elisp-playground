;;; package --- Control estructures
;;; Code:
;;; Commentary:

(defun ce-check-if (word)
  "Play with if clause depending on a WORD."
  (interactive "sSome word: ")
  (if (equal word "amigo")
      (message "Always so friendly my friend")
    (message "Hey Yo!")))

(defun ce-check-cond (word)
  "Play with cond clause depending on a WORD."
  (interactive "sSome word: ")
  (cond ((equal "amigo" word)
         (message "Hey amigo"))
        ((equal "say hi" word)
         (message "Hi!"))
        (t
         (message "Whatever!"))))

;;; control-estructures.el ends here
