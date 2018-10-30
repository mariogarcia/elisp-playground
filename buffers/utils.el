;;; package --- Summary
;;; Code:
;;; Commentary:

(defun count-words-buffer ()
  "Count words in a buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer cointains %d words." count))))

(defun goto-percent (pct)
  "Move cursor to a specific percentage (indicated by PCT) of the buffer."
  (interactive "nGoto percent: ")
  (let* ((size (point-max))
         (charpos (/ (* size pct) 100)))
    (goto-char charpos)))

;;; utils.el ends here
