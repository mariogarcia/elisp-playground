;;; package --- Summary
;;; Code:
;;; Commentary:

(defun ce-count-words-buffer ()
  "Count words in a buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer cointains %d words." count))))

(defun ce-count-words (from to)
  "Count words FROM TO."
  (how-many "\\w+" from to))

(defun ce-count-words-region ()
  "Count words in a defined region."
  (interactive)
  (let* ((start (region-beginning))
        (end (region-end))
        (count (ce-count-words start end)))
    (message "region countains %d words" count)))

(defun ce-goto-percent (pct)
  "Move cursor to a specific percentage (indicated by PCT) of the buffer."
  (interactive "nGoto percent: ")
  (let* ((size (point-max))
         (charpos (/ (* size pct) 100)))
    (goto-char charpos)))

(defun ce-count-lines-buffer ()
  "Count how many lines there are in the buffer."
  (interactive)
  (let ((count 1))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (re-search-forward "\n")
        (setq count (1+ count)))
      (message "buffer countains %d lines." count))))

(defun ce-what-line ()
  "Tell which line I am at."
  (interactive)
  (let ((current-point (point))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (<= (point) current-point)
        (re-search-forward "\n")
        (setq count (1+ count)))
      (message "you are at line %d." count))))

(defun ce-count-lines-until (condition)
  "Count lines until CONDITION is met."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (funcall condition (point))
        (re-search-forward "\n")
        (setq count (1+ count)))
      count)))

(defun ce-what-line-2 ()
  "Tell which line I am at refactored."
  (interactive)
  (let* ((current-point (point))
         (line-no (ce-count-lines-until
                   (lambda (p)
                     (<= (point) current-point)))))
    (message "you are at line %d." line-no)))

;;; utils.el ends here
