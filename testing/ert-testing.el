;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'seq)

(defun sort-by-string-length (list &optional asc-desc)
  "Sort a LIST of strings by their length.  Apply direction by ASC-DESC value which could be 'asc' or 'desc'."
  (progn
    (fset 'direction
          (if (equal asc-desc "desc")
              '>
            '<))
    (sort list (lambda (a b)
                 (direction (length a) (length b))))))

;;; Tests

(ert-deftest test-sort-by-default ()
  (should (equal (sort-by-string-length '("a" "aaa" "aa")) '("a" "aa" "aaa"))))

(ert-deftest test-sort-by-desc ()
  (should (equal (sort-by-string-length '("a" "aaa" "aa") "desc") '("aaa" "aa" "a"))))

(provide 'ert-testing)

;;; ert-testing.el ends here
