;; By ChatGPT-4 and Darius Bacon

;; In a repeated test cycle, start fresh on each reload.
(when (featurep 'swot-test)
  (unload-feature 'swot-test))

(require 'cl-lib)
(require 'ert)
(require 'swot-mode)

;;; Scaffolding

(defmacro swot-test-with-setup (&rest body)
  "Execute BODY with a temp-buffer in swot-mode and a standard
mocking of swot-today."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'swot-today) (lambda () swot-test-today)))
     (with-temp-buffer
       (swot-mode)
       ,(macroexp-progn body))))

(defvar swot-test-today "2023-04-01"
  "The pretended date while we're exercising swot-mode.")

(defun swot-test-insert-card (&rest lines)
  (save-excursion
    (insert "* Test Card\n")
    (insert ":PROPERTIES:\n")
    (insert ":review: 2023-01-01\n")
    (insert ":history:\n")
    (insert ":END:\n")
    (mapc 'insert lines)))

(defun swot-test-insert-paris ()
  "For initializing a new buffer with an example card."
  (swot-test-insert-card "What's the capital of France?\n"
                         "---\n"
                         "Paris\n"))


;;; Tests

(ert-deftest test-swot-goto-question ()
  "Test that `swot-goto-question` moves the point to the start of
the body of the current card."
  (swot-test-with-setup
    (swot-test-insert-paris)
    (swot-goto-question)

    (should (looking-at "What's the capital of France?"))))

(ert-deftest test-swot-set-field-and-swot-field ()
  "Test round trip of `swot-set-field` and `swot-field`."
  (swot-test-with-setup
    (swot-test-insert-paris)
    (swot-set-field "test-field" "test-value")

    (should (equal (swot-field "test-field") "test-value"))
    (should (equal (swot-field "review") "2023-01-01"))
    (should (equal (swot-field "history") ""))))

(ert-deftest test-swot-schedule-next-review ()
  "Test that `swot-schedule-next-review` schedules the next
review date correctly based on the provided rating."
  (swot-test-with-setup
    ;; TODO more cases to test
    (swot-test-insert-paris)
    (swot-schedule-next-review 3)

    (should (equal (swot-field "review") "2023-01-02"))
    ;; TODO actually check these
    (should (equal (swot-field "rep") "1"))
    (should (equal (swot-field "interval") "1"))
    (should (equal (swot-field "EF") "2.36"))))

(ert-deftest test-swot-update-history ()
  "Test that `swot-update-history` updates the review history for
the current card correctly based on the provided rating."
  (swot-test-with-setup
    (swot-test-insert-paris)
    (swot-update-history 4)

    (should (equal (swot-field "history") "2023-04-01:rate:4"))))

(ert-deftest test-swot-prompt-card ()
  (swot-test-with-setup
    (swot-test-insert-paris)
    (swot-prompt-card)
    
    (should (equal (buffer-string)
                   "What's the capital of France?\n"))
    ;; On re-widening, the buffer should be unchanged:
    (widen)
    (should (equal (buffer-string) (swot-test-with-setup
                                     (swot-test-insert-paris)
                                     (buffer-string))))))

(ert-deftest test-swot-reveal-card ()
  (swot-test-with-setup
    (swot-test-insert-paris)
    (swot-reveal-card)
    
    (should (equal (buffer-string)
                   "What's the capital of France?\n---\nParis\n"))
    ;; On re-widening, the buffer should be unchanged:
    (widen)
    (should (equal (buffer-string) (swot-test-with-setup
                                     (swot-test-insert-paris)
                                     (buffer-string))))))

;; TODO
;; could test starting from different points; and with multiple cards
;; also test that body is unchanged on changing properties
;; (defun swot-start-review ()
;; (defun swot-next-card ()
;; (defun swot-rate-yourself (rating)

(provide 'swot-test)
