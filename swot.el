;; swot-mode.el --- A mode for spaced-repetition memory practice
;; By Darius Bacon and ChatGPT-4
;;
;; Swot mode is an Emacs major mode for spaced-repetition practice. It
;; works with an org-mode file, holding the cards in a human-readable
;; format. Each card (item to train on) is represented by an Org
;; headline like so:
;;
;; * Card
;;   :PROPERTIES:
;;   :review: <date>
;;   :history: <review timestamps and ratings>
;;   ...<other properties>...
;;   :END:
;; <question text (multiline)
;; ---
;; <answer text (ditto)>
;;
;; The buffer should have only cards, nothing more.
;; 
;; The :review: property schedules the next review of a card.
;;
;; The :history: property holds past ratings, with their dates. While
;; this may seem redundant, the history allows us to change the
;; scheduling algorithm later without worries about missing state on
;; older cards. The current algorithm is SM-2.
;;
;; A few other properties exist as SM-2 state.

(require 'cl-lib)
(require 'org)
(require 'org-element)


;;; Utilities

(defun swot-shuffled (sequence)
  "Return the elements of SEQUENCE as a list in random order."
  (let ((v (vconcat sequence)))
    (swot-shuffle-vector v)
    (append v '())))

(defun swot-shuffle-vector (vec)
  (cl-loop for i from (length vec) downto 2
           do (cl-rotatef (aref vec (random i))
                          (aref vec (1- i)))))

(defun swot-today ()
  (format-time-string "%Y-%m-%d"))


;;; The SM-2 algorithm, from Wozniak

(defun swot-sm2-update (rating state)
  "Compute the next review date and the associated change in SM-2 state variables."
  (cl-assert (<= 0 rating 5))
  (pcase-let ((`(,last-review ,rep-field ,interval-field ,EF-field) state))
    (let ((rep      (or rep-field 0))
          (interval (or interval-field 1))
          (EF       (or EF-field 2.5)))
      (cond ((< rating 3)
             (setq interval 1)
             (setq rep 0))
            (t (setq interval (cl-case rep
                                (0 1)
                                (1 6)
                                (t (round (* interval EF)))))
               (incf rep)))
      (setq EF (max 1.3 (+ EF -0.8 (* 0.28 rating) (* -0.02 rating rating))))
      (let ((next-review (time-add last-review (days-to-time interval))))
        `(,next-review ,rep ,interval ,EF)))))
    

;;; Cards as entries in this org-mode buffer
;;; Pre: point is at the card's heading.

(defun swot-goto-question ()
  "Move point to the start of the body of the current card."
  (org-back-to-heading)
  (forward-line)
  ;; Skip the properties drawer. (I can't find a function for this in
  ;; org-mode or org-element.)
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line)))

(defun swot-set-field (field-name string)
  "Set the value of FIELD-NAME in the current card to STRING."
  (org-entry-put (point) field-name string))

(defun swot-field (field-name)
  "Return the string value of a field of the current card."
  (org-entry-get (point) field-name))

(defun swot-review-date ()
  (let ((date-string (swot-field "review")))
    (and date-string (org-time-string-to-time date-string))))

(defun swot-due-for-review-p ()
  (let ((next-review (swot-review-date)))
    (and next-review (time-less-p next-review (current-time)))))

(defun swot-schedule-next-review (rating)
  (let ((rep      (string-to-number (or (swot-field "rep") "0")))
        (interval (string-to-number (or (swot-field "interval") "1")))
        (EF       (string-to-number (or (swot-field "EF") "2.5"))))
    (pcase-let ((`(,next-review ,rep ,interval ,EF)
                 (swot-sm2-update rating (list (swot-review-date) rep interval EF))))
      (swot-set-field "review" (format-time-string "%Y-%m-%d" next-review))
      (swot-set-field "rep" (number-to-string rep))
      (swot-set-field "interval" (number-to-string interval))
      (swot-set-field "EF" (number-to-string EF)))))

(defun swot-update-history (rating)
  "Update the review history for the current card with the given rating."
  (let ((event (format "%s:rate:%s" (swot-today) rating)))
    (swot-set-field "history" (concat (swot-field "history") " " event))))

(defun swot-gather-cards (keep-p)
  "List markers of all the cards that pass KEEP-P when point is at their headline."
  (let (result)
    (org-map-entries (lambda ()
                       (let ((entry (org-element-at-point)))
                         (when (and (eq (org-element-type entry) 'headline)
                                    (funcall keep-p))
                           (push (point-marker) result)))))
    (nreverse result)))


;;; UI

(define-derived-mode swot-mode org-mode "Swot"
  "Major mode for spaced-repetition practice with Org-mode files.")

(make-variable-buffer-local
 (defvar swot-cards '()
   "A list of markers for cards still to review in the current session."))

(defun swot-start-new-card ()
  "Insert a new card template, leaving point where you can type in the question."
  (interactive)
  (org-insert-heading)
  (insert "Card\n")
  (org-insert-drawer "PROPERTIES")
  (swot-set-field "review" (swot-today))
  (swot-set-field "history" "")
  (save-excursion
    (insert "\n---\n\n")))  ;; Question --- answer. Both initially blank.

(defun swot-start-review ()
  "Start a review session: find cards that are due, and show one."
  (interactive)
  (setq swot-cards (swot-shuffled (swot-gather-cards 'swot-due-for-review-p)))
  (when swot-cards
    (goto-char (marker-position (car swot-cards)))
    (swot-prompt-card)))

(defun swot-next-card ()
  "Continue a review with the next card in SWOT-CARDS."
  (when swot-cards
    (let ((next-card (marker-position (car swot-cards))))
      (goto-char (marker-position (car swot-cards)))
      (swot-prompt-card)
      (setq swot-cards (cdr swot-cards)))))

(defun swot-prompt-card ()
  "Show the current card's question."
  (swot-show (lambda (end)
               (if (search-forward "---" end)
                   (match-beginning 0)
                 end)))
  (message "Here's a question"))

(defun swot-reveal-card ()
  "Show the current card's question and answer."
  (interactive)
  (swot-show (lambda (end) end))
  (message "Now rate your recall"))

(defun swot-rate-yourself (rating)
  "Take your score on the current card (a rating from 0 to 5) and
schedule the next review."
  (interactive "nEnter your rating (0-5): ")
  (when (<= 0 rating 5)
    (swot-update-history rating)
    (swot-schedule-next-review rating)
    (swot-next-card)))

(defun swot-show (end-fn)
  "Narrow the buffer to a region from the current card's question
up to a position returned by END-FN."
  (widen)
  (org-reveal)
  (org-show-entry)
  (save-excursion
    (swot-goto-question)
    (narrow-to-region (point)
                      (funcall end-fn (save-excursion (org-end-of-subtree t t))))))


(provide 'swot-mode)
