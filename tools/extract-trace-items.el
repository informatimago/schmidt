(require 'pjb-cl)

;; The extract-trace-items script scans an org-file containings tagged
;; paragraphs: sections starting with a tag
;; 
;; Paragraphs starting with tag numbers in brackets [Lnnnn] and ending
;; with ¶ are tracable elements.  They are extracted to feed a tracing
;; database or a TO-DO list.  They may be duplicated (same tag number)
;; for legibility of the text.
;; 
;; Note: within the brackets, there may be several tag numbers separated
;; with dots, representing a hierachical dependency path.  A tag number
;; may have several dependents (parents) in the graph: the same items is
;; reused in the specification of the parent item.
;; 
;; Only the last tag number in the path identifies the item (paragraph).


;; We could allow:
;; 
;;     [S9999] blah [S8888] blah blah ¶ blah ¶
;; 
;; to be equivalent to:
;; 
;;     [S9999] blah ¶
;;     [S9999.S8888] blah blah ¶

"
category            specification
id                  1
code                [S9999]
context             \"whatever'
title               \"title\"
language            \"EN\"
tags                (a b c)
contents            \"text\"
depends-on          (S9999 S9998)
dependents          (S9999 S9998)
/changed-dependents (S9999 S9998)

"

(defconstant +paragraph-open+      ?\[)
(defconstant +tag-separator+       ?\.)
(defconstant +paragraph-tag-end+   ?\])
(defconstant +table-open+          ?\|
  "Lines starting with that character are considered to be tables, and are kept as-is.")
(defconstant +paragraph-close+     ?\¶)
(defconstant +whitespaces+        " \t")
(defconstant +newlines+           "\n\r")

(defconstant +space+              32)
(defconstant +newline+            ?\n)



(defstruct (item (:type list))
  code
  dependents
  dependencies
  text
  file)



(defun whitespacep (ch)
  (find ch +whitespaces+))

(defun newlinep (ch)
  (find ch +newlines+))

(defun clean-item-text (text)
  (string-capitalize
   (let ((string '()))
     (flet ((write-char (ch) (push ch string)))
       (loop
         with state = :newline
         for ch across (string-trim text " -" " -")
         do (case state
              (:space    (cond
                           ((whitespacep ch))
                           ((newlinep ch)
                            (write-char ch)
                            (setf state :newline))
                           (t
                            (write-char +space+)
                            (write-char ch)
                            (setf state :word))))
              (:word     (cond
                           ((whitespacep ch)
                            (setf state :space))
                           ((newlinep ch)
                            (write-char ch)
                            (setf state :newline))
                           (t
                            (write-char ch))))
              (:newline (cond ((whitespacep ch))
                              ((newlinep ch)
                               (write-char ch))
                              ((char= +table-open+ ch)
                               (write-char ch)
                               (setf state :table))
                              (t
                               (write-char ch)
                               (setf state :word))))
              (:table  (cond ((newlinep ch)
                              (write-char ch)
                              (setf state :newline))
                             (t
                              (write-char ch))))))
       (coerce (nreverse string) 'string)))
   :end 1))



(defun* %extract-trace-items-from-buffer (&optional (buffer (current-buffer)))
  (let ((items (make-hash-table :test (function equal)))
        (file  (buffer-file-name buffer)))
    (save-excursion
     (with-current-buffer buffer
       (goto-char (point-min))
       (flet ((next-char ()
                (prog1 (char-after)
                  (ignore-errors (forward-char)))))
         (loop
           with depends = '()
           with code = nil
           with item = '()
           with state = :out
           with tag = '()
           for ch = (next-char)
           while ch
           ;; do (message "%S" (list :depends depends :code code :item item :state state :tag tag :ch ch))
           do (case state
                (:out     (when (char= +paragraph-open+ ch)
                            (setf tag '()
                                  state :code)))
                (:code    (cond
                            ((or (digit-char-p ch)
                                 (char= +tag-separator+ ch)
                                 (and (alpha-char-p ch)
                                      (upper-case-p ch)))
                             (push ch tag))
                            ((char= +paragraph-tag-end+ ch)
                             (setf depends (split-string (coerce (nreverse tag) 'string) "\\." t)
                                   code    (first (last depends))
                                   item    '()
                                   state   :item))
                            (t ;; Invalid code character in brackets, let's ignore it.
                             (setf state :out))))
                (:item (if (char= +paragraph-close+ ch)
                           (progn
                             (push (make-item
                                    :code code
                                    :dependents depends
                                    :text (clean-item-text
                                           (coerce (nreverse item) 'string))
                                    :file file)
                                   (gethash code items '()))
                             (setf state :out))
                           (push ch item))))
           finally (unless (eq state :out)
                     (error "End of file in %s state." state))))))
    items))

(defun item-list (items)
  (let ((item-list '()))
    (maphash (lambda (code item-data)
               (declare (ignore code))
               (push item-data item-list))
             items)
    (sort* item-list (function string<)
           :key (lambda (items) (item-code (first items))))))

(defun reconcile-items (item-list &optional file)
  "
When there are more than one paragraph with the same tag, we need to
see if they're the same or if they're different, to decide on whether
we must concatenate them or just remove the duplicate.
"
  (let ((items (make-hash-table :test (function equal))))
    (dolist (item-descs item-list)
      (let ((item (if (rest item-descs)
                      ;; More than one paragraph for the same tag:
                      ;; let's compute a new item merging them.
                      (let ((code (item-code (first item-descs)))
                            (paths '())
                            (texts '()))
                        (dolist (item item-descs)
                          (push (item-dependents item) paths)
                          (push (item-text       item) texts))
                        (let ((paths (remove* 1 (remove-duplicates paths :test (function equalp))
                                              :key (function length)))
                              (texts (remove-duplicates texts :test (function string-equal))))
                          (if (rest texts)
                              (progn
                                (warn "Multiple different texts for item ~s: %S"
                                      code texts) 
                                (make-item
                                 :code code
                                 :dependents paths
                                 :text (mapconcat (function identity) texts "\n")
                                 :file file))
                              (make-item
                               :code code
                               :dependents paths
                               :text (first texts)
                               :file file))))
                      ;; Only one paragraph for the tag: we just
                      ;; update the dependencies, removing the item
                      ;; itself from its dependents.
                      (let ((item (first item-descs)))
                        (setf (item-dependents item)
                              (let ((path (item-dependents item)))
                                (if (= 1 (length path))
                                    '()
                                    (list path))))
                        item))))
        (setf (gethash (item-code item) items) item)))
    (maphash (lambda (code item)
               (dolist (dependent (item-dependents item))
                 (block next
                   (push code (item-dependencies
                               (or (gethash (first (last dependent 2)) items)
                                   (warn "Unknown dependent code: ~s for item ~s."
                                         (first (last dependent 2))
                                         code)
                                   (return-from next)))))))
             items)
    (let ((item-list '()))
      (maphash (lambda (code item)
                 (declare (ignore code))
                 (push item item-list))
               items)
      (sort* item-list (function string<)
             :key (lambda (item) (item-code item))))))

(defun* extract-trace-items-from-buffer (&optional (buffer (current-buffer)))
  (reconcile-items (item-list (%extract-trace-items-from-buffer buffer))
                   (buffer-file-name buffer)))




(defun first-line (text)
  (let ((newline (position +newline+ text)))
    (if newline
        (values (subseq text 0 newline) text)
        (values text nil))))

;; (defun print-todo-list-for-items (item-list)
;;   (let ((*print-case* :upcase))
;;     (format t "* TO DO~%")
;;     (dolist (item item-list)
;;       (destructuring-bind (code dependents dependencies text) item
;;         (multiple-value-bind (first-line body) (first-line text)
;;           (format t "~%** TODO [~A] ~A~%" code first-line)
;;           (when (or (when dependents
;;                       (format t "~%Dependents: ~{~{~A~^.~}~^ ~}" dependents)
;;                       t)
;;                     (when dependencies
;;                       (format t "~%Dependencies: ~{~A~^ ~}" dependencies)
;;                       t)
;;                     (when body
;;                       (format t "~%~A" (string-trim "\n" body))
;;                       t))
;;             (format t "~%")))))))


(defun* optimum (sequence greaterp &key (key (function identity)))
  "Returns (values object index) of the maximum object according to `greaterp',
or (values nil nil) if the sequence is empty."
  (etypecase sequence
    ((or string vector)
     (if (zerop (length sequence))
         (values nil nil)
         (loop
           with maxi = 0
           with maxc = (aref sequence 0)
           with maxk = (funcall key maxc)
           for i from 0
           for c across sequence
           for k = (funcall key c)
           when (funcall greaterp k maxk)
             do (setf maxi i
                      maxc c
                      maxk k)
           finally (return (values maxc maxi)))))
    (list
     (if (endp sequence)
         (values nil nil)
         (loop
           with maxi = 0
           with maxc = (first sequence)
           with maxk = (funcall key maxc)
           for i from 0
           for c in sequence
           for k = (funcall key c)
           when (funcall greaterp k maxk)
             do (setf maxi i
                      maxc c
                      maxk k)
           finally (return (values maxc maxi)))))))

(defun* item-max-code (&optional (buffer (current-buffer)))
  (let ((item (nth-value 0 (optimum (extract-trace-items-from-buffer buffer)
                               (function string>)
                                    :key (function item-code)))))
    (when item
      (item-code item))))

;; (item-max-code (buffer-named "specifications.org"))
;; "S7004"


(defvar *code-category-map*
  '(("R" . requirement)
    ("S" . specification)
    ("A" . analysis)
    ("D" . design)
    ("C" . code)
    ("U" . unit-test)
    ("I" . integration-test)
    ("D" . documentation)))

(defun first-element (sequence)
  (elt sequence 0))

(defun item-category (item)
  (cdr (assoc* (aref (item-code item) 0)
               *code-category-map*
               :key (function first-element))))

(defun item-title (item)
  (nth-value 0 (first-line (item-text item))))

(defun item-identification (item)
  (format "%s/%s"
          ;; (item-category item)
          (item-code item)
          (item-title item)))

(defun item-increment-code (code)
  (let ((c (aref code 0))
        (n (car (read-from-string code 1))))
    (loop
      do (incf n)
      while (save-excursion
              (goto-char (point-min))
              (re-search-forward (format "\\[%c%04d\\]" c n) nil t)))
    (format "%c%04d" c n)))

(defun item-new-code ()
  (save-excursion
   (handler-case (previous-item)
     (error ()
       (goto-char (point-min))
       (next-item))))
  (item-increment-code (item-code *current-item*)))



(defvar *current-item* nil)
(defvar *items* nil)
(make-variable-buffer-local '*current-item*)
(make-variable-buffer-local '*items*)

(defun item-fill-current-item ()
  (setf *current-item*
        (make-item :code (match-string 1)
                   :text (match-string 2))))

(defun next-item ()
  (interactive)
  (when (re-search-forward *item-re* nil t)
    (item-fill-current-item)
    (goto-char (match-end 2))))

(defun previous-item ()
  (interactive)
  (when (re-search-backward *item-re* nil t)
    (item-fill-current-item)
    (goto-char (match-end 2))))

(defun insert-new-item ()
  (interactive)
  (insert (format "[%s] ¶" (item-new-code)))
  (backward-char 1))


(defparameter *clage-mode-map*
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "i"   'insert-new-item)
    (define-key map "C-i" 'insert-new-item)
    (define-key map "n"   'next-item)
    (define-key map "C-n" 'next-item)
    (define-key map "p"   'previous-item)
    (define-key map "C-p" 'previous-item)
    map))

(local-set-key (kbd "C-c i") *clage-mode-map*)
