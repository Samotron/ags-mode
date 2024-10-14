;;; ags-mode.el --- Support for the AGS Format files

(require 'generic-x)

(defvar ags-column-colors
  '("#ff6b6b" "#4ecdc4" "#45b7d1" "#f9d56e" "#ff8a5c" "#9b59b6" "#3498db" "#2ecc71")
  "List of colors for AGS columns, suitable for dark themes.")

(defun ags-make-faces ()
  "Create faces for AGS columns."
  (dolist (color ags-column-colors)
    (let ((face-name (intern (format "ags-column-%s-face" (substring color 1)))))
      (eval `(defface ,face-name
               '((t :foreground ,color))
               ,(format "Face for AGS column in %s." color))))))

(ags-make-faces)

(defvar-local ags-current-table-columns nil
  "List of column names for the current table.")

(defun ags-update-table-columns ()
  "Update the list of column names for the current table."
  (save-excursion
    (let ((columns nil))
      (when (re-search-backward "^\"GROUP\"" nil t)
        (forward-line)
        (when (looking-at "^\"HEADING\"")
          (let ((line (buffer-substring-no-properties (point) (line-end-position))))
            (setq columns (split-string (substring line 1 -1) "\",\"")))))
      (setq ags-current-table-columns columns))))

(defvar ags-font-lock-keywords
  `(("^\"GROUP\"\\(.*\\)"
     (1 '(face (:foreground "#ff6b6b" :weight bold)) t))
    ("^\"HEADING\"\\(.*\\)"
     (1 '(face (:foreground "#4ecdc4" :weight bold)) t))
    ("^\\(\"[^\"]*\"\\)\\(,\\(\"[^\"]*\"\\)\\)*"
     (0 (ags-colorize-line))))
  "Font-lock keywords for `ags-mode'.")

(defun ags-colorize-line ()
  "Apply colors to the current line in AGS mode."
  (save-excursion
    (let ((start (point))
          (end (line-end-position))
          (color-index 0))
      (while (re-search-forward "\"[^\"]*\"\\|," end t)
        (let* ((color (nth (mod color-index (length ags-column-colors)) ags-column-colors))
               (face (intern (format "ags-column-%s-face" (substring color 1)))))
          (put-text-property (match-beginning 0) (match-end 0) 'face face))
        (setq color-index (1+ color-index))))
    nil))

(define-derived-mode ags-mode fundamental-mode "AGS"
  "Major mode for editing AGS (Association of Geotechnical & Geoenvironmental Specialists) data files."
  (setq font-lock-defaults '(ags-font-lock-keywords))
  (font-lock-mode 1)
  (add-hook 'post-command-hook 'ags-update-modeline nil t))

(add-to-list 'auto-mode-alist '("\\.ags\\'" . ags-mode))

;; Variable to hold the current group, heading, and column for the modeline
(defvar-local ags-current-info ""
  "Current group, heading, and column displayed in the modeline.")

;; Function to get the current column number and name
(defun ags-current-column ()
  "Get the current column number and name in the AGS file."
  (let ((col 1)
        (pos (point)))
    (save-excursion
      (beginning-of-line)
      (while (< (point) pos)
        (when (looking-at ",")
          (setq col (1+ col)))
        (forward-char)))
    (cons col (nth (1- col) ags-current-table-columns))))



;; Function to update the modeline with current GROUP, HEADING, and column
(defun ags-update-modeline ()
  "Update the modeline with the current GROUP, HEADING, and column."
  (ags-update-table-columns)
  (let ((group "")
        (heading "")
        (column-info (ags-current-column)))
    (save-excursion
      (goto-char (point-at-bol))
      (when (re-search-backward "^\"GROUP\"" nil t)
        (setq group (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (when (re-search-forward "^\"HEADING\"" nil t)
          (setq heading (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
    (setq ags-current-info
          (concat
           (propertize (format " %s | " group) 'face '(:foreground "#ff6b6b" :weight bold))
           (propertize (format "Column %d: %s | "
                               (car column-info)
                               (or (cdr column-info) ""))
                       'face `(:foreground ,(nth (mod (1- (car column-info)) (length ags-column-colors)) ags-column-colors)
                                           :weight bold))
           (propertize (format "%s | " heading) 'face '(:foreground "#4ecdc4" :weight bold))

           ))
    (force-mode-line-update)))

;; Update the modeline format to include the current group, heading, and column
(setq-default mode-line-format
              (list '(:eval ags-current-info)
                    " " mode-line-buffer-identification))

;; ... [navigation functions remain the same] ...

;; Function to jump to the next table
(defun ags-jump-to-next-table ()
  "Jump to the next table in the AGS file."
  (interactive)
  (if (re-search-forward "^\"GROUP\"" nil t)
      (progn
        (beginning-of-line)
        (recenter-top-bottom))
    (message "No more tables found.")))

;; Function to jump to the previous table
(defun ags-jump-to-previous-table ()
  "Jump to the previous table in the AGS file."
  (interactive)
  (if (re-search-backward "^\"GROUP\"" nil t)
      (progn
        (beginning-of-line)
        (recenter-top-bottom))
    (message "No more tables found.")))

(defun ags-export-current-table-to-csv ()
  "Export the current selected AGS table to a CSV file."
  (interactive)
  (let ((table-start (save-excursion
                       (re-search-backward "^\"GROUP\"" nil t)
                       (line-beginning-position)))
        (table-end (save-excursion
                     (if (re-search-forward "^\"GROUP\"" nil t)
                         (line-beginning-position)
                       (point-max)))))
    (if (and table-start table-end)
        (let* ((table-content (buffer-substring-no-properties table-start table-end))
               (csv-file (read-file-name "Save CSV file as: " nil "table.csv" nil)))
          (with-temp-buffer
            (insert table-content)
            ;; Replace double quotes with single quotes for CSV format
            (while (search-forward "\"" nil t)
              (replace-match "\"\"")) ;; Escape quotes in CSV
            ;; Write to the CSV file
            (write-region (point-min) (point-max) csv-file)
            (message "Table exported to %s" csv-file)))
      (message "No table found to export."))))





(provide 'ags-mode)
