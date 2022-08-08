
;;;      _              __ _ _      
;;;   __| | ___        / _(_) | ___ 
;;;  / _` |/ _ \ _____| |_| | |/ _ \
;;; | (_| | (_) |_____|  _| | |  __/
;;;  \__,_|\___/      |_| |_|_|\___|
                              
(defun clocker-do-file (file)
  ""

  (let* ((org-startup-folded nil)
         (org-startup-align-all-tables nil)
         (buffer (if (file-exists-p file)
                     (org-get-agenda-file-buffer file)
                   (error "No such file %s" file))))
    
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (error "Agenda file %s is not in Org mode" file))

      (org-refresh-category-properties)

      (let* ((ast (org-element-parse-buffer))
             (filename (buffer-file-name))
             (clocks (org-element-map
                         ast
                         '(clock)
                       #'clocker-get-clock-info))
             (headings (org-element-map
                           ast
                           '(headline inlinetask)
                         #'clocker-get-heading-info)))

        (list :filename filename
              :clocks clocks
              :headings headings)))))

;;;             _          _            _    
;;;   __ _  ___| |_    ___| | ___   ___| | __
;;;  / _` |/ _ \ __|  / __| |/ _ \ / __| |/ /
;;; | (_| |  __/ |_  | (__| | (_) | (__|   < 
;;;  \__, |\___|\__|  \___|_|\___/ \___|_|\_\
;;;  |___/                                   

(defun clocker-get-clock-info (clock)
  "Get clock information about CLOSED clock CLOCK.

If CLOCK is not closed, ignore it.

Information is returned in a plist with properties

+ :year-start
+ :month-start
+ :day-start
+ :hour-start
+ :minute-start
+ :year-end
+ :month-end
+ :day-end
+ :hour-end
+ :minute-end
+ :parent-heading (beginning position of parent heading)
"

  (let* ((value (org-element-property :value clock))
        (status (org-element-property :status clock))
        (drawer (org-element-property :parent clock))
        (section (org-element-property :parent drawer))
        (heading (org-element-property :parent section))
        (parent-heading-elm
         (org-element-lineage clock (list 'headline 'inlinetask) nil))
        (parent-heading (org-element-property :begin parent-heading-elm)))

    (when (string= status "closed")

      (setq timestamp (cadr value))
      
      (list
       :year-start (plist-get timestamp :year-start)
       :month-start (plist-get timestamp :month-start)
       :day-start (plist-get timestamp :day-start)
       :hour-start (plist-get timestamp :hour-start)
       :minute-start (plist-get timestamp :minute-start)
       :year-end (plist-get timestamp :year-end)
       :month-end (plist-get timestamp :month-end)
       :day-end (plist-get timestamp :day-end)
       :hour-end (plist-get timestamp :hour-end)
       :minute-end (plist-get timestamp :minute-end)
       :parent-heading parent-heading))))

;;;             _     _                    _ _             
;;;   __ _  ___| |_  | |__   ___  __ _  __| (_)_ __   __ _ 
;;;  / _` |/ _ \ __| | '_ \ / _ \/ _` |/ _` | | '_ \ / _` |
;;; | (_| |  __/ |_  | | | |  __/ (_| | (_| | | | | | (_| |
;;;  \__, |\___|\__| |_| |_|\___|\__,_|\__,_|_|_| |_|\__, |
;;;  |___/                                           |___/ 

(defun clocker-get-heading-info (hd)
  "Get information about heading or inlinetask HD.

Information is returned in a plist with properties

+ :begin
+ :name
+ :category
+ :tags
+ :parent-heading (beginning position of parent heading)
"

  (let* ((begin (org-element-property :begin hd))
         (name (org-element-property :raw-value hd))
         
         ;; If I use org-element-property to get the tags and the
         ;; categories, inherited stuff does not appear. Hence, the
         ;; following code is ugly and inefficient, as it goes around
         ;; the buffer to find the headline and get the properties.
         (tags (progn
                 (goto-char begin)
                 (setq raw (cdar (org-entry-properties (point) "ALLTAGS")))
                 (set-text-properties 0 (length raw) nil raw)
                 raw))
         (category (cdar (org-entry-properties (point) "CATEGORY")))

         (parent-heading-elm (org-element-lineage hd (list 'headline) nil))
         (parent-heading (org-element-property :begin parent-heading-elm)))
         
    (list 
     :begin begin
     :name name
     :tags tags
     :category category
     :parent-heading parent-heading)))
     
