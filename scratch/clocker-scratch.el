
;; Provides function to export current org buffer as JSON structure
;; to $file.org.json. Adapted from an org-mode mailing post by
;; Brett Viren: https://lists.gnu.org/archive/html/emacs-orgmode/2014-01/msg00338.html
(require 'json)

;; original
(defun org-export-json ()
  (interactive)
  (let* ((tree (org-element-parse-buffer 'element nil)))
    (org-element-map tree
        '(headline inlinetask)
      (lambda (x)
        (if (org-element-property :parent x)
            (org-element-put-property x :parent "none"))))
    (write-region
     (json-encode tree)
     nil (concat (buffer-file-name) ".json"))))


;; only-greater elements
(defun org-export-json ()
  (interactive)
  (let* ((tree (org-element-parse-buffer 'greater-element nil)))
    (org-element-map tree
        '(headline inlinetask)
      (lambda (x)
        (if (org-element-property :parent x)
            (org-element-put-property x :parent "none"))))
    (write-region
     (json-encode tree)
     nil (concat (buffer-file-name) ".json"))))


;;;  __  __                          _        _           
;;; |  \/  | __ _ _ __     ___ _ __ | |_ _ __(_) ___  ___ 
;;; | |\/| |/ _` | '_ \   / _ \ '_ \| __| '__| |/ _ \/ __|
;;; | |  | | (_| | |_) | |  __/ | | | |_| |  | |  __/\__ \
;;; |_|  |_|\__,_| .__/   \___|_| |_|\__|_|  |_|\___||___/
;;;              |_|                                      

;; Use org-map-entries instead of parsing
;; Using properties
(defun clocker-get-headings ()
  ""

  (let* ((org-trust-scanner-tags t)
         (props (org-entry-properties))
         (pos (point))
         (id (concat (cdr (assoc "FILE" props)) ":" (number-to-string pos)))
         (tags (if-let (contents (cdr (assoc "ALLTAGS" props)))
                   (progn
                     (set-text-properties 0 (length contents) nil contents)
                     contents)
                 ""))
         )

    (list
     `("id" . ,id)
     `("name" . ,(cdr (assoc "ITEM" props)))
     `("category" . ,(cdr (assoc "CATEGORY" props)))
     `("tags" . ,tags)
    )))

     

;;;                                  _            __  __           
;;;  _ __   __ _ _ __ ___  ___      | |__  _   _ / _|/ _| ___ _ __ 
;;; | '_ \ / _` | '__/ __|/ _ \_____| '_ \| | | | |_| |_ / _ \ '__|
;;; | |_) | (_| | |  \__ \  __/_____| |_) | |_| |  _|  _|  __/ |   
;;; | .__/ \__,_|_|  |___/\___|     |_.__/ \__,_|_| |_|  \___|_|   
;;; |_|                                                            

;; Called for each headline
(defun clocker-get-headings (hd)
  ""

  (let* 
      ((pos (org-element-property :begin hd))
       (id (concat (buffer-file-name) ":" (number-to-string pos)))
       (name (org-element-property :raw-value hd))
       (category (org-get-category))
       (tags-contents (org-element-property :tags hd))
       (tags 
        (progn
          (mapc
           (lambda (s) (set-text-properties 0 (length s) nil s))
           tags-contents)
          tags-contents))
       (parent (org-element-property :begin (car (org-element-lineage hd))))
       (logbook )
       )

    `(:id ,id
         :name ,name
         :category ,category
         :tags ,tags
         :parent ,parent)
    )
  )


(org-element-map
    (org-element-parse-buffer 'greater-element)
    '(headline inlinetask)
  #'clocker-get-headings)

(org-element-map
    (org-element-parse-buffer 'greater-element)
    '(headline inlinetask)
  (lambda (x) (mapcar #'org-element-type (org-element-lineage x '('headline)))))

(org-element-map
    (org-element-parse-buffer)
    '(headline inlinetask drawer clock)
  #'org-element-type)
  

;;;             _          _            _        
;;;   __ _  ___| |_    ___| | ___   ___| | _____ 
;;;  / _` |/ _ \ __|  / __| |/ _ \ / __| |/ / __|
;;; | (_| |  __/ |_  | (__| | (_) | (__|   <\__ \
;;;  \__, |\___|\__|  \___|_|\___/ \___|_|\_\___/
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
        (parent-heading (org-element-property :begin heading)))

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
       :parent-heading parent-heading))

    )
  
  )


(org-element-map
    (org-element-parse-buffer)
    '(clock)
  #'clocker-get-clock-info)

;;; results: the value of each clock is a timestamp

;; if clock is open but not closed, I get:
;; type does not contain "range"
;; Careful: end == start

;; No need: I will only get closed clocks

((:value
  (timestamp
   (:type inactive-range
          :raw-value "[2022-04-02 Sat 15:38]--[2022-04-02 Sat 15:39]"
          :year-start 2022
          :month-start 4
          :day-start 2
          :hour-start 15
          :minute-start 38
          :year-end 2022
          :month-end 4
          :day-end 2 
          :hour-end 15 
          :minute-end 39 
          :begin 133 
          :end 180 
          :post-blank 1))
  :parent 2)
