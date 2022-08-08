
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


;;;  ____                 _ _       
;;; |  _ \ ___  ___ _   _| | |_ ___ 
;;; | |_) / _ \/ __| | | | | __/ __|
;;; |  _ <  __/\__ \ |_| | | |_\__ \
;;; |_| \_\___||___/\__,_|_|\__|___/
                                
 (:filename
  "/home/fnaufel/Development/00-Present/clocker/scratch/example1.org"
  :clocks
  ((
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
    :parent-heading 2)
   (
    :year-start 2022 
    :month-start 4 
    :day-start 2 
    :hour-start 15 
    :minute-start 39 
    :year-end 2022 
    :month-end 4 
    :day-end 2 
    :hour-end 16 
    :minute-end 12 
    :parent-heading 357)
   (
    :year-start 2022 
    :month-start 4 
    :day-start 3 
    :hour-start 16 
    :minute-start 26 
    :year-end 2022 
    :month-end 4 
    :day-end 3 
    :hour-end 16 
    :minute-end 27 
    :parent-heading 621))
  
  :headings nil)

;;;;;;

 (
  :filename "/home/fnaufel/Development/00-Present/clocker/scratch/example1.org" 
  :clocks
  ((
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
    :parent-heading 2)
   (
    :year-start 2022 
    :month-start 4 
    :day-start 2 
    :hour-start 15 
    :minute-start 39 
    :year-end 2022 
    :month-end 4 
    :day-end 2 
    :hour-end 16 
    :minute-end 12 
    :parent-heading 357)
   (
    :year-start 2022 
    :month-start 4 
    :day-start 3 
    :hour-start 16 
    :minute-start 26 
    :year-end 2022 
    :month-end 4 
    :day-end 3 
    :hour-end 16 
    :minute-end 27 
    :parent-heading 621))
  
  :headings
  ((
    :begin 2 
    :name "[#10] First" 
    :category "example1" 
    :tags
    ("python" "ubuntu" "emacs")
    
    :parent-heading nil)
   (
    :begin 262 
    :name "[0/3] First, first child" 
    :category "example1" 
    :tags nil 
    :parent-heading 2)
   (
    :begin 357 
    :name "First, second child" 
    :category "example1" 
    :tags nil 
    :parent-heading 2)
   (
    :begin 600 
    :name "Second" 
    :category "example1" 
    :tags nil 
    :parent-heading nil)
   (
    :begin 610 
    :name "Third" 
    :category "example1" 
    :tags nil 
    :parent-heading nil)
   (
    :begin 621 
    :name "This is an inline task" 
    :category "example1" 
    :tags nil 
    :parent-heading 621)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (headline (
            :raw-value "[#10] First" 
            :begin 24 
            :end 590 
            :pre-blank 0 
            :contents-begin 102 
            :contents-end 589 
            :level 1 
            :priority nil 
            :tags (
                   #("python"
                     0 1
                     (org-category
                      "buffercat"
                      display #(" p" 1 2 (cursor t))
                      keymap
                      (keymap (follow-link . mouse-face)
                              (mouse-3 . org-find-file-at-mouse)
                              (mouse-2 . org-open-at-mouse))
                      mouse-face highlight face org-modern-tag fontified t)
                     1 5
                     (org-category "buffercat" keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t) 5 6 (org-category "buffercat" display "n " keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t)) #("ubuntu" 0 1 (org-category "buffercat" display #(" u" 1 2 (cursor t)) keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t) 1 5 (org-category "buffercat" keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t) 5 6 (org-category "buffercat" display "u " keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t)) #("emacs" 0 1 (org-category "buffercat" display #(" e" 1 2 (cursor t)) keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t) 1 4 (org-category "buffercat" keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t) 4 5 (org-category "buffercat" display "s " keymap (keymap (follow-link . mouse-face) (mouse-3 . org-find-file-at-mouse) (mouse-2 . org-open-at-mouse)) mouse-face highlight face org-modern-tag fontified t))) 
            :todo-keyword #("STARTED" 0 7 (fontified t face (org-todo org-level-1) org-category "buffercat")) 
            :todo-type todo 
            :post-blank 1 
            :footnote-section-p nil 
            :archivedp nil 
            :commentedp nil 
            :post-affiliated 24 
            :title "[#10] First"))


 
 ;;;;;


 (#("python"
    0 1
    (org-category "buffercat" display
                  #(" p" 1 2
                    (cursor t))
                  keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)
    1 5
    (org-category "buffercat" keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)
    5 6
    (org-category "buffercat" display "n " keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t))
  #("ubuntu" 0 1
    (org-category "buffercat" display
                  #(" u" 1 2
                    (cursor t))
                  keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)
    1 5
    (org-category "buffercat" keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)
    5 6
    (org-category "buffercat" display "u " keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t))
  #("emacs" 0 1
    (org-category "buffercat" display
                  #(" e" 1 2
                    (cursor t))
                  keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)
    1 4
    (org-category "buffercat" keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)
    4 5
    (org-category "buffercat" display "s " keymap
                  (keymap
                   (follow-link . mouse-face)
                   (mouse-3 . org-find-file-at-mouse)
                   (mouse-2 . org-open-at-mouse))
                  mouse-face highlight face org-modern-tag fontified t)))
