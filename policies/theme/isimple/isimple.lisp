;;;; isimple.lisp

(defpackage #:arblog.theme.isimple
  (:use #:cl #:iter #:arblog.policy.theme
        #:arblog.theme.common)
  (:export #:arblog-isimple-theme))

(in-package #:arblog.theme.isimple)

(defclass arblog-isimple-theme (theme-with-templates)
  ((templates-package :initform '#:arblog.theme.isimple.tmpl)
   (static-prefix :initform "static/isimple")))

(arblog:register-theme-static-dir
 "isimple"
 (merge-pathnames "static/" (asdf:component-pathname  (asdf:find-system '#:arblog-theme-isimple))))


(defmacro define-isimple-method (method (&rest args) &body body)
  (alexandria:with-unique-names (inst)
  `(define-theme-method (,inst arblog-isimple-theme) ,method ,args ,@body)))

(define-isimple-method theme-list-recent-posts (posts navigation)
  (render-template show-all-blog-post
    (list :new-entry (new-entry-url)
          :posts (mapcar 'prepare-post-data posts)
          :disqus (list :enabled arblog:*disqus-enabled*
                        :shortname arblog:*disqus-shortname*)
          :navigation navigation)))

(define-isimple-method theme-archive-for-year (year months)
  (render-template archive-for-year
    (list :year year
          :months (iter (for month in months)
                        (collect (archive-for-month-link year month))))))

(define-isimple-method theme-archive-for-month (year month posts)
  (render-template archive-for-month
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (month-name month))))

(define-isimple-method theme-archive-for-day (year month day posts)
  (render-template archive-for-day
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (month-name month)
          :day day)))

(define-isimple-method theme-one-post (post)
  (let ((id (gethash "_id" post)))
    (render-template show-one-post
      (list* :new-entry (new-entry-url)
             :disqus (list :shortname arblog:*disqus-shortname*
                           :developer-mode arblog:*disqus-developer-mode*
                           :enabled arblog:*disqus-enabled*
                           :identifier id
                           :permalink (restas:genurl* 'arblog.public::post-permalink :id id))
             (prepare-post-data post)))))


;;;; Tags

(define-isimple-method theme-all-tags (tags)
  (render-template tags-page
    (list :tags
          (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
                (collect (list :href (restas:genurl 'arblog.public::posts-with-tag
                                                    :tag tag)
                               :name tag))))))

(define-isimple-method theme-posts-with-tag (tag posts navigation)
  (render-template post-with-tag-page
    (list :tag tag
          :atom-feed-href (restas:genurl 'arblog.public::posts-with-tag-feed :tag tag)
          :navigation navigation
          :posts (mapcar 'prepare-post-data posts))))

;;;; Admin

(defun render-published (published)
  (format nil
          "~A.~A.~A"
         (local-time:timestamp-year published)
         (local-time:timestamp-month published)
         (local-time:timestamp-day published)))

(define-isimple-method theme-admin-posts (posts navigation)
  (render-template admin-post-page
    (list :posts (iter (for post in posts)
                       (collect (list :id (gethash "_id" post)
                                      :title (gethash "title" post)
                                      :href (restas:genurl 'arblog.admin::edit-post :id (gethash "_id" post))
                                      :published (render-published (gethash "published" post)))))
          :navigation navigation
          :create-post-href (restas:genurl 'arblog.admin::create-post))))

(define-isimple-method theme-admin-edit-post (&key title markup tags preview)
  (render-template admin-edit-post-page
    (list :post (list :title title
                      :markup markup
                      :tags (iter (for tag in tags)
                                  (collect (list :name tag))))
          :preview preview)))

