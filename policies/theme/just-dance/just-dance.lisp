;;;; isimple.lisp

(defpackage #:arblog.theme.just-dance
  (:use #:cl #:iter #:arblog.policy.theme
        #:arblog.theme.common)
  (:export #:instance))

(in-package #:arblog.theme.just-dance)

(defclass instance (theme-with-templates)
  ((templates-package :initform '#:arblog.theme.just-dance.tmpl)
   (static-prefix :initform "static/just-dance")))

(arblog:register-theme-static-dir
 "just-dance"
 (merge-pathnames "static/" (asdf:component-pathname  (asdf:find-system '#:arblog-theme-just-dance))))

(defmacro define-jd-method (method (&rest args) &body body)
  (alexandria:with-unique-names (inst)
    `(define-theme-method (,inst instance) ,method ,args ,@body)))

(define-jd-method theme-list-recent-posts (posts navigation)
  (render-template all-posts
    (list :new-entry (new-entry-url)
          :posts (mapcar 'prepare-post-data posts)
          :disqus (list :enabled arblog:*disqus-enabled*
                        :shortname arblog:*disqus-shortname*)
          :navigation navigation)))


(define-jd-method theme-archive-for-year (year months)
  (render-template archive-for-year
    (list :year year
          :months (iter (for month in months)
                        (collect (archive-for-month-link year month))))))

(define-jd-method theme-archive-for-month (year month posts)
  (render-template archive-for-month
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (month-name month))))

(define-jd-method theme-archive-for-day (year month day posts)
  (render-template archive-for-day
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (month-name month)
          :day day)))


(define-jd-method theme-one-post (post)
  (let ((id (gethash "_id" post)))
    (render-template one-post
      (list* :new-entry (new-entry-url)
             :disqus (list :shortname arblog:*disqus-shortname*
                           :developer-mode arblog:*disqus-developer-mode*
                           :enabled arblog:*disqus-enabled*
                           :identifier id
                           :permalink (restas:genurl* 'arblog.public::post-permalink :id id))
             (prepare-post-data post)))))


;;;; Tags

;(define-jd-method theme-all-tags (tags)
;  (render-template tags-page
;    (list :tags
;          (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
;                (collect (list :href (restas:genurl 'arblog.public::posts-with-tag
;                                                    :tag tag)
;                               :name tag))))))

(define-jd-method theme-posts-with-tag (tag posts navigation)
  (render-template posts-with-tag
    (list :tag tag
          :atom-feed-href (restas:genurl 'arblog.public::posts-with-tag-feed :tag tag)
          :navigation navigation
          :posts (mapcar 'prepare-post-data posts))))

;;;; Admin

(define-jd-method theme-admin-posts (posts navigation)
  (arblog.internal.theme:render.list-recent-posts posts navigation))
;  (render-template all-posts
;    (list :posts (iter (for post in posts)
;                       (collect (list :id (gethash "_id" post)
;                                      :title (gethash "title" post)
;                                      :href (restas:genurl 'arblog.admin::edit-post :id (gethash "_id" post))
;                                      :published (render-published (gethash "published" post)))))
;          :navigation navigation
;          :create-post-href (restas:genurl 'arblog.admin::create-post))))

(define-jd-method theme-admin-edit-post (&key title markup tags preview)
  (render-template admin-edit-post-page
    (list :post (list :title title
                      :markup markup
                      :tags (iter (for tag in tags)
                                  (collect (list :name tag))))
          :preview preview)))

