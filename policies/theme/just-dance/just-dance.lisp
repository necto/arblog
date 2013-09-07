;;;; isimple.lisp

(defpackage #:arblog.theme.just-dance
  (:use #:cl #:iter #:arblog.policy.theme
        #:gallery.policy.render #:gallery.content
        #:arblog.theme.common)
  (:export #:instance))

(in-package #:arblog.theme.just-dance)

(defclass instance (theme-with-templates)
  ((templates-package :initform '#:arblog.theme.just-dance.tmpl)))

(arblog:register-theme-static-dir
 "just-dance"
 (merge-pathnames "static/" (asdf:component-pathname  (asdf:find-system '#:arblog-theme-just-dance))))

(defmacro define-jd-method (method (&rest args) &body body)
  `(define-theme-method instance "/static/just-dance" ,method ,args ,@body))

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
          :month (svref local-time:+month-names+ month))))

(define-jd-method theme-archive-for-day (year month day posts)
  (render-template archive-for-day
    (list :posts (mapcar 'prepare-post-data posts)
          :year year
          :month (svref local-time:+month-names+ month)
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
  (render.list-recent-posts posts navigation))
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

;;;; Gallery

(define-jd-method theme.album-list (add-album-url del-album-url albums)
  (render-template gallery-albums
    (list :albums (iter (for album in albums)
                        (collect (draw-preview album)))
          :new-album add-album-url
          :del-album del-album-url)))

(define-jd-method theme.add-pic (form album)
  (render-template gallery-add-pic
    (list :add-pic-form form
          :action (restas:genurl 'gallery:receive-pic)
          :album album)))

(define-jd-method theme.add-album (form)
  (render-template gallery-add-album
    (list :add-pic-form form
          :action (restas:genurl 'gallery:receive-album))))

(define-jd-method theme.view-album (add-pic-url rem-pic-url album)
  (render-template gallery-view-album
    (list :album-title (item-title album)
          :album-comment (item-comment album)
          :add-pic-url add-pic-url
          :rem-pic-url rem-pic-url
          :pictures (iter (for pic in (album-items album))
                          (collect (draw-preview pic))))))

