
(defpackage #:arblog.theme.common
  (:use #:cl #:iter #:arblog.policy.theme
        #:gallery.policy.render #:gallery.content)
  (:export #:theme-with-templates
           #:recent-posts-widget
           #:tags-widget
           #:archive-for-year-link
           #:archive-for-month-link
           #:admin-session
           #:new-entry-url
           #:archive-for-day-link
           #:prepare-post-data
           #:render-template
           #:templates-package
           #:define-theme-method))

(in-package #:arblog.theme.common)

(defclass theme-with-templates (gallery.default-render::handler)
  ((templates-package :initarg :templates-package
                      :reader theme-templates-package)))

(defun recent-posts-widget ()
  (iter (for item in (arblog.internal.datastore:ds.list-recent-posts 0 10 :fields '("title")))
        (collect
            (list :title (gethash "title" item)
                  :url (restas:genurl 'arblog::-public-.post-permalink :id (gethash "_id" item))))))


(defun tags-widget ()
  (let ((tags (arblog.internal.datastore:ds.all-tags)))
    (iter (for tag in (sort (copy-list tags) #'string< :key #'string-downcase))
          (collect (list :href (restas:genurl 'arblog::-public-.posts-with-tag
                                              :tag tag)
                         :name tag)))))

(defun archive-for-year-link (year)
  (list :title year
        :href (restas:genurl 'arblog.public::archive-for-year
                             :year year)))

(defun archive-for-month-link (year month)
  (list :title (svref local-time:+month-names+ month)
        :href (restas:genurl 'arblog.public::archive-for-month
                             :year year
                             :month (format nil "~2,'0D" month))))

(defun admin-session ()
  (hunchentoot:session-value :admin-session))

(defun new-entry-url ()
  (when (admin-session)
    (restas:genurl 'arblog::-admin-.create-post)))

(defun archive-for-day-link (year month day)
  (list :title day
        :href (restas:genurl 'arblog.public::archive-for-day
                             :year year
                             :month (format nil "~2,'0D" month)
                             :day (format nil "~2,'0D" day))))

(defun prepare-post-data (post)
  (let* ((published (gethash "published" post))
         (year (local-time:timestamp-year published))
         (month (local-time:timestamp-month published))
         (day (local-time:timestamp-day published)))
    (list :id (gethash "_id" post)
          :title (gethash "title" post)
          :href (restas:genurl 'arblog.public::one-post
                               :year year
                               :month (format nil "~2,'0D" month)
                               :day (format nil "~2,'0D" day)
                               :urlname (gethash "urlname" post))
          :content (gethash "content" post)
          :markup (gethash "markup" post)
          :all-tags-href (restas:genurl 'arblog.public::all-tags)
          :tags (iter (for tag in (gethash "tags" post))
                      (collect
                          (list :name tag
                                :href (restas:genurl 'arblog.public::posts-with-tag :tag tag))))
          :edit (when (admin-session)
                  (restas:genurl 'arblog::-admin-.edit-post :id (gethash "_id" post)))
          :published (local-time:format-rfc1123-timestring nil published))))

(defmacro define-theme-method (theme static-dir method (&rest args) &body body)
  (alexandria:with-unique-names (tmplname tmplargs inst)
    `(defmethod ,method ((,inst ,theme) ,@args)
       (macrolet ((render-template (,tmplname &body ,tmplargs)
                    `(closure-template:ttable-call-template
                      (closure-template:package-ttable (theme-templates-package ,',inst))
                      (string ',,tmplname)
                      (let ((is-not-admin (not (string= (string (slot-value restas:*module* 'restas::package))
                                                        "ARBLOG.ADMIN"))))
                        (list* :index-url (restas:genurl (if is-not-admin
                                                             'arblog::-public-.entry
                                                             'arblog::-admin-.entry))
                               :static ,,static-dir
                               :login-url (restas:genurl 'arblog::-admin-.entry)
                               :rss-url "/feed/rss"
                               :gallery-url (restas:genurl 'arblog::-gallery-.main)
                               :blog-name arblog:*blog-name*
                               :blog-author arblog:*blog-author*
                               :recent-posts (if is-not-admin (recent-posts-widget))
                               :all-tags (if is-not-admin (tags-widget))
                               ,@,tmplargs)))))
         ,@body))))


