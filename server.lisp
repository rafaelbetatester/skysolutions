
(ql:quickload :hunchentoot)
(ql:quickload :parenscript)
(ql:quickload :cl-who)
(ql:quickload :cl-json)
(ql:quickload :cl-mongo)

(defpackage :server
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))

(in-package :server)


(defclass user ()
  ((login :reader login
	  :initarg :login)
   (password :reader password
	     :initarg :password)))

(defclass mensagem()
  ((escopo :reader escopo
          :initarg  :escopo)
   (remetente :reader remetente
	      :initarg :remetente)
    (destinatario :reader destinatario
		  :initarg :destinatario)
   (id :accessor id
       :initform (gensym))))
  
(defmethod print-object ((object mensagem) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (escopo remetente destinatario id) object
      (format stream "mensagem: ~s do remetente ~s para o destinatário ~s com ~a" escopo remetente destinatario id))))

(defvar *user-database* '())
(defvar *mensagem-database* '())

(defun user-from-login (login)
  (find login *user-database* :test #'string-equal :key #'login))

(defun add-user (login password)
  (push (make-instance 'user 
		       :login login
		       :password password) *user-database*))

(defun mensagem-from-id (id)
  (find id *mensagem-database* :test #'string-equal :key  #'id))

(defun add-mensagem (escopo remetente destinatario)
  (let* ((nova-mensagem (make-instance 'mensagem 
				       :escopo escopo 
				       :remetente remetente 
				       :destinatario destinatario)))
    (push nova-mensagem *mensagem-database*)))
;;    (publish-mensagem nova-mensagem)))


(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/logo.jpg" "static/Commodore64.jpg") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/retro.css" "static/retro.css") *dispatch-table*))

(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro; 
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/retro.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.jpg"
                        :alt "Commodore 64"
                        :class "logo")
                  (:span :class "strapline"
                         "Dicionário de Variáveis"))
            ,@body))))


 (define-easy-handler (loggin :uri "/loggin") ()
  (standard-page (:title "Login")
    (:h1 "Entre com Login e Senha!")
    (:form :action "/login-inserted" :method "get" :id "addform"
	   (:p "Login" (:br)
	       (:input :type "text" :name "login" :class "txt"))
	   (:p "Senha" (:br)
	       (:input :type "text" :name "password" :class "txt"))
	   (:p (:input :type "submit" :value "Entrar" :class "btn")))))
     

(define-easy-handler (listmessages :uri "/listmessages") (login)
  (standard-page (:title "Sky App")
     (:h1 "Defina as Variáveis do Problema!")
     (:p "Envie uma mensagem " (:a :href (format nil "new-message?login=~a" login) "aqui"))
     (:h2 "Current stand")
     (:div :id "chart" ; Used for CSS styling of the links.
       (:ol
	(dolist (mensagens *mensagem-database*)
	  (if (equal login (destinatario mensagens))
	      (htm
	       (:li (format t "~a from ~a to ~a" (escopo mensagens) 
			    (remetente mensagens) (destinatario mensagens))))))))))

(define-easy-handler (new-message :uri "/new-message") (login)
  (standard-page (:title "Envie uma nova mensagem")
    (:h1 "Adicione uma nova mensagem")
    (:form :action  "/message-added" :method "get" :id "addform"
	   (:input :type "text" :name "login" :value login)
	   (:p "Mensagem" (:br)
	       (:input :type "text" :name "escopo" :class "txt"))
	   (:p "Destinatário" (:br)
	       (:input :type "text" :name "destinatario" :class "txt"))
	   (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (login-inserted :uri "/login-inserted") (login password)
  (let*((usuario (user-from-login login)))
    (if (or (null usuario) (not (equal password (password usuario))))
	(redirect "/loggin")
	(redirect (format nil "/listmessages?login=~a" login)))))


(define-easy-handler (message-added :uri "/message-added") (login escopo destinatario)
 ;; (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
  ;;print login))
  (add-mensagem escopo login destinatario)
  (redirect (format nil "/listmessages?login=~a" login))) ; back to the front page



(start-server 8080)
