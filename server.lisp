
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
	     :initarg :password)
   (group-list :accessor group-list
	       :initarg :group-list
	       :initform nil)
   (image :reader image
	  :accessor image
	  :initarg :image
	  :initform (concatenate 'string (write-to-string (1+ (random 10))) ".jpeg"))))

(defclass group ()
  ((name :reader name
	 :initarg :name)
   (user-list :accessor user-list
	      :initarg :user-list
	      :initform nil)))

(defclass mensagem()
  ((escopo :reader escopo
          :initarg  :escopo)
   (remetente :reader remetente
	      :initarg :remetente)
    (destinatario :reader destinatario
		  :initarg :destinatario)
   (time :accessor tempo
	 :initform (get-universal-time))
   (id :accessor id
       :initform (gensym))))
  
(defmethod print-object ((object mensagem) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (escopo remetente destinatario id) object
      (format stream "mensagem: ~s do remetente ~s para o destinatário ~s com ~a" escopo remetente destinatario id))))


(defvar *user-database* '())
(defvar *mensagem-database* '())
(defvar *group-database* '())

(defun user-from-login (login)
  (find login *user-database* :test #'string-equal :key #'login))

(defun add-user (login password)
  (let*((novo (make-instance 'user 
		       :login login
		       :password password)))
    (push novo *user-database*)
    (add-user-to-group login "Bem-Vindo")))
    
(defun add-admin-user (login password)
  (let*((novo (make-instance 'user 
		       :login login
		       :password password)))
    (push novo *user-database*)))


(defun add-group (login name)
  (push (make-instance 'group
		       :name name
		       :user-list (list login)) *group-database*)
  (let*((alist (group-list (user-from-login login))))
    (setf (group-list (user-from-login login)) (append alist (list name)))))
  

(defun group-from-name (name)
  (find name *group-database* :test #'string-equal :key #'name))

(defun add-user-to-group (login name)
  (let*((alist (user-list (group-from-name name))))
    (setf (user-list (group-from-name name)) (append alist (list login))))
  (let*((alist (group-list (user-from-login login))))
    (setf (group-list (user-from-login login)) (append alist (list name)))))
  
    
(defun mensagem-from-id (id)
  (find id *mensagem-database* :test #'string-equal :key  #'id))

(defun add-mensagem (escopo remetente destinatario)
  (let* ((nova-mensagem (make-instance 'mensagem 
				       :escopo escopo 
				       :remetente remetente 
				       :destinatario destinatario)))
    (push nova-mensagem *mensagem-database*)))
;;    (publish-mensagem nova-mensagem)))


(defun open-time (tempo)
   (multiple-value-bind (second minute hour)
       (decode-universal-time tempo)
     (format t "(~2,'0D:~2,'0D) " hour minute)))

(add-admin-user "Admin" "1234")
(add-group "Admin" "Bem-Vindo")


(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/logo.png" "./chat-box/assets/img/logo.png") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
	 "/bootstrap.css" "./chat-box/assets/css/bootstrap.css") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
	 "/font-awesome.css" "./chat-box/assets/css/font-awesome.css") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	 "/style.css" "./chat-box/assets/css/style.css") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	 "/bootstrap.js" "./chat-box/assets/js/bootstrap.js") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	 "/jquery-1.11.1.js" "./chat-box/assets/js/jquery-1.11.1.js") *dispatch-table*)
     (push (create-static-file-dispatcher-and-handler
	   "/1.jpeg" "./chat-box/assets/img/1.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/2.jpeg" "./chat-box/assets/img/2.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/3.jpeg" "./chat-box/assets/img/3.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/4.jpeg" "./chat-box/assets/img/4.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/5.jpeg" "./chat-box/assets/img/5.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/6.jpeg" "./chat-box/assets/img/6.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/7.jpeg" "./chat-box/assets/img/7.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/8.jpeg" "./chat-box/assets/img/8.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	   "/9.jpeg" "./chat-box/assets/img/9.jpeg") *dispatch-table*)
        (push (create-static-file-dispatcher-and-handler
	   "/10.jpeg" "./chat-box/assets/img/10.jpeg") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	 "/jquery.js" "./chat-box/assets/js/jquery.js") *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler
	 "/refreshing.php" "./chat-box/assets/php/refreshing.php") *dispatch-table*))
    
    

   
           


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
	    (:meta :name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1")
            (:title ,title)
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/bootstrap.css")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/font-awesome.css")
           
	    (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/style.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body
	    (:script :src "/jquery-1.11.1.js")
            (:script :src "/bootstrap.js")
	   ;; (:div :id "header" ; Retro games header
              ;;    (:img :style "float:right" 
	;;	   :src "/logo.png"
		;;   :alt "Lisp Logo"
		;;   :class "logo"))
            ,@body))))
(defmacro standard-refresh ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro; 
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
        ;;    (:meta :charset "utf-8")
	   ;; (:meta :name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1")
         ;;   (:title ,title)
           ;; (:link :type "text/css"
                  ; :rel "stylesheet"
                  ; :href "/style.css")
            ;;(:link :type "text/css"
              ;;     :rel "stylesheet"
                ;;   :href "/font-awesome.css")
          ;;  ,(when script
           ;;    `(:script :type "text/javascript"
            ;;             (str ,script))))
          ;; (:body
	  ;;  (:script :src "/bootstrap.js")
	   ;; (:script :src "/jquery-1.11.1.js")
           ; (:div :id "header" ; Retro games header
            ;;     )
            ,@body))))

(defmacro standard-main-page ((&key title script) &body body)
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
            (:meta :http-equiv "refresh" :content "2")
	    (:title ,title)
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/retro.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.png"
                        :alt ""
                        :class "logo"))
                   ,@body))))

 (define-easy-handler (loggin :uri "/login") ()
  (standard-page (:title "Login")
		 (:h1 "Entre com Login e Senha!")
    (:form :action "/login-inserted" :method "get" :id "addform"
	   (:p "Login" (:br)
	       (:input :type "text" :name "login" :class "txt"))
	   (:p "Senha" (:br)
	       (:input :type "password" :name "password" :class "txt"))
	   (:p (:input   :type "submit" :value "Entrar" :class "btn")))))

 (define-easy-handler (login-admin :uri "/login-admin") ()
  (standard-page (:title "Login")
		 (:h1 "Entre com Login e Senha!")
    (:form :action "/login-admin-inserted" :method "get" :id "addform"
	   (:p "Login" (:br)
	       (:input :type "text" :name "login" :class "txt"))
	   (:p "Senha" (:br)
	       (:input :type "password" :name "password" :class "txt"))
	   (:p (:input :type "submit" :value "Entrar" :class "btn")))))



(define-easy-handler (listgroups :uri "/listgroups") (login)
  (standard-main-page (:title "Sky App")
     ;(:h1 "Bem vindo ao SkyApp!")
     ;;(:p "Envie uma mensagem " (:a :href (format nil "new-message?login=~a" login) "aqui"))
     ;(:h2 "Seus Projetos")
     (:div :id "chart" ; Used for CSS styling of the links.
       (:ol
	(dolist (groups *group-database*)
	  (if (member (string login) (user-list groups) :test #'string-equal)
	      (htm
	       (:a :href (format nil "/show-group?name=~a&login=~a" (name groups) login) (format t "~a~%" (name groups))))))))))




(define-easy-handler (show-group :uri "/show-group") (name login)
  (standard-page (:title "Sky App")
     ;(:h1 "Bem vindo ao SkyApp!")
    ; (:p "Envie uma mensagem para este grupo" (:a :href (format nil "new-message?login=~a&group=~a" login name) "aqui"))
 ; (:h2 (format t "~a" name))
  ;;(:meta :http-equiv "refresh" :content "2"
  
  (:div :class "container" :id "chart" ; Used for CSS styling of the links.
	(:div :class "row pad-top pad-bottom"
	      (:div :class " col-lg-6 col-md-6 col-sm-6" 
		    (:div :class "chat-box-div" 
			  (:div :class "chat-box-head"
				(format t "~a" name))
	 		  (:script (format t "
 $(document).ready(function(){
$('*').css('margin','0');
$('*').css('padding','0');
      $(\"#scroll-table\").load(\"/refresh?name=~a&login=~a\");
      setInterval(function(){
        $(\"#scroll-table\").load(\"/refresh?name=~a&login=~a\");
      }, 1000);
    });
function sendMessage(){
   jQuery.ajax('/message-added?login=~a&group=~a&escopo='+$(\"#mensagem\").val(),{
      success: function(data){
         update();
      }
   });
   $(\"#mensagem\").val('');
   $('#scroll-table').scrollTop(324234234);
   return false; 
}
"name login name login login name))
			  (:ol :id "scroll-table" 
			       (dolist (mensagens (reverse *mensagem-database*))
				 (if (equal name (destinatario mensagens))
				     (htm
				      ;;(:div :class "panel-body chat-box-main"
				      ;;(string (escopo mensagens)))
				      (:div :class "chat-box-name-left"
					    ;; (:img :src "./chat-box/assets/img/user.png" :alt "bootstrap Chat box user image" :class "img-circle")
						     (format t "~a: ~a" (remetente mensagens) (escopo mensagens)
							     (open-time (tempo mensagens)))))))))
		    (:div :class "chat-box-footer"
			  (:div :class "input-group"
				(:form :style "width:100%"  :onsubmit "return sendMessage()" :id "addform"
				       (:input :type "hidden" :name "login" :value login)
				       (:input :type "hidden" :name "group" :value name)	
				       (:input :style "width:100%" :id  "mensagem" :type "text" :class "form-control" :name "escopo" :placeholder "Digite sua mensagem...")
				       (:span :class "input-group-btn"
					      (:button :class "btn btn-default"  :type "submit" (format t "~a" "Enviar")))))))
		    (:div :class "col-lg-3 col-md-3 col-sm-3"
			  (:div :class "chat-box-online-div"
				(:div :class "chat-box-online-head"
				      (format t "Membros do Projeto (~a)" (length (user-list (group-from-name name)))))
				
				(:script (format t " $(document).ready(function(){
         $(\"#scroll-table-2\").load(\"/refresh-group?name=~a&login=~a\");
      setInterval(function(){
        $(\"#scroll-table-2\").load(\"/refresh-group?name=~a&login=~a\");
      }, 3000);
    });"name login name login))
	
				(:ol :id "scroll-table-2"
				     (dolist (users (user-list (group-from-name name)))
				       (htm
					(:div :class "panel-body chat-box-online"
					      (:div :class "chat-box-online-left"
						    (:img :src (format nil "~a" (image (user-from-login users))) :class "img-circle")
						    ;; (:br)
						    (format t " ~a" users)))))))
			  (:div :class "chat-box-footer"
			  (:div :class "input-group"
				(:form :action   "/contact-added" :method "get" :id "addform"
				       (:input :type "hidden" :name "login" :value login)
				       (:input :type "hidden" :name "group" :value name)	
				       (:input :type "text" :class "form-control" :name "novo" :placeholder "Novo Contato...")
				       (:span :class "input-group-btn"
					      (:button :class "btn btn-default" :type "submit" (format t "~a" "Adicionar")))))))
		    
		    (:div :class "col-lg-3 col-md-3 col-sm-3"
			  (:div :class "chat-box-new-div"
				(:div :class "chat-box-new-head"
				      (format t "Projetos (~a)" (length (group-list (user-from-login login)))))
						
				(:script (format t " $(document).ready(function(){
         $(\"#scroll-table-3\").load(\"/refresh-group-list?name=~a&login=~a\");
      setInterval(function(){
        $(\"#scroll-table-3\").load(\"/refresh-group-list?name=~a&login=~a\");
      }, 3000);
    });"name login name login))
	
				(:ol :id "scroll-table-3"
				     (dolist (groups (group-list (user-from-login login)))
				       (htm
					(:div :class "panel-body chat-box-new"
					      (:a :href (format nil "/show-group?name=~a&login=~a" groups login) (format t "~a" groups)))))))
			    (:div :class "chat-box-footer"
			  (:div :class "input-group"
				(:form :action   "/group-added" :method "get" :id "addform"
				       (:input :type "hidden" :name "login" :value login)
				       ;(:input :type "hidden" :name "group" :value name)	
				       (:input :type "text" :class "form-control" :name "novo" :placeholder "Novo Projeto...")
				       (:span :class "input-group-btn"
					      (:button :class "btn btn-default" :type "submit" (format t "~a" "Criar")))))))))))
		    
					
		

(define-easy-handler (refresh :uri "/refresh") (name login)
  (standard-refresh (:title "SkyApp")  
					   (dolist (mensagens (reverse *mensagem-database*))
	 (if (equal name (destinatario mensagens))
	     (htm
	      ;;(:div :class "panel-body chat-box-main"
	      ;;(string (escopo mensagens)))
	      (:div :class "chat-box-name-left"
		    ;; (:img :src "./chat-box/assets/img/user.png" :alt "bootstrap Chat box user image" :class "img-circle")
		    (format t "~a: ~a" (remetente mensagens) (escopo mensagens)
			    (open-time (tempo mensagens)))))))))
				
(define-easy-handler (refresh-group :uri "/refresh-group") (name login)
  (standard-refresh (:title "SkyApp") (:ol :id "scroll-table-2" 
					   (dolist (users (user-list (group-from-name name)))
					     (htm
					      (:div :class "panel-body chat-box-online"
						    (:div :class "chat-box-online-left"
		 (:img :src (format nil "~a" (image (user-from-login users))) :class "img-circle")
		 ;; (:br)
		 (format t " ~a" users))))))))
(define-easy-handler (refresh-group-list :uri "/refresh-group-list") (name login)
  (standard-refresh (:title "SkyApp") 	(:ol :id "scroll-table-3"
				     (dolist (groups (group-list (user-from-login login)))
				       (htm
					(:div :class "panel-body chat-box-new"
					      (:a :href (format nil "/show-group?name=~a&login=~a" groups login) (format t "~a" groups))))))))
			
;;     (
;; (:div :id "chart" ; Used for CSS styling of the links.
    ;; 	(:ol
    ;; 	 (dolist (groups *group-database*)
    ;; 	   (if (member (string login) (user-list groups) :test #'string-equal)
    ;; 	       (htm
    ;; 		(:a :href (format nil "/show-group?name=~a&login=~a" (name groups) login) (format t "~a~%" (name groups))))))))))

	
;;(standard-page (:title "Envie uma nova mensagem")
    ;;(:h1 "Adicione uma nova mensagem")


;; (define-easy-handler (listmessages :uri "/listmessages") (login)
;;   (standard-main-page (:title "Sky App")
;;      (:h1 "Bem vindo ao SkyApp!")
;;      (:p "Envie uma mensagem " (:a :href (format nil "new-message?login=~a" login) "aqui"))
;;      (:h2 "Current stand")
;;      (:div :id "chart" ; Used for CSS styling of the links.
;;        (:ol
;; 	(dolist (mensagens *mensagem-database*)
;; 	  (if (equal login (destinatario mensagens))
;; 	      (htm
;; 	       (format t "~a: ~a" (remetente mensagens) (escopo mensagens) 
;; 			   (open-time (tempo mensagens))))))))))

  
(define-easy-handler (login-inserted :uri "/login-inserted") (login password)
  (let*((usuario (user-from-login login)))
    (if (or (null usuario) (not (equal password (password usuario))))
	(redirect "/login")
	(redirect (format nil "/show-group?name=Bem-Vindo&login=~a" login)))))

(define-easy-handler (login-admin-inserted :uri "/login-admin-inserted") (login password)
  (let*((usuario (user-from-login login)))
    (if (or (null usuario) (not (equal password (password usuario))))
	(redirect "/login-admin")
	(redirect  "/admin"))))

(define-easy-handler (admin :uri "/admin") ()
  (standard-page (:title "Admin")
  (:h1 "Entre com Login e Senha do novo Usuario!")
  (:form :style "width:100%"  :action "/user-inserted" :method "get" :id "addform"
	 (:p "Login" (:br)
	     (:input :type "text" :name "login" :class "txt"))
	 (:p "Senha" (:br)
	     (:input :type "password" :name "password" :class "txt"))
	 (:p (:input :style "width:100%" :type "submit" :value "Criar" :class "btn")))))


(define-easy-handler (user-inserted :uri "/user-inserted") (login password)
 ;; (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
  ;;print login))
  (add-user login password)
  (redirect (format nil "/admin"))) ; back to the front page



(define-easy-handler (message-added :uri "/message-added") (login group escopo)
 ;; (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
  ;;print login))
  (add-mensagem escopo login group))
;;  (redirect (format nil "/show-group?name=~a&login=~a" group login))) ; back to the front page

(define-easy-handler (contact-added :uri "/contact-added") (login group novo)
  (unless (or (null novo) (zerop (length novo))(member novo (user-list (group-from-name group))) ; In case JavaScript is turned off.
   (print login)))
  (add-user-to-group novo group)
  (redirect (format nil "/show-group?name=~a&login=~a" group login))) ; back to the front page

(define-easy-handler (group-added :uri "/group-added") (login novo)
  ;;(unless (or (null novo) (zerop (length novo))(member novo (user-list (group-from-name group))) ; In case JavaScript is turned off.
  ;; (print login)))
  (add-group login novo)
  (redirect (format nil "/show-group?name=~a&login=~a" novo login))) ; back to the front page


(publish-static-content)
(start-server 8081)
