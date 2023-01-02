(use-package ejc-sql
  :config
  (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
  (ejc-create-connection
   "local"
   :classpath (concat "/Users/sakura/.m2/repository/com/mysql/mysql-connector-j/8.0.31/"
                      "mysql-connector-j-8.0.31.jar")
   :classname "com.mysql.cj.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//localhost:3069/unemeta_console"
   :user "unemeta_backend_dev"
   :password "uneune202"
   )
  (ejc-create-connection
   "test"
   :classpath (concat "/Users/sakura/.m2/repository/com/mysql/mysql-connector-j/8.0.31/"
                      "mysql-connector-j-8.0.31.jar")
   :classname "com.mysql.cj.jdbc.Driver"
   :subprotocol "mysql"
   :subname "//localhost:3069/unemeta_console"
   :user "unemeta_backend_dev"
   :password "uneune202"
   )
  (setq ejc-temp-editor-file-path (expand-file-name "ejc-sql" clytie-cache-dir))
  (setq ejc-conn-statistics-file (expand-file-name "ejc-sql/connection-statistics.el" clytie-cache-dir))
  )

(provide 'init-sql)
