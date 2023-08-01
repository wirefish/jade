;;;; Server configuration.

(in-package :jade)

(defparameter *config*
  #h(;; Where the server listens for player requests.
     :server-address "127.0.0.1"
     :server-port 5000
     ;; Timeout to disconnect an idle player.
     :session-idle-seconds* 1800
     ;; The directory that contains data files required by the server.
     :root-directory "/Users/craig/local/var/jade/"
     ;; Default properties for new avatars.
     :new-avatar-proto 'jade.lib::new-avatar
     :new-avatar-location 'jade.isle-of-dawn::hilltop))
