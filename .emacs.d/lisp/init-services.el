;;; init-services.el --- My Prodigy Services
;;; Commentary:
;;

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'prodigy)
(prodigy-define-service
  :name "K300"
  :command "make"
  :cwd "/Users/guyvalariola/Projects/k300"
  :args '("dev")
  :stop-signal 'sigterm
  :tags '(work k300))
(prodigy-define-service
  :name "SDK"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "sdk")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "AMP"
  :command "yarn"
  :cwd "/Users/guyvalariola/Projects/amphtml"
  :args '("run" "gulp")
  :stop-signal 'sigterm
  :tags '(work amp))
(prodigy-define-service
  :name "Match"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "match")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "Strip-PWA"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "strip-pwa")
  :stop-signal 'sigterm
  :tags '(work strip))
(prodigy-define-service
  :name "Editor"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "editor")
  :stop-signal 'sigterm
  :tags '(work))
(prodigy-define-service
  :name "Campaign"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "campaign")
  :stop-signal 'sigterm
  :tags '(work campaign))
(prodigy-define-service
  :name "Campaign-API"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "campaign-api")
  :stop-signal 'sigterm
  :tags '(work campaign))
(prodigy-define-service
  :name "Display"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "display")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "Plans-API"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "plans-api")
  :stop-signal 'sigterm
  :tags '(work plans))
(prodigy-define-service
  :name "Console"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "console")
  :stop-signal 'sigterm
  :tags '(work))
(prodigy-define-service
  :name "ApeX"
  :command "yarn"
  :cwd "/Users/guyvalariola/Projects/ApeX"
  :args '("run" "dev")
  :stop-signal 'sigterm
  :tags '(hackathon))
(prodigy-define-service
  :name "Player"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "player")
  :stop-signal 'sigterm
  :tags '(work))

(provide 'init-services)

;;; init-services.el ends here
