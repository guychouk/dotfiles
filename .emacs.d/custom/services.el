;; Enables management of predefined services
(package-install 'prodigy)
(require 'prodigy)
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
  :tags '(work))
(prodigy-define-service
  :name "Strip-PWA"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "strip-pwa")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "SDK (Tests)"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "sdk-tests")
  :stop-signal 'sigterm
  :tags '(tests sdk))
(prodigy-define-service
  :name "Editor"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "editor")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "Campaign"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "campaign")
  :stop-signal 'sigterm
  :tags '(work sdk))
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
  :tags '(work sdk))
(prodigy-define-service
  :name "Campaign-API"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "campaign-api")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "Console"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "console")
  :stop-signal 'sigterm
  :tags '(work sdk))
(prodigy-define-service
  :name "Player"
  :command "docker-compose"
  :args '("-f" "/Users/guyvalariola/Projects/box/docker-compose.yml" "up" "player")
  :stop-signal 'sigterm
  :tags '(work sdk))
