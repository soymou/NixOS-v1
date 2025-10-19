;; Use built-in dark theme
(define-configuration browser
  ((theme theme:+dark-theme+)))

;; Set dark mode preference for web pages
(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

;; Web buffer configuration with VI mode and useful modes
(define-configuration web-buffer
  ((default-modes (append '(vi-normal-mode
                            hint-mode
                            blocker-mode
                            reduce-tracking-mode)
                          %slot-default%))
   (search-engines (list
                    (make-instance 'search-engine
                      :shortcut "ddg"
                      :search-url "https://duckduckgo.com/?q=~a&kae=d&kak=-1&kax=-1&kaq=-1&kap=-1&kap=-1&kao=-1&kau=-1&k1=-1&kav=1"
                      :fallback-url "https://duckduckgo.com/?kae=d&kak=-1&kax=-1&kaq=-1&kap=-1&kao=-1&kau=-1&k1=-1&kav=1")
                    (make-instance 'search-engine
                      :shortcut "google"
                      :search-url "https://www.google.com/search?q=~a"
                      :fallback-url "https://www.google.com/")))
   (default-search-engine (find-search-engine "ddg"))))
