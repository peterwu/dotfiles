(defun ixl--get-and-parse-json (url)
  (let ((buffer (url-retrieve-synchronously url))
	(json-object-type 'plist))
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "^$") 
    (json-read-from-string
     (buffer-substring-no-properties (point) (point-max)))))

;; http://ip-api.com/json/
;; (defun ixl-get-coordinate ()
;;   (let ((url "http://ip-api.com/json/"))
;;     (let ((data (ixl-get-and-parse-json url)))
;;       (list :lat (plist-get data :lat)
;; 	    :lon (plist-get data :lon)))))

;; https://ipinfo.io/json
(defun ixl--get-coordinate ()
  (let* ((token "44cf10c96aeed5")
	 (url (concat "https://ipinfo.io/json"
		      "?"
		      (url-build-query-string (list
					       (list "token" token))))))
    (let ((data (ixl--get-and-parse-json url)))
      (let ((loc (plist-get data :loc)))
	(-interleave '(:lat :lon) (split-string loc ","))))))

(defun ixl--get-weather-info (coord)
  (let* ((openweathermapUrl "https://api.openweathermap.org/data/2.5/weather")
	 (appid "94f7172e9d0fd1e0c9756a48fa9c9477")
	 (lat (plist-get coord :lat))
	 (lon (plist-get coord :lon))
	 (url (concat openweathermapUrl
		      "?"
		      (url-build-query-string (list
					       (list "lat" lat)
					       (list "lon" lon)
					       (list "appid" appid)
					       (list "units" "metric"))))))
    (let ((data (ixl--get-and-parse-json url)))
      (list
       :main (plist-get (aref (plist-get data :weather) 0) :main)
       :temp (number-to-string (round (plist-get (plist-get data :main) :temp)))
       :minTemp (number-to-string (round (plist-get (plist-get data :main) :temp_min)))
       :maxTemp (number-to-string (round (plist-get (plist-get data :main) :temp_max)))
       :humidity (number-to-string (plist-get (plist-get data :main) :humidity))
       :pressure (number-to-string (plist-get (plist-get data :main) :pressure))
       :wind (number-to-string (round (* 3.6 (plist-get (plist-get data :wind) :speed)))) ;; convert m/s to km/h
       :sunrise (format-time-string "%R %Z" (seconds-to-time(plist-get (plist-get data :sys) :sunrise)))
       :sunset (format-time-string "%R %Z" (seconds-to-time(plist-get (plist-get data :sys) :sunset)))))))

(defvar ixl--weather-info nil)

(defvar ixl-weather-mode-line-string nil)
(put 'ixl-weather-mode-line-string 'risky-local-variable t)

(defun ixl--weather-details ()
    (concat
     "Min Temp\t" (plist-get ixl--weather-info :minTemp) " °C\n"
     "Max Temp\t" (plist-get ixl--weather-info :maxTemp) " °C\n"
     "Humidity\t" (plist-get ixl--weather-info :humidity) " %\n"
     "Pressure\t" (plist-get ixl--weather-info :pressure) " hPa\n"
     "Wind\t" (plist-get ixl--weather-info :wind) " km/h\n"
     "Sunrise\t" (plist-get ixl--weather-info :sunrise) "\n"
     "Sunset\t" (plist-get ixl--weather-info :sunset) ""))

(defun ixl--show-weather-details ()
  (interactive)
  ;; (tooltip-show (ixl--weather-details))
  )

(defvar ixl--display-weather-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]  'ixl--show-weather-details)
    ;; (define-key map [mode-line down-mouse-1]  'minions-minor-modes-menu)
    map))

(defun ixl-display-weather-info ()
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'ixl-weather-mode-line-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(ixl-weather-mode-line-string))))
  (run-with-timer 0 (* 60 10)
		  (lambda ()
		    (setq ixl--weather-info (ixl--get-weather-info (ixl--get-coordinate)))
		    (let* ((weather ixl--weather-info)
			   (weather-brief-info (concat "[" 
						       (plist-get weather :main)
						       " "
						       (plist-get weather :temp)
						       "°C]")))
		      (setq ixl-weather-mode-line-string
			    (propertize weather-brief-info
					'help-echo "Click for more details" 
					'local-map ixl--display-weather-keymap))
		      (force-mode-line-update)))))

(provide 'ixl-display-weather)
