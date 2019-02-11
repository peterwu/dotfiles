(require 's)
(require 'url)

(setq max-mini-window-height 10)

(defun ixl--wind-degree-to-direction (degree)
  (when  (numberp degree)
    (cond ((or  (>= degree 348.75)
		(< degree 11.25))
	   "N")
	  ((and (>= degree 11.25)
		(< degree 33.75))
	   "NNE")
	  ((and (>= degree 33.75)
		(< degree 56.25))
	   "NE")
	  ((and (>= degree 56.25)
		(< degree 78.75))
	   "ENE")
	  ((and (>= degree 78.75)
		(< degree 101.25))
	   "E")
	  ((and (>= degree 101.25)
		(< degree 123.75))
	   "ESE")
	  ((and (>= degree 123.75)
		(< degree 146.25))
	   "SE")
	  ((and (>= degree 146.25)
		(< degree 168.75))
	   "SSE")
	  ((and (>= degree 168.75)
		(< degree 191.25))
	   "S")
	  ((and (>= degree 191.25)
		(< degree 213.75))
	   "SSW")
	  ((and (>= degree 213.75)
		(< degree 236.25))
	   "SW")
	  ((and (>= degree 236.25)
		(< degree 258.75))
	   "WSW")
	  ((and (>= degree 258.75)
		(< degree 281.25))
	   "W")
	  ((and (>= degree 281.25)
		(< degree 303.75))
	   "WNW")
	  ((and (>= degree 303.75)
		(< degree 326.25))
	   "NW")
	  ((and (>= degree 326.25)
		(< degree 348.75))
	   "NNW"))))

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
       :city (plist-get data :name)
       :main (plist-get (aref (plist-get data :weather) 0) :main)
       :description (plist-get (aref (plist-get data :weather) 0) :description)
       :temp (round (plist-get (plist-get data :main) :temp))
       :minTemp (round (plist-get (plist-get data :main) :temp_min))
       :maxTemp (round (plist-get (plist-get data :main) :temp_max))
       :cloudiness (round (plist-get (plist-get data :clouds) :all))
       :humidity (plist-get (plist-get data :main) :humidity)
       :pressure (plist-get (plist-get data :main) :pressure)
       :windSpeed (round (* 3.6 (plist-get (plist-get data :wind) :speed))) ;; convert m/s to km/h
       :windDir (plist-get (plist-get data :wind) :deg)
       :sunrise (format-time-string "%R %Z" (seconds-to-time (plist-get (plist-get data :sys) :sunrise)))
       :sunset (format-time-string "%R %Z" (seconds-to-time (plist-get (plist-get data :sys) :sunset)))))))

(defvar ixl--weather-info nil)

(defvar ixl-weather-mode-line-string nil)
(put 'ixl-weather-mode-line-string 'risky-local-variable t)

(defun ixl--weather-details ()
  (concat
   (format "%12s: %s" "Location" (plist-get ixl--weather-info :city)) "\n"
   (format "%12s: %s { %s }" "Weather" (plist-get ixl--weather-info :main) (plist-get ixl--weather-info :description)) "\n"
   (format "%12s: %d°C / %d°C" "Temperature" (plist-get ixl--weather-info :minTemp) (plist-get ixl--weather-info :maxTemp)) "\n"
   (format "%12s: %d%%" "Cloudiness" (plist-get ixl--weather-info :cloudiness)) "\n"
   (format "%12s: %d%%" "Humidity" (plist-get ixl--weather-info :humidity)) "\n"
   (format "%12s: %d hPa" "Pressure" (plist-get ixl--weather-info :pressure)) "\n"
   (format "%12s: %d km/h %s" "Wind" (plist-get ixl--weather-info :windSpeed) (ixl--wind-degree-to-direction (plist-get ixl--weather-info :windDir))) "\n"
   (format "%12s: %s" "Sunrise" (plist-get ixl--weather-info :sunrise)) "\n"
   (format "%12s: %s" "Sunset" (plist-get ixl--weather-info :sunset) "")))

(defun ixl-show-weather-details ()
  (interactive)
  (message "%s" (ixl--weather-details)))
(global-set-key (kbd "C-c w") 'ixl-show-weather-details)

(defvar ixl--display-weather-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]  'ixl--show-weather-details)
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
			   (weather-brief-info
			    (format "[%s %d°C]" (plist-get weather :main)  (plist-get weather :temp))))
		      (setq ixl-weather-mode-line-string
			    (propertize weather-brief-info
					'help-echo (plist-get weather :city)))
		      (force-mode-line-update)))))

(provide 'ixl-display-weather)
