(require 'dash)
(require 's)
(require 'url)
(require 'request)

(setq max-mini-window-height 10)

(defun my--wind-degree-to-direction (degree)
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

;; http://ip-api.com/json/
;; (defun my-get-coordinate ()
;;   (let ((url "http://ip-api.com/json/"))
;;     (let ((data (my-get-and-parse-json url)))
;;       (list :lat (plist-get data :lat)
;; 	    :lon (plist-get data :lon)))))


;; https://ipinfo.io/json
(defun my--get-coordinate ()
  (let* ((token "44cf10c96aeed5")
	 (url (concat "https://ipinfo.io/json"
		      "?"
		      (url-build-query-string (list
					       (list "token" token))))))
    (request url
	     :parser 'json-read
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (my--get-weather-info (-interleave '(:lat :lon) (split-string (assoc-default 'loc data) ","))))))))
    

(defun my--get-weather-info (coord)
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
    (request url
	     :parser 'json-read
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (setq my--weather-info  (list
						   :city (assoc-default 'name data)
						   :main (assoc-default 'main (aref (assoc-default 'weather data) 0))
						   :description (assoc-default 'description (aref (assoc-default 'weather data) 0))
						   :temp (round (assoc-default 'temp (assoc-default 'main data)))
						   :minTemp (round (assoc-default 'temp_min (assoc-default 'main data)))
						   :maxTemp (round (assoc-default 'temp_max (assoc-default 'main data)))
						   :cloudiness (round (assoc-default 'all (assoc-default 'clouds data)))
						   :humidity (assoc-default 'humidity (assoc-default 'main data))
						   :pressure (assoc-default 'pressure (assoc-default 'main data))
						   :windSpeed (round (* 3.6 (assoc-default 'speed (assoc-default 'wind data)))) ;; convert m/s to km/h
						   :windDir (assoc-default 'deg (assoc-default 'wind data))
						   :sunrise (format-time-string "%R %Z" (seconds-to-time (assoc-default 'sunrise (assoc-default 'sys data))))
						   :sunset (format-time-string "%R %Z" (seconds-to-time (assoc-default 'sunset (assoc-default 'sys data))))))
			 (let* ((weather my--weather-info)
			 	(weather-brief-info
			 	 (format "[%s %d°C]" (plist-get weather :main)  (plist-get weather :temp))))
			   (setq my-weather-mode-line-string
			 	 (propertize weather-brief-info
			 		     'help-echo (plist-get weather :city)))
			   (force-mode-line-update)))))))

(defcustom my-weather-visible-lines 10
  "Display how many lines in the *Weather* buffer"
  :type 'number)

(defconst my--weather-buffer-name "*Weather*")

(defvar my--weather-info nil)

(defvar my-weather-mode-line-string nil)
(put 'my-weather-mode-line-string 'risky-local-variable t)

(defun my--weather-details ()
  (if my--weather-info  (concat
			   (format "%12s: %s" "Location" (plist-get my--weather-info :city)) "\n"
			   (format "%12s: %s { %s }" "Weather" (plist-get my--weather-info :main) (plist-get my--weather-info :description)) "\n"
			   (format "%12s: %d°C / %d°C" "Temperature" (plist-get my--weather-info :minTemp) (plist-get my--weather-info :maxTemp)) "\n"
			   (format "%12s: %d%%" "Cloudiness" (plist-get my--weather-info :cloudiness)) "\n"
			   (format "%12s: %d%%" "Humidity" (plist-get my--weather-info :humidity)) "\n"
			   (format "%12s: %d hPa" "Pressure" (plist-get my--weather-info :pressure)) "\n"
			   (format "%12s: %d km/h %s" "Wind" (plist-get my--weather-info :windSpeed) (my--wind-degree-to-direction (plist-get my--weather-info :windDir))) "\n"
			   (format "%12s: %s" "Sunrise" (plist-get my--weather-info :sunrise)) "\n"
			   (format "%12s: %s" "Sunset" (plist-get my--weather-info :sunset) ""))
  "No weather information available at this time."))

(defun my-show-weather-details ()
  (interactive)
  (when (and (not (window-dedicated-p))
	     (window-full-height-p))
    (let ((win (split-window-below (- my-weather-visible-lines)))
	  (buff (get-buffer-create my--weather-buffer-name)))
      (with-current-buffer buff
	(setq mode-line-format nil)
	(erase-buffer)
	(insert (my--weather-details)))
      (set-window-buffer win buff))))
(global-set-key (kbd "C-c w") 'my-show-weather-details)

(defun my--retrieve-weather-info ()
  (my--get-coordinate))

(defun my-display-weather-info ()
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'my-weather-mode-line-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(my-weather-mode-line-string))))
  (run-with-timer 0 (* 60 10)
		  (lambda ()
		    (my--retrieve-weather-info))))

(provide 'my-display-weather)
