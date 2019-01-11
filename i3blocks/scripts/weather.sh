#!/bin/bash

# curl ifconfig.me
# curl ipinfo.io/<ip address>
# api.openweathermap.org/data/2.5/weather?lat=35&lon=139
key=94f7172e9d0fd1e0c9756a48fa9c9477
openweathermapUrl=https://api.openweathermap.org/data/2.5/weather

# {"coord":{"lon":139,"lat":35},
# "sys":{"country":"JP","sunrise":1369769524,"sunset":1369821049},
# "weather":[{"id":804,"main":"clouds","description":"overcast clouds","icon":"04n"}],
# "main":{"temp":289.5,"humidity":89,"pressure":1013,"temp_min":287.04,"temp_max":292.04},
# "wind":{"speed":7.31,"deg":187.002},
# "rain":{"3h":0},
# "clouds":{"all":92},
# "dt":1369824698,
# "id":1851632,
# "name":"Shuzenji",
# "cod":200}

# build an array of mapping of icons to unicodes
declare -A arr
arr=(
  [01d]=f185
  [01n]=f186
  [02d]=f6c4
  [02n]=f6c3
  [03d]=f0c2
  [03n]=f0c2
  [04d]=f73b
  [04n]=f73b
  [09d]=f73d
  [09n]=f73b
  [10d]=f743
  [10n]=f73c
  [11d]=f740 # could find a better icon
  [11n]=f740
  [13d]=f2dc
  [13n]=f2dc
  [50d]=f773
  [50n]=f773
)

ip_address=$(curl -s https://ifconfig.me)
geoloc=$(curl -s https://ipinfo.io/$ip_address | jq -r '.loc')
lat=$(echo $geoloc | cut -d ',' -f 1)
lon=$(echo $geoloc | cut -d ',' -f 2)
weather=$(curl -s "$openweathermapUrl?lat=$lat&lon=$lon&units=metric&appid=$key")
icon=$(echo $weather | jq -r '.weather[0].icon')
icon="\u"${arr[$icon]}
temp=$(echo $weather | jq -r '.main.temp')

echo -e "$icon $temp\u2103"

exit 0
