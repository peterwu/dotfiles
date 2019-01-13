#!/usr/bin/env bash

key=94f7172e9d0fd1e0c9756a48fa9c9477
openweathermapUrl=https://api.openweathermap.org/data/2.5/weather

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

geoloc=$(curl -s https://ipinfo.io | jq -r '.loc,.city' | paste -sd, -)
if [[ -n $geoloc ]]; then 
  lat=$(echo $geoloc | cut -d ',' -f 1)
  lon=$(echo $geoloc | cut -d ',' -f 2)
  city=$(echo $geoloc | cut -d ',' -f 3)
  weather=$(curl -s "$openweathermapUrl?lat=$lat&lon=$lon&units=metric&appid=$key")
  icon=$(echo $weather | jq -r '.weather[0].icon')
  icon="\u"${arr[$icon]}
  temp=$(echo $weather | jq -r '.main.temp')
  temp=${temp%.*}

  echo -e "$city $icon $temp\u2103"
  exit 0
else
  exit 33
fi 
