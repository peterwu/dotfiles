#!/usr/bin/env bash

key=94f7172e9d0fd1e0c9756a48fa9c9477
openweathermapUrl=https://api.openweathermap.org/data/2.5/weather

# build an array of mapping of icons to unicodes
declare -A icons

icons=(
    [01d]=""
    [01n]=""
    [02d]=""
    [02n]=""
    [03d]=""
    [03n]=""
    [04d]=""
    [04n]=""
    [09d]=""
    [09n]=""
    [10d]=""
    [10n]=""
    [11d]=""
    [11n]=""
    [13d]=""
    [13n]=""
    [50d]=""
    [50n]=""
)
	    
geoloc=$(curl -s https://ipinfo.io | jq -r '.loc,.city' | paste -sd, -)
if [[ -n $geoloc ]]; then 
  lat=$(echo $geoloc | cut -d ',' -f 1)
  lon=$(echo $geoloc | cut -d ',' -f 2)
  city=$(echo $geoloc | cut -d ',' -f 3)
  weather=$(curl -s "$openweathermapUrl?lat=$lat&lon=$lon&units=metric&appid=$key")
  icon=$(echo $weather | jq -r '.weather[0].icon')
  icon=${icons[$icon]}
  temp=$(echo $weather | jq -r '.main.temp')
  temp=${temp%.*}

  # echo -e "$city $icon $temp℃"
  echo -e "$icon $temp℃"
  exit 0
else
  exit 33
fi 
