library(dplyr)
library(rvest)

#Question 3
#source
url="https://en.wikipedia.org/wiki/Ironman_Triathlon#:~:text=The%20most%20recent%20Ironman%20World,hours%2040%20minutes%2024%20seconds."

#read in the htlm
page = read_html(url)

#create table
IM_table = page %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table() %>% .[[1]]

#Question 4
library(fredr)
HVAC_PPI = fredr(series_id = "PCU3334133341")


