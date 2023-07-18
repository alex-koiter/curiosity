library(tidyverse)
library(sf)
library(mapview)

# Get the data
download.file("https://mli.gov.mb.ca/quarter_sec/data/mb_township_system_corners.zip", 
              destfile = "mb_township_system_corners.zip")

unzip("mb_township_system_corners.zip", files = "MB_QuarterSectionCorners.csv")

# Extract quarter section number 
MB_corners <- read.csv("MB_QuarterSectionCorners.csv") %>%
  mutate(Q = str_sub(DLS_KEY, start = 10, end = 11),
         S = str_remove(str_sub(DLS_KEY, start = 8, end = 9), "^0+"),
         `T` = str_remove(str_sub(DLS_KEY, start = 5, end = 7), "^0+"),
         R = str_remove(str_sub(DLS_KEY, start = 3, end = 4), "^0+"),
         M = str_sub(DLS_KEY, start = 1, end = 1)) %>%
  mutate(legal = paste(paste(Q, S,`T`, R, sep = "-"), M, sep ="")) 

head(MB_corners)

# Map two Quarters for context
quarters <- c("NE-11-33-29W", "NE-1-33-29W")  #Do not include any leading zeros

search <- MB_corners %>%
  filter(legal %in% quarters) %>%
  st_as_sf(coords = c("EASTING", "NORTHING"), crs = "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

m <- mapview(search, map.type = "Esri.WorldImagery")
m


