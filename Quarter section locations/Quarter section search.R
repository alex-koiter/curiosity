library(tidyverse)
library(sf)
library(mapview)

# Get the data

MB_QuarterSection <- "MB_QuarterSectionCorners.csv"
if (file.exists(MB_QuarterSection)) {
  print("The data already exists!")
} else {
  download.file("https://mli.gov.mb.ca/quarter_sec/data/mb_township_system_corners.zip", 
                destfile = "mb_township_system_corners.zip")
  unzip("mb_township_system_corners.zip", files = "MB_QuarterSectionCorners.csv")
}


# Extract quarter section number 
MB_corners <- read.csv("MB_QuarterSectionCorners.csv") %>%
  mutate(Q = str_sub(DLS_KEY, start = 10, end = 11),
         S = str_remove(str_sub(DLS_KEY, start = 8, end = 9), "^0+"),
         `T` = str_remove(str_sub(DLS_KEY, start = 5, end = 7), "^0+"),
         R = str_remove(str_sub(DLS_KEY, start = 3, end = 4), "^0+"),
         M = str_sub(DLS_KEY, start = 1, end = 1)) %>%
  mutate(legal = paste(paste(Q, S,`T`, R, sep = "-"), M, sep ="")) 


## Search by coordinates 
closest_centoid <- function(X, Y, data = MB_corners) {
  data %>%
    bind_cols(X = X, Y = Y) %>%
    mutate(dist = (X - EASTING)^2 + (NORTHING - Y) ^2) %>%
    filter(dist == min(dist)) %>%
    select(-X, -Y)
}

search_coord <- function(long, lat) {
  tibble(long = long, lat = lat) %>%
    st_as_sf(coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
    st_transform(crs = "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")  %>%
    bind_cols(st_coordinates(.)) %>%
    st_drop_geometry() %>%
    rowid_to_column("point") %>%
    mutate(dist = map2(X, Y, closest_centoid)) %>%
    unnest(dist) %>%
    select(point, legal) %>%
    left_join(tibble(long = long, lat = lat) %>%
                rowid_to_column("point"), by = "point") %>%
    select(-point)
}

## Search by legal designation
search_legal <- function(x, data = MB_corners) {
  data %>%
    filter(legal %in% x) %>%
    st_as_sf(coords = c("EASTING", "NORTHING"), crs = "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    bind_cols(st_coordinates(.)) %>%
    st_drop_geometry() %>%
    select(legal, long = X, lat = Y) %>%
    tibble()
}

## Mapping Functions
centroid <- function(x, data = MB_corners) {
  data %>%
    right_join(x, by = join_by(legal)) %>%
    st_as_sf(coords = c("EASTING", "NORTHING"), crs = "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    select(legal, geometry)
}

polygon <- function(x, data = MB_corners) {
  data %>%
    right_join(x, by = join_by(legal)) %>%
    pivot_longer(cols = NE_E:SE_N, names_to = c("corner", "coord"), names_sep = "_", values_to = "value") %>%
    pivot_wider(names_from = coord, values_from = value) %>%
    drop_na(N) %>%
    st_as_sf(coords = c("E", "N"), crs = "+proj=utm +zone=14 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>%
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    group_by(legal) %>%
    summarise() %>%
    ungroup() %>%  
    st_convex_hull()
}


map_quarter <- function(x, map.type = "Esri.WorldImagery") {
  Polygon <- polygon(x)
  Centroid <- centroid(x)
    mapview(list(Centroid, Polygon), map.type = map.type, homebutton = FALSE)
}            


## Examples of the three functions
test1 <- search_coord(long = c(-101.4656, -102.4660), lat = c(51.81913, 51.83500))
test1

test2 <- search_legal(x = c("NE-11-33-29W", "SW-20-2-1W"))
test2

test3 <- map_quarter(test1)
test3

test4 <-map_quarter(test2)
test4
