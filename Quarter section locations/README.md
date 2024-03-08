# Quarter Section Locations

A short little R script that includes three functions:

1) Locate Quarter section coordinates in Manitoba by legal land designation name (e.g., NW-31-5-27W). 
  - `search_legal()` 
2) Locate Quarter section in Manitoba by longitude and latitude, e.g., -101.4656, 51.81913.
  - `search_coord()`
  - Locates quarter section based on the shortest euclidean distance to the center of the section
3) Map the above centroids and ploygons of the located sections 
  - `map_quarter()`

Still a work in progress. My to do list:
1) Include checks in `search_coord()`. Right now it will **always** return the closest matching quarter section, even if coordinates are outside MB
2) Turn this into an R package!!!
  - Need a clever name (open to suggestions)
3) Have the package peer-reviewed

Data source: https://mli.gov.mb.ca/quarter_sec/index.html