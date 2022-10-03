## code to prepare `50m_geojson` dataset goes here
# Read geojson file to plot the map of world
worldcountry = geojsonio::geojson_read("data-raw/50m.geojson", what = "sp")
usethis::use_data(worldcountry, overwrite = TRUE)

