# Terra installation fails because gdal config not found
# see https://github.com/rspatial/terra/issues/487

box::use(
    sp,
    gd = geodata,
    json = geojsonio,
    ut = utils,
    sf,
    terra,
)

sweden <- gd$gadm(country = "SWEDEN", level = 1, path = "app/logic/data/")

sweden_small <- terra$simplifyGeom(sweden, 0.05) # going to 0.1 gives us a pretty rugged map

terra$writeVector(sweden_small, filename = "app/logic/data/gadm/sweden_test.geojson", filetype = "GeoJSON", overwrite = TRUE)

sweden_sf <- sf$st_as_sf(sweden_small)

# Previous pipeline to get json data, no longer working because gadm now provides SpatVector class
# Above is working pipeline to download SpatVector, simplify and save as json
# sweden_small <- rmapshaper::ms_simplify(sweden, keep = 0.05)
# sweden_json_small <- json$geojson_list(sweden_small)
# json$geojson_write(sweden_json_small, file = "app/logic/data/gadm/sweden.geojson")
