box::use(
    sp,
    gd = geodata,
    json = geojsonio,
    rms = rmapshaper,
)

sweden <- gd$gadm(country = "SWEDEN", level = 1, path = "app/logic/data/")

sweden_small <- rms$ms_simplify(sweden, keep = 0.05)

sweden_json_small <- json$geojson_list(sweden_small)

json$geojson_write(sweden_json_small, file = "app/logic/data/gadm/sweden.geojson")
