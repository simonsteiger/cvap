box::use(
    raster,
    sp,
    json = geojsonio,
    rms = rmapshaper,
)

sweden <- raster$getData("GADM", country = "SWEDEN", level = 1)

sweden_small <- rms$ms_simplify(sweden, keep = 0.05)

#' @export
sweden_json_small <- json$geojson_list(sweden_small)
