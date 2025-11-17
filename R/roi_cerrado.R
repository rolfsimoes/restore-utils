.roi_cerrado_ecoregion_data_file <- function() {
    default_file <- system.file("extdata/cerrado/cerrado-regions-bdc-md.gpkg", package = "restoreutils")

    fs::path(Sys.getenv("RESTORE_PLUS_AMAZON_REGION_ROI_FILE", default_file))
}

.roi_cerrado_ecoregion_name <- function(region_id) {
    paste0("Q", region_id)
}

.roi_cerrado_ecoregion_sf <- function(region_id, crs) {
    # Region name
    region_name <- .roi_cerrado_ecoregion_name(region_id)

    # Load ecoregion roi file
    eco_region <- sf::st_read(.roi_cerrado_ecoregion_data_file(), quiet = TRUE)

    # Transform / Filter region (region 3)
    if (!is.null(crs)) {
        eco_region <- suppressWarnings(sf::st_transform(eco_region, crs = crs))
    }

    eco_region <- dplyr::filter(eco_region, part == !!region_name) |>
                    dplyr::mutate(tile_id = .data[["tile"]]) |>
                    dplyr::select(-name_biome, -code_biome, -year, -tile)

}

#' @export
roi_cerrado_regions <- function(region_id, crs = NULL, as_file = FALSE, as_union = FALSE, as_convex = FALSE) {
    # generate eco region geometry
    eco_region_geom <- .roi_cerrado_ecoregion_sf(
        region_id = region_id,
        crs = crs
    )

    # Union
    if (as_union) {
        if (use_buffer) {
            eco_region_geom <- sf::st_buffer(eco_region_geom, 0.001)
        }

        eco_region_geom <- eco_region_geom |>
            sf::st_union() |>
            sf::st_make_valid()
    }

    # Transform convex hull
    if (as_convex) {
        eco_region_geom <- sf::st_union(eco_region_geom) |>
            sf::st_convex_hull()
    }

    if (as_file) {
        # Create temp file
        eco_region_file <- fs::file_temp(ext = "gpkg")

        # Save sf
        sf::st_write(eco_region_geom, eco_region_file, quiet = TRUE)

        # Update result variable
        eco_region_geom <- eco_region_file
    }

    # return!
    return(eco_region_geom)
}
