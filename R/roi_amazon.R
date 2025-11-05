.roi_amazon_ecoregion_data_file <- function() {
    default_file <- system.file("extdata/amazon/amazon-regions-bdc-md.gpkg", package = "restoreutils")

    fs::path(Sys.getenv("RESTORE_PLUS_AMAZON_REGION_ROI_FILE", default_file))
}

.roi_amazon_biome_data_file <- function() {
    default_file <- system.file("extdata/amazon/amazon-biome.gpkg", package = "restoreutils")

    fs::path(Sys.getenv("RESTORE_PLUS_AMAZON_BIOME_ROI_FILE", default_file))
}

.roi_amazon_ecoregion_name <- function(region_id) {
    paste0("eco_", region_id)
}

.roi_amazon_ecoregion_sf <- function(region_id, crs) {
    # Region name
    eco_name <- .roi_amazon_ecoregion_name(region_id)

    # Load ecoregion roi file
    eco_region <- sf::st_read(.roi_amazon_ecoregion_data_file(), quiet = TRUE)

    # Transform / Filter region (region 3)
    eco_region <- sf::st_transform(eco_region, crs = crs)
    eco_region <- dplyr::filter(eco_region, layer == eco_name) |>
        dplyr::select(-gid, -id, -grs_schema)
}

.roi_amazon_biome_sf <- function(crs = NULL) {
    # Load ecoregion roi file
    eco_region <- sf::st_read(.roi_amazon_biome_data_file(), quiet = TRUE)

    if (!is.null(crs)) {
        eco_region <- sf::st_transform(eco_region, crs = crs)
    }

    return(eco_region)
}

#' @export
roi_amazon_regions <- function(region_id, crs, as_file = FALSE, as_union = FALSE, as_convex = FALSE, use_buffer = FALSE) {
    # generate eco region geometry
    eco_region_geom <- .roi_amazon_ecoregion_sf(
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

#' @export
roi_amazon_biome <- function(crs = NULL, as_file = FALSE, as_convex = FALSE, use_buffer = FALSE) {
    # generate eco region geometry
    amazon_geom <- .roi_amazon_biome_sf(crs = crs) |>
                    sf::st_make_valid()

    # apply buffer if required
    if (use_buffer) {
        amazon_geom <- sf::st_buffer(amazon_geom, 0.001)
    }

    # transform convex hull
    if (as_convex) {
        amazon_geom <- sf::st_union(amazon_geom) |>
            sf::st_convex_hull()
    }

    if (as_file) {
        # create temp file
        amazon_file <- fs::file_temp(ext = "gpkg")

        # save sf
        sf::st_write(amazon_geom, amazon_file, quiet = TRUE)

        # update result variable
        amazon_geom <- amazon_file
    }

    # return!
    return(amazon_geom)
}
