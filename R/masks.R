
.water_mask_dir <- function(version, year) {
    wm_base_dir <- "data/derived/masks/base/water"
    wm_base_dir <- .project_env_variable("MASK_WATER_BASE_DIR", wm_base_dir)

    fs::path(wm_base_dir) / version / year
}

#' @export
prepare_water_mask <- function(region_id, reference_year = 2020, multicores = 36) {
    # Define output dir
    output_dir <- .water_mask_dir(year = reference_year, version = "glad")

    # Create directories
    fs::dir_create(output_dir)

    # Prepare output file
    output_file <- sprintf(
        "LANDSAT_TM-ETM-OLI_MOSAIC_%d-01-01_%d-01-31_class_v1.tif", reference_year, reference_year
    )

    output_file <- fs::path_abs(output_dir / output_file)

    # Define eco region SF
    eco_region_roi <- roi_ecoregions(
        region_id  = region_id,
        crs        = "EPSG:4326",
        as_union   = TRUE,
        use_buffer = TRUE,
        as_file    = TRUE
    )

    eco_region_roi_sf <- sf::read_sf(eco_region_roi, quiet = TRUE)

    # Load cube
    reference_cube <- sits::sits_cube(
        source      = "OGH",
        collection  = "LANDSAT-GLAD-2M",
        roi         = eco_region_roi_sf,
        crs         = "EPSG:4326",
        start_date  = sprintf("%d-01-01", reference_year),
        end_date    = sprintf("%d-01-31", reference_year),
        bands       = "BLUE"
    )

    # Load raster
    mask_file <- reference_cube[["file_info"]][[1]][["path"]]

    # Mosaic
    sits:::.gdal_crop_image(
        file = mask_file,
        out_file = output_file,
        roi_file = eco_region_roi,
        data_type = "INT1U",
        as_crs = "EPSG:4326",
        multicores = multicores,
        overwrite = TRUE,
        miss_value = 255
    )

    system(
        sprintf("gdal_edit.py %s -unsetnodata -offset 0 -scale 1", output_file)
    )

    system(
        sprintf("gdal_calc.py -A %s --calc='A==255' --outfile=%s", output_file, output_file)
    )

    system(
        sprintf("gdal_edit.py %s -a_nodata 0", output_file)
    )
}
