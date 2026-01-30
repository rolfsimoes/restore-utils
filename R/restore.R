
#' @export
load_restore_map_bdc <- function(data_dir, multicores = 32, memsize = 120, labels = NULL, ...) {
    # Default classification label - based on classification results
    default_label <- c(
        "1" = "2ciclos",
        "2" = "Agr. Semiperene",
        "3" = "agua",
        "4" = "Forest",
        "5" = "Mountainside_Forest",
        "6" = "past_arbustiva",
        "7" = "past_herbacea",
        "8" = "Riparian_Forest",
        "9" = "Seasonally_Flooded_ICS",
        "10" = "Silvicultura",
        "11" = "vegetacao_secundaria",
        "12" = "Wetland_ICS"
    )

    if (is.null(labels)) {
        labels = default_label
    }

    sits::sits_cube(
        source = "BDC",
        collection = "LANDSAT-OLI-16D",
        data_dir = data_dir,
        memsize = memsize,
        multicores = multicores,
        parse_info = c("satellite", "sensor",
                       "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = labels,
        ...
    )
}

#' @export
load_restore_map_glad <- function(data_dir, multicores = 32, memsize = 120, labels = NULL, ...) {
    # Default classification label - based on classification results
    default_label <- c(
        "1" = "2ciclos",
        "2" = "Agr. Semiperene",
        "3" = "Forest",
        "4" = "Mountainside_Forest",
        "5" = "past_arbustiva",
        "6" = "past_herbacea",
        "7" = "Riparian_Forest",
        "8" = "Seasonally_Flooded_ICS",
        "9" = "Silvicultura",
        "10" = "vegetacao_secundaria",
        "11" = "Wetland_ICS"
    )

    if (is.null(labels)) {
        labels = default_label
    }

    sits::sits_cube(
        source = "BDC",
        collection = "LANDSAT-OLI-16D",
        data_dir = data_dir,
        memsize = memsize,
        multicores = multicores,
        parse_info = c("satellite", "sensor",
                       "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = labels,
        ...
    )
}




#' @export
get_restore_masks_files <- function(mask_version, files_version, multicores = 32, memsize = 120) {
    files_dir <- create_data_dir("data/derived/masks", mask_version)

    files_pattern <- paste0("^LANDSAT_OLI_MOSAIC_\\d{4}-01-01_\\d{4}-12-01_class_", files_version, "\\.tif$")

    files <- list.files(
        path = files_dir,
        pattern = files_pattern,
        recursive = TRUE,
        full.names = TRUE
    )

    years <- get_mask_file_year(files)

    # sort files by year
    ordered_indices <- order(years)
    files <- files[ordered_indices]
    years <- years[ordered_indices]

    expected_years <- seq(min(years), max(years))

    if (!all(years == expected_years)) {
        stop("Sanity check failed: years are missing or out of order.\nFound years: ", paste(years, collapse = ", "))
    }

    return(files)
}

#' @export
get_restore_rds_files <- function(mask_version) {
    files_dir <- fs::path("data/derived/masks") / mask_version

    files <- fs::dir_ls(
        path = files_dir,
        regexp = "mask-cube.rds",
        recurse = TRUE
    )

    files_lst <- lapply(files, function(file_rds) {
        cube <- readRDS(file_rds)
        year <-  as.integer(gsub(".*/(\\d{4})", "\\1", fs::path_dir(file_rds)))

        tibble::tibble(
            file = file_rds,
            year = year,
            labels = list(sits::sits_labels(cube)),
            path = cube[["file_info"]][[1]][["path"]]
        )
    })

    files <- do.call(rbind, files_lst)

    return(files)
}

#' @export
load_cerrado_version <- function(year, version, multicores = 10, memsize = 16, data_dir = "data/derived/classifications", labels = NULL) {
    data_dir <- fs::path(data_dir) / version / year
    rds_file <- data_dir / "map.rds"

    default_labels <- c(
        "1" = "Agricultura anual",
        "2" = "Campo Natural",
        "3" = "Água",
        "4" = "Floresta",
        "5" = "Formações Arenosas",
        "6" = "Pasture",
        "7" = "Perennial_Crop",
        "8" = "Silviculture",
        "9" = "Sugarcane",
        "10" = "Vegetação Natural"
    )

    if (is.null(labels)) {
        labels <- default_labels
    }

    if (fs::file_exists(rds_file)) {

        cube <- readRDS(rds_file)

    } else {
        cube <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = data_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = labels
        )

        saveRDS(cube, rds_file)
    }

    # Return!
    return(cube)
}

#' @export
get_restore_assets_files <- function(mask_version, files_version, multicores = 32, memsize = 120) {
    files_dir <- create_data_dir("data/derived/masks/base", mask_version)
    files_pattern <- "LANDSAT_TM-ETM-OLI_MOSAIC_2024-01-01_2024-12-31_class_v1\\.tif$"

    files <- fs::dir_ls(
        path    = files_dir,
        recurse = TRUE,
        regexp  = files_pattern
    )

    years <- get_mask_file_year(files)

    # sort files by year
    ordered_indices <- order(years)
    files <- files[ordered_indices]
    years <- years[ordered_indices]

    expected_years <- seq(min(years), max(years))

    if (!all(years == expected_years)) {
        stop("Sanity check failed: years are missing or out of order.\nFound years: ", paste(years, collapse = ", "))
    }

    return(files)
}
