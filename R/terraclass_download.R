
.terraclass_files <- function() {
    files <- c(
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2008.M.zip",
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2010.M.zip",
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2012.M.zip",
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2014.M.zip",
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2018.M.zip",
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2020.M.zip",
        "https://www.terraclass.gov.br/helpers/terraclass_data4download_2024/M/amz/AMZ.2022.M.zip"
    )

    tibble::tibble(
        file = files,
        year = as.numeric(stringr::str_extract(files, "(?<=\\.)\\d{4}(?=\\.)"))
    )
}

.terraclass_file_metadata <- function(year) {
    file_selected <- .terraclass_files() |>
        dplyr::filter(.data[["year"]] == !!year)

    # Sanity check: We need to have one file per year
    if (nrow(file_selected) != 1) {
        cli::cli_abort("{year} is invalid and is not available as a Terraclass year")
    }

    # Return!
    file_selected
}

.terraclass_file_zip <- function(tc_year_file) {
    fs::path_file(tc_year_file[["file"]])
}

.terraclass_file_sits_name <- function(year, ext) {
    template <- "LANDSAT_TM-ETM-OLI_MOSAIC_XYZ-01-01_XYZ-12-31_class_v1.EXT"

    stringr::str_replace_all(template, "XYZ", as.character(year)) |>
        stringr::str_replace("EXT", ext)
}

.terraclass_download <- function(year, output_dir) {
    # Get file metadata of the selected file year
    tc_year_file <- .terraclass_file_metadata(year = year)

    # Define output dir
    output_file <- fs::path(output_dir) / .terraclass_file_zip(tc_year_file)

    # If file already exists, return it!
    if (fs::file_exists(output_file)) {
        return(output_file)
    }

    # Download file
    download.file(tc_year_file[["file"]], output_file)

    # Return new file
    output_file
}

.terraclass_extract_files <- function(year, file, output_dir) {
    # Fix output type
    output_dir <- fs::path(output_dir)

    # Get files to be extracted
    files_zip <- utils::unzip(file, list = TRUE) |>
        dplyr::rename("name"   = "Name",
                      "length" = "Length",
                      "date"   = "Date") |>
        dplyr::mutate(
            file      = output_dir / .data[["name"]],
            file_ext  = ifelse(fs::path_ext(.data[["name"]]) == "qml", "style", "raster"),
            file_sits = output_dir / .terraclass_file_sits_name(year, ext = fs::path_ext(.data[["name"]]))
        ) |>
        dplyr::mutate(
            file_available = fs::file_exists(.data[["file_sits"]])
        )

    # Check if raster file is available
    if (all(unname(files_zip[["file_available"]]))) {
        return(
            tibble::tibble(
                file      = files_zip[["file_sits"]],
                type      = files_zip[["file_ext"]],
                processed = TRUE
            )
        )
    }

    # Extract files
    utils::unzip(file, exdir = output_dir)

    # Rename files
    purrr::map_dfr(seq_len(nrow(files_zip)), function(idx) {
        # Get file metadata
        file_row <- files_zip[idx, ]

        # Rename file
        fs::file_move(file_row[["file"]], file_row[["file_sits"]])

        tibble::tibble(
            file      = file_row[["file_sits"]],
            type      = file_row[["file_ext"]],
            processed = FALSE
        )
    })
}

#' @export
prepare_terraclass <- function(years, region_id, multicores = 1, version = "v2") {
    # Setup multisession workers
    future::plan(future::multisession, workers = multicores, seed = TRUE)

    # Download all specified years
    furrr::future_map(years, function(year) {
        # Define output dir
        output_dir <- .terraclass_dir(year = year, version = version)

        # Create directory
        fs::dir_create(output_dir)

        # Download year file
        output_file <- .terraclass_download(year = year, output_dir = output_dir)

        # Extract files from zip
        extracted_files <- .terraclass_extract_files(year       = year,
                                                     file       = output_file,
                                                     output_dir = output_dir)

        # Check if files are already processed
        are_files_finished <- all(unname(extracted_files[["processed"]]))

        # If no, crop raster using the eco region selected by the user
        if (!are_files_finished) {
            # Get eco region polygon
            eco_region_roi <- roi_ecoregions(
                region_id  = region_id,
                crs        = "EPSG:4674",
                as_union   = TRUE,
                as_file    = TRUE,
                use_buffer = TRUE
            )

            eco_region_roi <- terra::vect(eco_region_roi)

            # Select raster file
            raster_file <- dplyr::filter(extracted_files, type == "raster")
            raster_file <- raster_file[["file"]]

            # Define temporary file
            raster_file_out <- stringr::str_replace(
                raster_file, "v1.tif", "v1-cropped.tif"
            )

            raster_object <- terra::rast(raster_file)
            raster_datatype <- terra::datatype(raster_object)

            # Reproject polygon to raster CRS
            vector_object <- terra::project(
                x = eco_region_roi,
                y = terra::crs(raster_object)
            )

            # Crop raster files in a given eco region
            terra::crop(
                x        = raster_object,
                y        = vector_object,
                filename = raster_file_out,
                NAflag   = 0,
                mask     = TRUE
            )

            # After crop, remove original one
            fs::file_delete(raster_file)

            # Renamed cropped
            fs::file_move(raster_file_out, raster_file)
        }

        # Return!
        dplyr::select(extracted_files, -.data[["processed"]])
    })
}
