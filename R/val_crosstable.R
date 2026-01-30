#' @export
crosstable <- function(map, ref, output_dir, multicores = 10, memsize = 16) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # The following functions define optimal parameters for parallel processing
    file <- sits:::.tile_path(map)
    rast_template <- sits:::.raster_open_rast(file)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(file))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(file),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Update chunk to save extra information
    chunks[["map"]] <- map
    chunks[["ref"]]  <- ref
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    data <- sits:::.jobs_map_parallel_dfr(chunks, function(chunk) {
        # Get job block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        map  <- chunk[["map"]]
        ref  <- chunk[["ref"]]
        out_file <- chunk[["file"]]
        # Output file name
        block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Output mask file name
        mask_block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file, suffix = "_mask"),
            block = block, output_dir = output_dir
        )
        # If there is any mask file delete it
        unlink(mask_block_file)
        # Resume processing in case of failure
        if (sits:::.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Project mask block to template block
        # Get band conf missing value
        band = "class"
        band_conf <- sits:::.conf_derived_band(
            derived_class = "class_cube", band = band
        )
        # Create template block for mask
        sits:::.gdal_template_block(
            block = block, bbox = sits:::.bbox(chunk), file = mask_block_file,
            nlayers = 1L, miss_value = sits:::.miss_value(band_conf),
            data_type = sits:::.data_type(band_conf)
        )
        # Copy values from mask cube into mask template
        sits:::.gdal_merge_into(
            file = mask_block_file,
            base_files = sits:::.fi_paths(sits:::.fi(ref)), multicores = 1L
        )
        # Build a new tile for mask based on template
        mask_tile <- sits:::.tile_derived_from_file(
            file = mask_block_file,
            band = "class",
            base_tile = sits:::.tile(ref),
            derived_class = "class_cube",
            update_bbox = FALSE
        )
        # Read and preprocess values
        map_values <- sits:::.tile_read_block(
            tile = map, band = sits:::.tile_bands(map), block = block
        )
        # Read and preprocess values of mask block
        ref_values <- sits:::.tile_read_block(
            tile = mask_tile, band = sits:::.tile_bands(mask_tile), block = NULL
        )
        # Calculate contingency table
        tbl <- as.data.frame(
            table(map_values, ref_values)
        )
        # Delete unneeded mask block file
        unlink(mask_block_file)
        # Free memory
        gc()
        # Returned value
        tbl
    }, progress = TRUE)
    # Summarize!
    data |>
        dplyr::group_by(.data[["map_values"]], .data[["ref_values"]]) |>
        dplyr::summarise(n = sum(.data[["Freq"]]), .groups = 'drop')
}

#' @export
crosstable_named <- function(map, reference, map_name, reference_name, multicores = 10, memsize = 16, data_dir = NULL) {
    # Get maps labels
    map_labels <- sits_labels(map)
    ref_labels <- sits_labels(reference)

    current_col_name <- glue::glue("_{map_name}")
    next_col_name <- glue::glue("_{reference_name}")

    # Define directory
    if (is.null(data_dir)) {
        data_dir <- tempdir()
    }

    # Calculate crosstable
    crosstable(
        map        = map,
        ref        = reference,
        multicores = multicores,
        memsize    = memsize,
        output_dir = data_dir
    ) |>
        dplyr::rename(
            "current" = "map_values",
            "next"    = "ref_values"
        ) |>
        dplyr::mutate(
            "current" = as.factor(.data[["current"]]),
            "next" = as.factor(.data[["next"]]),
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            "current" = (
                stringr::str_to_title(
                    map_labels[[  levels(.data[["current"]])[.data[["current"]]]  ]]
                ) |>
                    stringr::str_c(glue::glue("_{map_name}"))
            ),
            "next"  = (
                stringr::str_to_title(
                    ref_labels[[  levels(.data[["next"]])[.data[["next"]]]  ]]
                ) |>
                    stringr::str_c(glue::glue("_{reference_name}"))
            )
        ) |>
        dplyr::rename(
            !!current_col_name := "current",
            !!next_col_name := "next"
        ) |>
        tidyr::pivot_wider(
            names_from = !!current_col_name,
            values_from = n,
            values_fill = 0
        )
}

