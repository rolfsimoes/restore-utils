#' @export
contextual_cleaner <- function(cube,
                               window_size = 3L,
                               target_class = 1L,
                               mode_class = 2L,
                               memsize = 4L,
                               multicores = 2L,
                               output_dir,
                               version = "v1",
                               progress = TRUE) {
    stopifnot(!is.null(target_class))
    stopifnot(!is.null(mode_class))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Get block size
    block <- sits:::.raster_file_blocksize(
        sits:::.raster_open_rast(sits:::.tile_path(cube))
    )
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = overlap),
        npaths = 1L, nbytes = 8L,
        proc_bloat = sits:::.conf("processing_bloat")
    )

    # Get input band
    band <- sits:::.cube_bands(cube)
    # Update multicores parameter
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = sits:::.tile_size(sits:::.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)

    # Process each tile sequentially
    clean_cube <- sits:::.cube_foreach_tile(cube, function(tile) {
        # Process the data
        .contextual_cleaner_tile(
            tile = tile,
            block = block,
            band = band,
            window_size = window_size,
            target_class = target_class,
            mode_class = mode_class,
            overlap = overlap,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
    # Update cube class and return
    sits:::.set_class(clean_cube, "class_cube", class(clean_cube))
}

.contextual_cleaner_tile <- function(tile,
                                     block,
                                     band,
                                     window_size,
                                     target_class,
                                     mode_class,
                                     overlap,
                                     output_dir,
                                     version,
                                     progress) {
    # Output file
    out_file <- sits:::.file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume tile
    if (sits:::.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        sits:::.check_recovery()
        # Create tile based on template
        tile <- sits:::.tile_derived_from_file(
            file = out_file, band = band,
            base_tile = tile, derived_class = sits:::.tile_derived_class(tile),
            labels = sits:::.tile_labels(tile),
            update_bbox = FALSE
        )
        return(tile)
    }
    # Create chunks as jobs
    chunks <- sits:::.tile_chunks_create(tile = tile, overlap = overlap, block = block)
    # Process jobs sequentially
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- sits:::.block(chunk)
        # Block file name for each fraction
        block_files <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (sits:::.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- sits:::.clean_data_read(
            tile = tile, block = block, band = band
        )
        # Apply kernel modal
        values <- restoreutils:::C_context_cleaner(
            x = as.matrix(values),
            ncols = block[["ncols"]],
            nrows = block[["nrows"]],
            band = 0L,
            window_size = window_size,
            target_class = target_class,
            mode_class = mode_class
        )
        # Prepare fractions to be saved
        band_conf <- sits:::.tile_band_conf(tile = tile, band = band)
        # Job crop block
        crop_block <- sits:::.block(sits:::.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_files, block = block, bbox = sits:::.bbox(chunk),
            values = values, data_type = sits:::.data_type(band_conf),
            missing_value = sits:::.miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    }, progress = progress)
    # Merge blocks into a new class_cube tile
    sits:::.tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = sits:::.tile_labels(tile),
        base_tile = tile,
        derived_class = sits:::.tile_derived_class(tile),
        block_files = block_files,
        multicores = 1L,
        update_bbox = FALSE
    )
}
