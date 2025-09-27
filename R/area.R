
#' @export
cube_pixel_frequency <- function(cube, multicores, memsize) {
    # Get cube labels
    labels <- sits:::.cube_labels(cube)
    # The following functions define optimal parameters for parallel processing
    # Get block size
    block <- sits:::.tile_path(cube) |>
                sits:::.raster_open_rast() |>
                sits:::.raster_file_blocksize()
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0L),
        npaths = length(labels),
        nbytes = 8L,
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
        image_size = sits:::.tile_size(sits:::.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallel processing
    if (is.null(sits:::sits_env[["cluster"]])) {
        sits:::.parallel_start(workers = multicores)
        on.exit(sits:::.parallel_stop(), add = TRUE)
    }
    # Extract pixel frequency from all tiles
    cube_values <- slider::slide(cube, function(tile) {
        # Generate tile chunks
        chunks <- sits:::.tile_chunks_create(
            tile = tile,
            overlap = 0L,
            block = block
        )
        # Get tile path
        tile_path <- sits:::.tile_path(tile)
        # Get pixel frequency from chunks
        sits:::.jobs_map_parallel_dfr(chunks, function(chunk) {
            # Get chunk block
            chunk_block <- sits:::.block(chunk)
            # Open raster and crop metadata
            chunk_raster <- sits:::.raster_open_rast(tile_path)
            chunk_raster <- sits:::.raster_crop_metadata(
                rast = chunk_raster,
                block = chunk_block
            )
            # Chunk frequency
            sits:::.raster_freq(chunk_raster)
        }, progress = TRUE)
    })
    # Count values
    cube_values |>
        dplyr::bind_rows() |>
        dplyr::group_by(.data[["value"]]) |>
        dplyr::summarise(size = sum(.data[["count"]]), .groups = "drop") |>
        dplyr::mutate(label = labels[as.character(.data[["value"]])]) |>
        dplyr::select(.data[["label"]], .data[["size"]])
}

#' @export
calculate_area_by_class <- function(cube, multicores, memsize, res = NULL) {
    # Get cube resolution
    cube_xres <- sits:::.cube_xres(cube)
    cube_yres <- sits:::.cube_yres(cube)

    # If `res` is defined, use it
    if (!is.null(res)) {
        cube_xres <- res
        cube_yres <- res
    }

    # Extract values frequency
    cube_pixel_freq <- cube_pixel_frequency(
        cube       = cube,
        multicores = multicores,
        memsize    = memsize
    )

    # Calculate area
    cube_pixel_freq |>
        dplyr::mutate(
            area = .data[["size"]] * cube_xres * cube_yres
        ) |>
        dplyr::select(
            .data[["label"]], .data[["area"]], .data[["size"]]
        )
}
