
#' @export
cube_generate_indices_bdc <- function(cube, output_dir, multicores, memsize) {
    # Generate NDVI
    cube <- sits_apply(
        data       = cube,
        NDVI       = (NIR08 - RED) / (NIR08 + RED),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate EVI (https://www.usgs.gov/landsat-missions/landsat-enhanced-vegetation-index)
    cube <- sits_apply(
        data       = cube,
        EVI        = 2.5 * ((NIR08 - RED) / (NIR08 + 6 * RED - 7.5 * BLUE + 1)),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate MNDWI
    cube <- sits_apply(
        data       = cube,
        MNDWI      = (GREEN - SWIR16) / (GREEN + SWIR16),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate NBR (https://www.usgs.gov/landsat-missions/landsat-normalized-burn-ratio)
    cube <- sits_apply(
        data       = cube,
        NBR        = (NIR08 - SWIR22) / (NIR08 + SWIR22),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    return(cube)
}

#' @export
cube_generate_indices_glad <- function(cube, output_dir, multicores, memsize) {
    # Generate NDVI
    cube <- sits_apply(
        data       = cube,
        NDVI       = (NIR - RED) / (NIR + RED),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate EVI (https://www.usgs.gov/landsat-missions/landsat-enhanced-vegetation-index)
    cube <- sits_apply(
        data       = cube,
        EVI        = 2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1)),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate MNDWI
    cube <- sits_apply(
        data       = cube,
        MNDWI      = (GREEN - SWIR1) / (GREEN + SWIR1),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # 1.6. Generate NBR (https://www.usgs.gov/landsat-missions/landsat-normalized-burn-ratio)
    cube <- sits_apply(
        data       = cube,
        NBR        = (NIR - SWIR2) / (NIR + SWIR2),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    return(cube)
}
