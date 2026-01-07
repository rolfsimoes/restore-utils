
#' @export
reclassify_mask <- function(rules, region_id, mask_version, classification_version, classification_year, multicores = 35, memsize = 160) {
    # Validate `rules` tibble
    required_columns <- c("order", "fun", "mask", "args", "version")
    missing_columns  <- setdiff(required_columns, names(rules))

    if (length(missing_columns)) {
        cli::cli_abort(paste0("`rules` is missing columns: ", paste(missing_columns, collapse = ", ")))
    }

    # Define base dirs
    base_masks_dir           <- project_masks_dir()
    base_classifications_dir <- project_classifications_dir()

    # Define classification directory
    classification_dir <- (
        base_classifications_dir / classification_version / classification_year
    )

    # Define output directory
    output_dir <- create_data_dir(
        base_masks_dir / mask_version, classification_year
    )

    # Define eco region roi
    eco_region_roi <- roi_amazon_regions(
        region_id  = region_id,
        crs        = crs_bdc(),
        as_union   = TRUE,
        use_buffer = TRUE
    )

    # Load classification
    loader <- load_restore_map_bdc

    if (classification_year < 2015) {
        loader <- load_restore_map_glad
    }

    # Ensure correct types and order
    rules <- rules |>
        dplyr::mutate(order = as.integer(.data[["order"]])) |>
        dplyr::arrange(.data[["order"]])

    # Load classification
    eco_class <- loader(
        data_dir   = classification_dir,
        multicores = multicores,
        memsize    = memsize,
        version    = classification_version,
        tiles      = "MOSAIC"
    )

    # First step: Clean
    eco_mask <- sits::sits_clean(
        cube         = eco_class,
        window_size  = 5,
        multicores   = multicores,
        memsize      = memsize,
        output_dir   = output_dir,
        version      = "start-clean"
    )

    # Apply rules
    purrr::map(seq_len(nrow(rules)), function(rule_idx) {
        rule <- rules[rule_idx,]

        mask_fn <- rule[["mask"]][[1]]
        rule_fn  <- rule[["fun"]][[1]]
        rule_args <- rule[["args"]]
        version  <- rule[["version"]]

        # Load mask
        mask_obj <- mask_fn(
            multicores = multicores,
            memsize    = memsize
        )

        # Prepare arguments
        args <- list(
            cube       = eco_mask,
            mask       = mask_obj,
            multicores = multicores,
            memsize    = memsize,
            output_dir = output_dir,
            version    = version
        )

        if (!is.null(unlist(rule_args))) {
            args <- c(args, rule_args)
        }

        # call the rule with current cube
        eco_mask <- do.call(rule_fn, args)
    })

    # Final step: Mosaic to crop using the eco region roi
    eco_mask <- sits::sits_mosaic(
        cube       = eco_mask,
        crs        = crs_bdc(),
        roi        = eco_region_roi,
        multicores = multicores,
        output_dir = output_dir,
        version    = "final-mosaic"
    )

    # Save rules in the resulting cube
    eco_mask[["rules"]] <- list(rules)

    # Save RDS
    saveRDS(eco_mask, output_dir / "mask-cube.rds")

    # Add overviews
    sf::gdal_addo(eco_mask[["file_info"]][[1]][["path"]])

    # Return!
    return(eco_mask)
}
