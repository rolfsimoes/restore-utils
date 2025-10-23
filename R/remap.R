#' @export
restore_mapping_reference_table <- function() {
    tibble::tibble(source = character(), target = numeric()) |>
        tibble::add_row(source = "2ciclos"                , target = 100) |>
        tibble::add_row(source = "Agr. Semiperene"        , target = 101) |>
        tibble::add_row(source = "agua"                   , target = 102) |>

        tibble::add_row(source = "Forest"                 , target = 103) |>
        tibble::add_row(source = "Mountainside_Forest"    , target = 103) |>
        tibble::add_row(source = "Riparian_Forest"        , target = 103) |>

        tibble::add_row(source = "Pasture_Wetland"        , target = 104) |>
        tibble::add_row(source = "past_arbustiva"         , target = 104) |>
        tibble::add_row(source = "past_herbacea"          , target = 104) |>
        tibble::add_row(source = "pasto_silvicultura"     , target = 104) |>
        tibble::add_row(source = "pasto_semiperene"       , target = 104) |>
        tibble::add_row(source = "pasto_semiperene_2"       , target = 104) |>
        tibble::add_row(source = "pasture_annual_agriculture" , target = 104) |>
        tibble::add_row(source = "pasture_deforestation_in_nonforest" , target = 104) |>

        tibble::add_row(source = "Seasonally_Flooded_ICS" , target = 105) |>
        tibble::add_row(source = "Wetland_ICS"            , target = 105) |>

        tibble::add_row(source = "Silvicultura"           , target = 106) |>
        tibble::add_row(source = "vegetacao_secundaria"   , target = 107) |>

        tibble::add_row(source = "mineracao"              , target = 108) |>
        tibble::add_row(source = "area_urbanizada"        , target = 109) |>

        tibble::add_row(source = "deforest_year"          , target = 110) |>
        tibble::add_row(source = "nat_non_forest"         , target = 111) |>

        tibble::add_row(source = "Perene"                 , target = 112)
}

#' @export
restore_mapping_release_table <- function() {
    tibble::tibble(source = numeric(), target = numeric()) |>
        tibble::add_row(source = 100, target = 1)  |>  # "2ciclos"
        tibble::add_row(source = 101, target = 2)  |>  # "Agr. Semiperene"
        tibble::add_row(source = 102, target = 3)  |>  # "agua"
        tibble::add_row(source = 103, target = 4)  |>  # "Forest"
        tibble::add_row(source = 106, target = 5)  |>  # "Silvicultura"
        tibble::add_row(source = 107, target = 6)  |>  # "vegetacao_secundaria"
        tibble::add_row(source = 108, target = 7)  |>  # "mineracao"
        tibble::add_row(source = 109, target = 8)  |>  # "area_urbanizada"
        tibble::add_row(source = 111, target = 9)  |>  # "nat_non_forest"
        tibble::add_row(source = 104, target = 10) |>  # "Pasture"
        tibble::add_row(source = 105, target = 11) |>  # "Wetlands"
        tibble::add_row(source = 110, target = 12) |>  # "desmatamento"
        tibble::add_row(source = 112, target = 13)     # "Perene"
}

#' @export
cube_remap <- function(cube, output_dir, multicores, memsize, mapping_reference = NULL, rules = NULL) {
    # Define mapping reference when not exists
    if (is.null(mapping_reference)) {
        mapping_reference <- restore_mapping_reference_table()
    }

    # Define output dir
    output_dir <- fs::path(output_dir)

    # Create directory
    fs::dir_create(output_dir)

    # For tile remap
    purrr::map_dfr(seq_len(nrow(cube)), function(idx) {
        # Define tile
        tile <- cube[idx, ]

        # 1. Get files
        file_path <- tile[["file_info"]][[1]][["path"]]

        # Define output file
        file_out <- output_dir / fs::path_file(file_path)

        # Update tile label
        tile[["file_info"]][[1]][["path"]] <- file_out

        # If file exists, return it
        if (fs::file_exists(file_out)) {
            return(tile)
        }

        # Define file rules
        if (is.null(rules)) {
            rules <- .build_remap_table(tile, mapping_reference)
        }

        # Remap!
        file_out <- reclassify_remap_pixels(
            file       = file_path,
            file_out   = file_out,
            rules      = rules,
            multicores = multicores,
            memsize    = memsize,
            output_dir = output_dir
        )

        # Create overviews
        sf::gdal_addo(file_out)

        # Return!
        tile
    })
}

.build_remap_table <- function(cube, truth_table) {
    cube_labels <- sits::sits_labels(cube)

    cube_labels_remap <- purrr::map_dfr(seq_len(length(cube_labels)), function(idx) {
        cube_label <- cube_labels[idx]

        cube_label_name <- as.character(cube_label)
        cube_label_value <- as.numeric(names(cube_label))

        cube_label_target <- truth_table[truth_table[["source"]] == cube_label_name, "target"][["target"]]

        tibble::tibble(
            source = cube_label_value,
            target = cube_label_target
        )
    })

    if (nrow(cube_labels_remap) != length(cube_labels)) {
        cli::cli_abort("Error: There is an error in the mapping. Please, check it")
    }

    # Return!
    return(cube_labels_remap)
}
