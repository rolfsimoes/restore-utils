
.read_qml <- function(qml_file) {
    # Reads the ".qml" file
    qml <- xml2::read_xml(qml_file)

    # Finds all "<paletteEntry>" elements inside "<colorPalette>"
    nodes <- xml2::xml_find_all(qml, ".//colorPalette/paletteEntry")
    if (length(nodes) == 0) {
        stop("The QML file does not contain any <paletteEntry> tags.")
    }

    # Extracts the "value", "color" and "label" attributes
    values <- xml2::xml_attr(nodes, "value")
    colors <- xml2::xml_attr(nodes, "color")
    labels <- xml2::xml_attr(nodes, "label")

    rgb_mat <- grDevices::col2rgb(toupper(colors))

    tibble::tibble(
        pixel = as.integer(values),
        r     = as.integer(rgb_mat["red", ]),
        g     = as.integer(rgb_mat["green", ]),
        b     = as.integer(rgb_mat["blue", ]),
        label = labels
    ) |>
    dplyr::arrange(.data[["pixel"]])
}

#' @export
rat_set_style <- function(map, qml, band = 1) {
    # Check if map exist
    if (!fs::file_exists(map)) {
        cli::cli_abort(glue::glue("{map} doesn't exist"))
    }

    # Check if file exist
    if (!fs::file_exists(qml)) {
        cli::cli_abort(glue::glue("{qml} doesn't exist"))
    }

    # Load map
    qml_style <- .read_qml(qml)

    # Prepare pixel style
    pixel_style <- qml_style |>
                    dplyr::select(
                        .data[["pixel"]],
                        .data[["r"]],
                        .data[["g"]],
                        .data[["b"]]
                    )

    # Prepare pixel labels
    pixel_labels <- qml_style |>
                    dplyr::transmute(
                        VALUE = .data[["pixel"]],
                        Name  = .data[["label"]],
                        R     = .data[["r"]],
                        G     = .data[["g"]],
                        B     = .data[["b"]]
                    )

    # Load raster
    raster <- new(gdalraster::GDALRaster, map, read_only = FALSE)

    # Set color table
    raster$setColorTable(band, pixel_style, "RGB")

    # Create RAT
    rat <- gdalraster::buildRAT(
        raster     = raster,
        band       = band,
        table_type = "thematic",
        na_value   = NULL,
        join_df    = pixel_labels
    )

    # Set raster
    raster$setDefaultRAT(band, rat)

    # Close raster
    raster$flushCache()
    raster$close()

    # Return!
    return(map)
}
