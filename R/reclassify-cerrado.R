#' @export
reclassify_cer_rule0_natveg <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {

    # Labels that will not be included in mask
    not_included_labels <- c(
        "31 - Área Antropizada",
        "32 - Corpos d'Água",
        "34 - Não Observado"
    )

    # Filter labels to be included in mask
    labels_to_mask <- setdiff(
        unname(sits::sits_labels(cube)), not_included_labels
    )

    # Build rules expression: each label will be a class
    rules_expression <- setNames(
        lapply(labels_to_mask, function(label) {
            bquote(mask == .(label))
        }),
        labels_to_mask
    )

    # reclassify!
    cube <- eval(bquote(
        sits::sits_reclassify(
            cube = cube,
            mask = mask,
            rules = .(rules_expression),
            exclude_mask_na = exclude_mask_na,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = version
        )
    ))

    .reclassify_save_rds(cube, output_dir, version)
}
