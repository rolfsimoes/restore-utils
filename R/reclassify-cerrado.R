#' @export
reclassify_cer_rule0_natveg <- function(cube, mask, multicores, memsize, output_dir, version, exclude_mask_na = FALSE) {

    # Labels that will not be included in mask
    not_included_labels <- c(
        "31 - Área Antropizada",
        "32 - Corpos d'Água",
        "34 - Não Observado"
    )

    mask_labels <-  c("1"  = "1 - Formação Florestal",
                               "3"  = "3 - Formação Florestal",
                               "11" = "11 - Vegetação Natural (Cerrado Denso)",
                               "12" = "12 - Vegetação Natural (Cerrado Típico)",
                               "13" = "13 - Vegetação Natural (Cerrado Ralo)",
                               "14" = "14 - Vegetação Natural (Cerrado Rupestre)",
                               "15" = "15 - Vegetação Natural (Parque de Cerrado)",
                               "17" = "17 - Vegetação Natural (Babaçual)",
                               "19" = "19 - Vegetação Natural (Vereda)",
                               "21" = "21 - Campo Natural (Campo Sujo)",
                               "25" = "25 - Campo Natural (Campo Limpo)",
                               "29" = "29 - Campo Natural (Campo Rupestre)",
                               "31" = "31 - Área Antropizada",
                               "32" = "32 - Corpos d'Água",
                               "34" = "34 - Não Observado",
                               "37" = "37 - Desmatamento do Ano",
                               "38" = "38 - Duna",
                               "39" = "39 - Floresta (Mangue)",
                               "40" = "40 - Floresta (Savana-Estépica Florestada)",
                               "41" = "41 - Vegetação Natural (Savana-Estépica Arborizada)",
                               "43" = "43 - Campo Natural (Savana-Estépica Gramíneo-Lenhosa)",
                               "44" = "44 - Depósito Fluvial"
    )

    # Filter labels to be included in mask
    labels_to_mask <- setdiff(
        mask_labels, not_included_labels
    )

    # Build rules expression: each label will be a class
    expressions <- lapply(labels_to_mask, function(label) {
        bquote(mask == .(label))
    })

    names(expressions) <- labels_to_mask

    rules_expression <- as.call(c(bquote(list), expressions))

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
