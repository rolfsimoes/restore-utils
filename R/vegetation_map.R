#' @export
load_vegmap <- function(year, multicores = 10, memsize = 16,
                             data_dir = "data/raw/masks/vegmap/") {
    vegmap_dir <- fs::path(data_dir) / year
    vegmap_rds <- vegmap_dir / "vegmap.rds"

    if (fs::file_exists(vegmap_rds)) {

        vegmap <- readRDS(vegmap_rds)

    } else {

        vegmap <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = vegmap_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("1"  = "1 - Formação Florestal",
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
        )

        saveRDS(vegmap, vegmap_rds)
    }
    vegmap
}
