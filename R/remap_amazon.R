
#' @export
restore_amazon_mapping_pre_2015_files <- function(files, labels_2008, labels_2009) {
    valid_years_for_conversion <- which(files[["year"]] %in% c(2008, 2010, 2012, 2014))

    tibble::tibble(
        source = character(),
        target = character(),
        indices = list()
    ) |>
        tibble::add_row(source = "2ciclos",                target = "2ciclos",            indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Agr. Semiperene",        target = "Agr. Semiperene",    indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Forest",                 target = "Forest",             indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Mountainside_Forest",    target = "Mountainside_Forest",indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "past_arbustiva",         target = "past_arbustiva",     indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "past_herbacea",          target = "past_herbacea",      indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Riparian_Forest",        target = "Riparian_Forest",    indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Seasonally_Flooded_ICS", target = "Seasonally_Flooded_ICS", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Silvicultura",           target = "Silvicultura",       indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "vegetacao_secundaria",   target = "vegetacao_secundaria", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Wetland_ICS",            target = "Wetland_ICS",        indices = list(valid_years_for_conversion)) |>
        # Source 12
        tibble::add_row(source = "Pasture_Wetland",        target = "Pasture_Wetland",    indices = list(valid_years_for_conversion)) |>
        # Source 13
        tibble::add_row(source = "pasto_silvicultura",     target = "pasto_silvicultura", indices = list(valid_years_for_conversion)) |>
        # Source 14
        tibble::add_row(source = "pasto_semiperene_2",     target = "pasto_semiperene_2", indices = list(valid_years_for_conversion)) |>
        # Source 15
        tibble::add_row(source = "pasture_deforestation_in_nonforest", target = "pasture_deforestation_in_nonforest", indices = list(valid_years_for_conversion)) |>
        # Source 16
        tibble::add_row(source = "mineracao",              target = "mineracao",          indices = list(valid_years_for_conversion)) |>
        # Source 17
        tibble::add_row(source = "area_urbanizada",        target = "area_urbanizada",    indices = list(valid_years_for_conversion)) |>
        # Source 18
        tibble::add_row(source = "pasture_annual_agriculture", target = "pasture_annual_agriculture", indices = list(valid_years_for_conversion)) |>
        # Source 19 -> Target 18
        tibble::add_row(source = "vs_herbacea_pasture",    target = "pasture_annual_agriculture", indices = list(valid_years_for_conversion)) |>
        # Source 20 -> Target 18
        tibble::add_row(source = "vs_pasture",             target = "pasture_annual_agriculture", indices = list(valid_years_for_conversion)) |>
        # Source 21 -> Target 19
        tibble::add_row(source = "deforest_year",          target = "deforest_year",      indices = list(valid_years_for_conversion)) |>
        # Source 22 -> Target 20
        tibble::add_row(source = "nat_non_forest",         target = "nat_non_forest",     indices = list(valid_years_for_conversion)) |>
        # Source 23 -> Target 21
        tibble::add_row(source = "agua",                   target = "agua",               indices = list(valid_years_for_conversion)) |>
        # Source 24 -> Target 22
        tibble::add_row(source = "Perene",                 target = "Perene",             indices = list(valid_years_for_conversion)) |>
        dplyr::mutate(
            source = as.numeric(labels_2008[as.character(.data[["source"]])]),
            target = as.numeric(labels_2009[as.character(.data[["target"]])])
        )

}

#' @export
restore_amazon_mapping_post_2015_files <- function(files, labels_2018, labels_2019) {
    valid_years_for_conversion <- which(files[["year"]] %in% c(2018, 2020, 2022))

    tibble::tibble(
        source  = character(),
        target  = character(),
        indices = list()
    ) |>
        tibble::add_row(source = "2ciclos",            target = "2ciclos",            indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Agr. Semiperene",    target = "Agr. Semiperene",    indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "agua",               target = "agua",               indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Forest",             target = "Forest",             indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Mountainside_Forest",target = "Mountainside_Forest",indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "past_arbustiva",     target = "past_arbustiva",     indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "past_herbacea",      target = "past_herbacea",      indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Riparian_Forest",    target = "Riparian_Forest",    indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Seasonally_Flooded_ICS", target = "Seasonally_Flooded_ICS", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Silvicultura",       target = "Silvicultura",       indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "vegetacao_secundaria", target = "vegetacao_secundaria", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Wetland_ICS",        target = "Wetland_ICS",        indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Pasture_Wetland",    target = "Pasture_Wetland",    indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "pasto_silvicultura", target = "pasto_silvicultura", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "pasto_semiperene",   target = "pasto_semiperene",   indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "pasture_deforestation_in_nonforest", target = "pasture_deforestation_in_nonforest", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "mineracao",          target = "mineracao",          indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "area_urbanizada",    target = "area_urbanizada",    indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "pasture_annual_agriculture", target = "pasture_annual_agriculture", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "vs_herbacea_pasture", target = "pasture_annual_agriculture", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "vs_pasture",         target = "pasture_annual_agriculture", indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "deforest_year",      target = "deforest_year",      indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "nat_non_forest",     target = "nat_non_forest",     indices = list(valid_years_for_conversion)) |>
        tibble::add_row(source = "Perene",             target = "Perene",             indices = list(valid_years_for_conversion)) |>
        dplyr::mutate(
            source = as.numeric(labels_2018[as.character(.data[["source"]])]),
            target = as.numeric(labels_2019[as.character(.data[["target"]])])
        )
}

#' @export
restore_amazon_mapping_reference_table <- function() {
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
        tibble::add_row(source = "vs_herbacea_pasture" , target = 104) |>
        tibble::add_row(source = "vs_pasture" , target = 104) |>

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
restore_amazon_mapping_release_table <- function() {
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
