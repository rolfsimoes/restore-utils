
#' @title Get Brazil Data Cube (BDC) coordinate reference system
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Returns the CRS object of the Brazil Data Cube (BDC) data.
#'
#' @returns Character representing the Brazil Data Cube CRS
#'
#' @export
crs_bdc <- function() {
    readRDS(system.file("extdata/crs/bdc.rds", package = "restoreutils"))
}
