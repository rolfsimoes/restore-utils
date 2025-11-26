#' @export
cube_to_rgb_mosaic_ogh <- function(cube,
                                   output_dir,
                                   multicores = 64,
                                   bands = NULL,
                                   roi_file = NULL) {
    # convert output_dir to fs
    output_dir <- fs::path(output_dir)

    # create base output dirs
    output_mosaic_tile_dir <- output_dir / "tiles"
    output_mosaic_complete_dir <- output_dir / "cube"

    # mosaic by tile
    print("Processing tiles")
    mosaic <- purrr::map_dfr(seq_len(nrow(cube)), function(tile_idx) {
        # select tile
        tile <- cube[tile_idx, ]

        # extract tile crs
        tile_crs <- unique(cube[["crs"]])

        # define output directory
        tile_dir <- output_mosaic_tile_dir / tile[["tile"]]

        # create dir
        fs::dir_create(tile_dir, recurse = TRUE)

        # mosaic
        sits_mosaic(
            cube       = tile,
            multicores = multicores,
            output_dir = tile_dir,
            crs        = tile_crs
        )
    })

    print("Finished - tiles are now ready")

    # create output dir to the whole area
    fs::dir_create(output_mosaic_complete_dir, recurse = TRUE)

    # get mosaic timeline
    mosaic_timeline <- sits_timeline(mosaic)

    # merge mosaic file infos
    mosaic_files <- dplyr::bind_rows(mosaic[["file_info"]])

    # mosaic by date
    mosaic_files <- purrr::map_vec(mosaic_timeline, function(timeline_date) {
        # define output file
        mosaic_file <- output_mosaic_complete_dir / paste0(timeline_date, ".tif")
        mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")

        # if file exists, return it
        if (fs::file_exists(mosaic_mbtiles)) {
            return(mosaic_mbtiles)
        }

        # filter files by date
        tiles_in_date <- mosaic_files  |>
            dplyr::filter(date == timeline_date) |>
            dplyr::mutate(band = factor(band, levels = bands)) |>
            dplyr::arrange(band)

        # process files by `fid` (assuming 3 bands per fid)
        vrt_files <- dplyr::group_by(tiles_in_date, .data[["fid"]]) |>
            dplyr::group_map(function(group_fid, key) {
                # define vrt file path
                vrt_file <- fs::path(paste0(fs::file_temp(), ".vrt"))

                # create vrt
                sf::gdal_utils(
                    util = "buildvrt",
                    source = group_fid[["path"]],
                    destination = vrt_file,
                    options = c("-separate")
                )

                # return!
                vrt_file
            }) |>
            unlist()

        # define vrt list file
        vrt_files_lst <- fs::file_temp(ext = "txt")

        # write vrt files to list file
        readr::write_lines(vrt_files, file = vrt_files_lst)

        # build vrt (using system as sf was raising errors)
        vrt_merged <- fs::file_temp(ext = "vrt")

        system(paste(
            "gdalbuildvrt -input_file_list",
            vrt_files_lst,
            vrt_merged,
            sep = " "
        ))

        # translate
        rgb_file <- output_mosaic_complete_dir / "mosaic-rgb.tif"

        # scale image colors

        # tests:
        # -scale 0 40
        # no scale
        # -scale 0 1

        system(
            paste(
                "gdal_translate -ot Byte -a_nodata 255 -scale -b 1 -b 2 -b 3",
                vrt_merged,
                rgb_file,
                sep = " "
            )
        )

        # warp
        if (!is.null(roi_file)) {
            system(paste(
                "gdalwarp -dstalpha -cutline", roi_file, "-crop_to_cutline", rgb_file, rgb_file, "-overwrite", sep = " "
            ))
        }

        # create mbtiles
        system(paste(
            "gdal_translate -of MBTILES",
            rgb_file,
            mosaic_mbtiles,
            sep = " "
        ))

        # add mbtiles zoom
        system(paste("gdaladdo -r average ", mosaic_mbtiles, "2 4 8 16 32", sep = " "))

        # delete temp files
        fs::file_delete(rgb_file)

        # return
        mosaic_mbtiles
    })

    # return!
    return(mosaic_files)
}


#' @export
cube_to_rgb_mosaic_bdc <- function(cube,
                                   output_dir,
                                   multicores = 64,
                                   bands = NULL,
                                   roi_file = NULL,
                                   mbtiles = FALSE) {
    # convert output_dir to fs
    output_dir <- fs::path(output_dir)

    # create base output dirs
    output_mosaic_tile_dir <- output_dir / "tiles"
    output_mosaic_complete_dir <- output_dir / "cube"

    # mosaic by tile
    print("Processing tiles")
    mosaic <- purrr::map_dfr(seq_len(nrow(cube)), function(tile_idx) {
        # select tile
        tile <- cube[tile_idx, ]

        # extract tile crs
        tile_crs <- unique(cube[["crs"]])

        # define output directory
        tile_dir <- output_mosaic_tile_dir / tile[["tile"]]

        # create dir
        fs::dir_create(tile_dir, recurse = TRUE)

        # mosaic
        sits::sits_mosaic(
            cube       = tile,
            multicores = multicores,
            output_dir = tile_dir,
            crs        = tile_crs
        )
    })

    print("Finished - tiles are now ready")

    # create output dir to the whole area
    fs::dir_create(output_mosaic_complete_dir, recurse = TRUE)

    # get mosaic timeline
    mosaic_timeline <- sits::sits_timeline(mosaic)

    # merge mosaic file infos
    mosaic_files <- dplyr::bind_rows(mosaic[["file_info"]])

    # mosaic by date
    mosaic_files <- purrr::map_vec(mosaic_timeline, function(timeline_date) {
        # define output file
        mosaic_file <- output_mosaic_complete_dir / paste0(timeline_date, ".tif")

        # if file exists, return it
        if (fs::file_exists(mosaic_file)) {
            # create mbtiles if needed
            mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")
            # generate mbtiles if file doesn't exist
            if (mbtiles && !fs::file_exists(mosaic_mbtiles)) {
                system(paste(
                    "gdal_translate -of MBTILES",
                    mosaic_file,
                    mosaic_mbtiles,
                    sep = " "
                ))
                # generate cogs
                system(paste("gdaladdo -r average ", mosaic_file, "2 4 8 16 32", sep = " "))
                # update mosaic file name
                mosaic_file <- mosaic_mbtiles
            }
            return(mosaic_file)
        }

        # filter files by date
        tiles_in_date <- mosaic_files  |>
            dplyr::filter(date == timeline_date) |>
            dplyr::mutate(band = factor(band, levels = bands)) |>
            dplyr::arrange(band)

        # process files by `fid` (assuming 3 bands per fid)
        vrt_files <- dplyr::group_by(tiles_in_date, .data[["fid"]]) |>
            dplyr::group_map(function(group_fid, key) {
                # define vrt file path
                vrt_file <- fs::path(paste0(fs::file_temp(), ".vrt"))

                # create vrt
                sf::gdal_utils(
                    util = "buildvrt",
                    source = group_fid[["path"]],
                    destination = vrt_file,
                    options = c("-separate")
                )

                # return!
                vrt_file
            }) |>
            unlist()

        # define vrt list file
        vrt_files_lst <- fs::file_temp(ext = "txt")

        # write vrt files to list file
        readr::write_lines(vrt_files, file = vrt_files_lst)

        # build vrt (using system as sf was raising errors)
        vrt_merged <- fs::file_temp(ext = "vrt")

        # create vrt file
        system(paste(
            "gdalbuildvrt -input_file_list",
            vrt_files_lst,
            vrt_merged,
            sep = " "
        ))

        # create tif file
        sf::gdal_utils(
            util = "translate",
            source = vrt_merged,
            destination = mosaic_file,
            options = c(
                "-of", "GTiff",
                "-ot", "Byte",
                "-a_nodata", "0",
                "-exponent", "0.7",
                "-scale",
                "-b", "1",
                "-b", "2",
                "-b", "3",
                "-co", "BIGTIFF=YES",
                "-co", "TILED=YES",
                "-co", "COMPRESS=ZSTD",
                "-co", "PREDICTOR=1",
                "-co", "NUM_THREADS=ALL_CPUS"
            ),
            config_options = c(
                "GDAL_CACHEMAX" = "4096"
            ),
            quiet = FALSE
        )

        # warp
        if (!is.null(roi_file)) {
            sf::gdal_utils(
                util = "warp",
                source = mosaic_file,
                destination = mosaic_file,
                options = c(
                    "-dstalpha",
                    "-cutline", roi_file,
                    "-overwrite",
                    "-crop_to_cutline",
                    "-co", "BIGTIFF=YES",
                    "-co", "TILED=YES",
                    "-co", "COMPRESS=ZSTD",
                    "-co", "PREDICTOR=1",
                    "-co", "NUM_THREADS=ALL_CPUS"
                ),
                config_options = c(
                    "GDAL_CACHEMAX" = "4096"
                ),
                quiet = FALSE
            )
        }

        # create mbtiles
        if (mbtiles) {
            mosaic_mbtiles <- output_mosaic_complete_dir / paste0(timeline_date, ".mbtiles")
            system(paste(
                "gdal_translate -of MBTILES",
                mosaic_file,
                mosaic_mbtiles,
                sep = " "
            ))
            # update mosaic file name
            mosaic_file <- mosaic_mbtiles
        }
        system(paste("gdaladdo -r average ", mosaic_file, "2 4 8 16 32", sep = " "))

        # return
        mosaic_file
    })

    # return!
    return(mosaic_files)
}

#' @export
load_mosaic_glad <- function(data_dir, multicores = 32, memsize = 120) {
    mosaic_dir <- fs::path(data_dir)
    mosaic_rds <- mosaic_dir / "mosaic.rds"

    if (fs::file_exists(mosaic_rds)) {

        mosaic <- readRDS(mosaic_rds)

    } else {
        mosaic <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = mosaic_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c(
                "1"  = "Agricultura_Anual",
                "2"  = "Agricultura_Semiperene",
                "3"  = "Floresta",
                "5"  = "Pastagem_Arbustiva",
                "6"  = "Pastagem_Herbacea",
                "8"  = "Sazonalmente_Inundada_ICS",
                "9"  = "Silvicultura",
                "10" = "Vegetacao_Secundaria",
                "11" = "Area_Umida_ICS",
                "12" = "Desmatamento_Do_Ano",
                "13" = "Pasto_Wetland",
                "14" = "Pasto_Silvicultura",
                "15" = "Pasto_Semiperene",
                "16" = "Mineracao",
                "17" = "Area_Urbanizada",
                "18" = "Agua",
                "19" = "Natural_Nao_Florestal"
            )
        )

        saveRDS(mosaic, mosaic_rds)
    }
    mosaic
}


#' @export
load_mosaic_bdc <- function(data_dir, multicores = 32, memsize = 120) {
    mosaic_dir <- fs::path(data_dir)
    mosaic_rds <- mosaic_dir / "mosaic.rds"

    if (fs::file_exists(mosaic_rds)) {

        mosaic <- readRDS(mosaic_rds)

    } else {
        mosaic <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = mosaic_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c(
                "1"   = "Agricultura_Anual",
                "2"   = "Agricultura_Semiperene",
                "3"   = "Agua",
                "4"   = "Floresta",
                "6"   = "Pastagem_Arbustiva",
                "7"   = "Pastagem_Herbacea",
                "9"   = "Sazonalmente_Inundada_ICS",
                "10"  = "Silvicultura",
                "11" = "Vegetacao_Secundaria",
                "12" = "Area_Umida_ICS",
                "13" = "Desmatamento_Do_Ano",
                "14" = "Pasto_Wetland",
                "15" = "Pasto_Silvicultura",
                "16" = "Pasto_Semiperene",
                "17" = "Mineracao",
                "18" = "Area_Urbanizada",
                "19" = "Natural_Nao_Florestal"
            )
        )

        saveRDS(mosaic, mosaic_rds)
    }
    mosaic
}

