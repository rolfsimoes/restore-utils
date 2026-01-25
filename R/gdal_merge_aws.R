#' @title Check if path is an S3 path
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param path Character string representing a file path
#'
#' @returns Logical indicating whether the path is an S3 path
#' @keywords internal
.s3_gdal_is_s3_path <- function(path) {
    grepl("^s3://", path, ignore.case = TRUE)
}

#' @title Parse S3 path into bucket and key
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param s3_path Character string with S3 path (s3://bucket/key)
#'
#' @returns Named list with bucket and key components
#' @keywords internal
.s3_gdal_parse_s3_path <- function(s3_path) {
    # If it is not s3, stop it!
    if (!.s3_gdal_is_s3_path(s3_path)) {
        stop("Path must start with 's3://'")
    }

    # Get path without s3
    clean_path <- gsub("^s3://", "", s3_path)

    # Split into multiple parts
    parts <- strsplit(clean_path, "/", fixed = TRUE)[[1]]

    # It is assumed here, first is the bucket and remaining ones are the key
    list(bucket = parts[1], key = paste(parts[-1], collapse = "/"))
}

#' @title Get list of S3 keys matching pattern
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param bucket S3 bucket name
#' @param prefix Key prefix to search within
#' @param pattern File pattern to match (regex style)
#' @param recursive Logical indicating whether to search recursively
#'
#' @returns Character vector of matching S3 keys
#' @keywords internal
.s3_gdal_get_s3_keys <- function(bucket, prefix, pattern, recursive = FALSE) {
    # Get all objects with the prefix
    all_objects <- aws.s3::get_bucket_df(bucket = bucket,
                                         prefix = prefix,
                                         max = Inf)

    # If there is nothing,
    if (nrow(all_objects) == 0) {
        return(character(0))
    }

    # Get object keys
    keys <- all_objects$Key

    # Filter by recursive option
    if (!recursive) {
        # Only get files at the same level as prefix
        prefix_depth <- length(strsplit(prefix, "/")[[1]])

        keys <- keys[sapply(keys, function(k) {
            # Get the depth of the key
            key_depth <- length(strsplit(k, "/")[[1]])

            # If the key is the same depth as the prefix, or one level deeper, return TRUE
            key_depth == prefix_depth ||
                key_depth == prefix_depth + 1
        })]
    }

    # Filter by pattern
    matching_keys <- keys[grepl(pattern, basename(keys))]

    # Return!
    return(matching_keys)
}

#' @title Download S3 files to local directory
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param bucket S3 bucket name
#' @param keys Character vector of S3 keys to download
#' @param dest_dir Local directory to download files to
#'
#' @returns Character vector of local file paths
#' @keywords internal
.s3_gdal_download_s3_files <- function(bucket, keys, dest_dir) {
    # Start results file
    local_paths <- character(length(keys))

    for (i in seq_along(keys)) {
        # Prepare local file path
        local_file <- fs::path(dest_dir, basename(keys[i]))

        # If file doesn't exist, download it
        if (!fs::file_exists(local_file)) {
            # Download!
            aws.s3::save_object(object = keys[i],
                                bucket = bucket,
                                file = local_file)
        }

        # Upload result list!
        local_paths[i] <- local_file
    }

    # Return!
    return(local_paths)
}

#' @title Remove pattern from filename
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param filename Character string with filename
#' @param pattern Pattern to remove from filename
#'
#' @returns Character string with pattern removed
#' @keywords internal
.s3_gdal_remove_pattern <- function(filename, pattern) {
    # Remove matching string (with no regex marks) from the filename
    result <- gsub(pattern, "", filename)

    # If there is no file extension, add it
    if (fs::path_ext(result) == "") {
        result <- fs::path_ext_set(result, "tif")
    }

    # Return!
    return(result)
}

#' @title Merge raster files using GDAL
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param input_files Character vector of input raster file paths
#' @param output_file Output file path for merged raster
#' @param type Output raster type (GDAL Names - e.g., Bytes)
#' @param roi Optional region of interest (sf object or file path)
#'
#' @returns Path to the merged file
#' @keywords internal
.s3_gdal_merge_rasters <- function(input_files, output_file, type, roi = NULL) {
    # Validate input files exist
    missing_files <- !fs::file_exists(input_files)
    if (any(missing_files)) {
        stop("Input files not found: ",
             paste(input_files[missing_files], collapse = ", "))
    }

    # Create temporary VRT
    temp_vrt <- fs::file_temp(ext = ".vrt")

    sf::gdal_utils(util = "buildvrt",
                   source = input_files,
                   destination = temp_vrt)

    # Prepare warp options
    warp_options <- c(
        "-ot", type,
        "-of", "GTiff",
        "-co", "BIGTIFF=YES",
        "-co", "TILED=YES",
        "-co", "COMPRESS=ZSTD",
        "-co", "PREDICTOR=1",
        "-co", "NUM_THREADS=ALL_CPUS"
    )

    # Add ROI if provided
    if (!is.null(roi)) {
        # Prepare roi file
        roi_file <- .s3_gdal_process_roi(roi)

        # Add roi to the warp options
        warp_options <- c(warp_options,
                          "-crop_to_cutline",
                          "-cutline",
                          roi_file)
    }

    # Mosaic files
    sf::gdal_utils(
        util = "warp",
        source = temp_vrt,
        destination = output_file,
        options = warp_options,
        config_options = c(
            "GDAL_CACHEMAX" = "4096",
            "GDAL_TIFF_INTERNAL_MASK" = "YES"
        ),
        quiet = FALSE
    )

    # Build overviews
    sf::gdal_addo(output_file)

    # Clean up temporary VRT
    fs::file_delete(temp_vrt)

    # Return!
    return(output_file)
}

#' @title Process ROI input into file path
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param roi Region of interest (sf object or file path)
#'
#' @returns File path to ROI
#' @keywords internal
.s3_gdal_process_roi <- function(roi) {
    # Special case: sf object
    if (inherits(roi, "sf")) {
        # Save sf object to temporary file
        roi <- fs::file_temp(ext = ".gpkg")

        sf::st_write(roi, temp_roi, quiet = TRUE)
    }

    return(roi)
}

#' @title Upload file to S3
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param local_file Local file path to upload
#' @param bucket S3 bucket name
#' @param key S3 key (path) for uploaded file
#'
#' @returns S3 path to uploaded file
#'
#' @keywords internal
.s3_gdal_upload_to_s3 <- function(local_file, bucket, key) {
    # If file doesn't exist, stop!
    if (!file.exists(local_file)) {
        stop("Local file not found: ", local_file)
    }

    # Upload local file
    aws.s3::put_object(
        file = local_file,
        object = key,
        bucket = bucket,
        multipart = TRUE
    )

    # Prepare return as s3 path
    s3_path <- paste0("s3://", bucket, "/", key)

    # Return!
    return(s3_path)
}

#' @title Merge raster files from S3 bucket
#'
#' @description High-level function to select, download, and merge raster files from S3.
#' This function searches for raster files matching a pattern in an S3 bucket, downloads them,
#' merges them using `sf`, and uploads the result back to S3. Supports caching to avoid
#' re-downloading and re-processing files.
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param base S3 base path where files are located (e.g., "s3://bucket/path/")
#' @param glob File pattern to match using glob style (e.g., "*q*.tif", "*_B04_*.tif"). Mutually exclusive with regex
#' @param regex File pattern to match using regex style (e.g., "-q\\d+\\.tif$"). Mutually exclusive with glob
#' @param roi Optional region of interest for clipping (sf object or file path to vector file). Default is NULL
#' @param output_file Optional custom S3 path for output file. If NULL, generates name automatically by removing the pattern. Default is NULL
#' @param cache_dir Optional custom folder to save temporary files. If NULL, generates one automatically in system temp. Default is NULL
#' @param recursive Logical indicating whether to search recursively in subdirectories. Default is FALSE
#' @param cleanup Logical indicating whether to delete temporary files after completion. Default is FALSE
#' @param type Output raster type (GDAL valid format, e.g., Byte). Default is Byte
#' @param processor Function to pre-process files before merge.
#' @param ... Additional parameters (reserved for future use)
#'
#' @returns S3 path to the merged raster file
#'
#' @examples
#' \dontrun{
#' # Basic usage with glob pattern
#' merged_path <- gdal_merge_s3(
#'   base = "s3://mybucket/rasters/",
#'   glob = "*q*.tif"
#' )
#'
#' # Using regex pattern
#' merged_path <- gdal_merge_s3(
#'   base = "s3://mybucket/rasters/",
#'   regex = "-q\\d+\\.tif$"
#' )
#'
#' # With custom cache directory (files persist for reuse)
#' merged_path <- gdal_merge_s3(
#'   base = "s3://mybucket/rasters/",
#'   glob = "*q*.tif",
#'   cache_dir = "~/Downloads/cache/"
#' )
#'
#' # With ROI and custom output
#' merged_path <- gdal_merge_s3(
#'   base = "s3://mybucket/rasters/",
#'   glob = "*q*.tif",
#'   roi = my_sf_polygon,
#'   output_file = "s3://mybucket/outputs/merged.tif"
#' )
#'
#' # Recursive search with cleanup
#' merged_path <- gdal_merge_s3(
#'   base = "s3://mybucket/rasters/",
#'   regex = "_B04_.*\\.tif$",
#'   recursive = TRUE,
#'   cleanup = TRUE
#' )
#' }
#' @export
gdal_merge_s3 <- function(base,
                          glob = NULL,
                          regex = NULL,
                          roi = NULL,
                          output_file = NULL,
                          cache_dir = NULL,
                          recursive = FALSE,
                          cleanup = FALSE,
                          type = "Byte",
                          processor = NULL,
                          ...) {
    # Validate inputs
    if (!.s3_gdal_is_s3_path(base)) {
        stop("Base path must be an S3 path (starting with 's3://')")
    }

    # Check if `cache_dir` exist
    if (!is.null(cache_dir) && !fs::dir_exists(cache_dir)) {
        stop("cache_dir specified must exist!")
    }

    # If cache dir is defined, expand it to avoid issues with gdal operations
    if (!is.null(cache_dir)) {
        cache_dir <- fs::path_expand(cache_dir)
    }

    # Check roi
    if (!is.null(roi)) {
        # File case: If it is a character, check if it exists
        if (is.character(roi) && !fs::file_exists(roi)) {
            stop("roi file must exist")
        }
    }

    # User needs to define `glob` or `regex`
    pattern <- NULL

    if (!is.null(glob) && !is.null(regex)) {
        stop("`glob` and `regex` detected! Use only one of them")
    }

    # Select glob or regex
    if (!is.null(glob)) {
        pattern <- glob2rx(glob)
    } else {
        pattern <- regex
    }

    # Ensure base ends with /
    if (!grepl("/$", base)) {
        base <- paste0(base, "/")
    }

    # Parse S3 path
    s3_parts <- .s3_gdal_parse_s3_path(base)
    bucket <- s3_parts$bucket
    prefix <- s3_parts$key

    # Inform user
    message("Searching for files matching pattern: ", pattern)

    # Get matching S3 keys
    matching_keys <- .s3_gdal_get_s3_keys(
        bucket = bucket,
        prefix = prefix,
        pattern = pattern,
        recursive = recursive
    )

    # If there is no file matching the patterns, finish it!
    if (length(matching_keys) == 0) {
        stop("No files found matching pattern '", pattern, "' in ", base)
    }

    # Inform user
    message("Found ", length(matching_keys), " files to merge")

    # Define temporary directory for downloads
    if (is.null(cache_dir)) {
        cache_dir <- fs::dir_create(fs::file_temp(pattern = "gdal_merge_"))
    }

    tryCatch({
        # Download files
        message("Downloading files from S3...")

        # Download files
        local_files <- .s3_gdal_download_s3_files(bucket = bucket,
                                                  keys = matching_keys,
                                                  dest_dir = cache_dir)

        # If processor is defined, use it
        if (!is.null(processor)) {
            local_files <- processor(local_files)
        }

        # Generate output filename
        if (is.null(output_file)) {
            # Use the first file and remove pattern
            base_name <- basename(matching_keys[1])
            merged_name <- .s3_gdal_remove_pattern(base_name, pattern)

            # Ensure it has .tif extension
            if (!grepl("\\.tif$", merged_name, ignore.case = TRUE)) {
                merged_name <- fs::path_ext_set(fs::path_ext_remove(merged_name), ".tif")
            }

            # Construct S3 path (same location as source files)
            if (recursive) {
                # If it is recursive, use the higher folder level
                output_key <- paste0(prefix, merged_name)
            } else {
                # Otherwise, use the path of the first file
                output_key <- paste0(dirname(matching_keys[1]), "/", merged_name)
            }

            output_s3_path <- paste0("s3://", bucket, "/", output_key)
        } else {
            # Use the define file as output
            output_s3_path <- output_file

            # Extract s3 parts
            output_parts <- .s3_gdal_parse_s3_path(output_s3_path)

            # Get s3 key
            output_key <- output_parts$key
        }

        # Create local output path
        local_output <- fs::path(cache_dir, basename(output_key))

        # Merge rasters
        if (!fs::file_exists(local_output)) {
            message("Merging ", length(local_files), " raster files...")

            local_output <- .s3_gdal_merge_rasters(
                input_files = local_files,
                output_file = local_output,
                roi = roi
            )
        }

        # Upload result to S3
        message("Uploading merged file to S3...")
        final_s3_path <- .s3_gdal_upload_to_s3(local_file = local_output,
                                               bucket = bucket,
                                               key = output_key)

        # Inform user!
        message("Successfully merged files to: ", final_s3_path)

        # Return!
        return(final_s3_path)

    }, finally = {
        if (cleanup) {
            # Clean up temporary files
            message("Cleaning up temporary files...")
            fs::file_delete(cache_dir)
        }
    })
}
