#' @title Upload files to AWS S3 bucket
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Uploads one or more local files to an AWS S3 bucket.
#'
#' @param files    Character vector of local file paths to upload
#' @param object   Character vector specifying S3 object path prefix(es). Can be a single
#'                 value (used for all files) or same length as files (one per file)
#' @param bucket   Character specifying the S3 bucket name
#' @param ...      Additional parameters passed to \code{aws.s3::put_object()}
#'
#' @returns A list of results from \code{aws.s3::put_object()} for each uploaded file
#'
#' @export
upload_aws <- function(files, object, bucket, ...) {
    # Verify files exists
    stopifnot(any(fs::file_exists(files)))
    stopifnot(length(object) == 1 || length(object) == length(files))

    # Transform into fs
    files <- fs::path(files)
    object <- fs::path(object)

    # Verify if bucket exists
    stopifnot(aws.s3::bucket_exists(bucket))

    # Upload file
    purrr::map2(files, object, function(file_to_upload, obj) {
        cli::cli_alert_info("Uploading {fs::path_file(file_to_upload)}")
        aws.s3::put_object(
            file = file_to_upload,
            object = obj / fs::path_file(file_to_upload),
            bucket = bucket,
            show_progress = TRUE,
            ...
        )
    })
}
