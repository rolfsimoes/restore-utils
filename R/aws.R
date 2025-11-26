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
