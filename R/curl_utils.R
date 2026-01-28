#' @title Configure curl options for robust file downloads
#'
#' @description Sets global R options to use curl for file downloads with retry logic,
#' timeout settings, and error handling.
#'
#' @details
#' The function configures the following curl behaviors:
#' \itemize{
#'   \item **Redirects**: Follows HTTP redirects automatically
#'   \item **Error handling**: Returns errors for non-2xx HTTP status codes
#'   \item **Retries**: Attempts up to 20 retries with 60-second delays between attempts
#'   \item **Cleanup**: Removes partially downloaded files on error
#'   \item **Timeouts**: Sets connection timeout to 15 seconds and max operation time to 2000 seconds
#'   \item **Verbose output**: Enables detailed logging for debugging
#' }
#'
#' @returns Invisibly returns `NULL`. The function is called for its side effect of setting
#' global options for \code{download.file()}.
#'
#' @export
curl_patch_config <- function() {
    options(
        "download.file.method" = "curl",
        "download.file.extra" = c(
            "-L",                    # follow redirects
            "--fail",                # non-2xx => error
            "--retry", "20",         # retry count
            "--remove-on-error",     # remove file on error
            #"--skip-existing",       # skipping when downloaded file name exists
            "--retry-delay", "60",   # seconds between retries
            "--retry-all-errors",    # retry on all failure types
            "--retry-connrefused",
            "--connect-timeout", "15",
            "--max-time", "2000",
            "--verbose"
        )
    )
}
