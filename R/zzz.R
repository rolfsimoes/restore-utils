#' @importFrom Rcpp sourceCpp
#' @useDynLib restoreutils, .registration = TRUE
NULL

options(
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
        "--max-time", "720",
        "--verbose"
    )
)
