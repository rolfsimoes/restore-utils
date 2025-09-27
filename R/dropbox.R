.dropbox_default_folder <- function() {
    default_dir <- "projects/restore-plus/data"

    fs::path(.project_env_variable("DROPBOX_FOLDER", default_dir))
}

#' @export
dropbox_upload <- function(files, dropbox_dir) {
    purrr::map(files, function(file) {
        print(paste0("Uploading: ", file))
        system(paste("rclone copy", file, paste0("dropbox:", dropbox_dir), sep = " "))
    })
}

#' @export
dropbox_dir <- function(name) {
    .dropbox_default_folder() / name
}

#' @export
dropbox_download_temporal_files <- function(output_dir, files, years) {
    # Define output dir
    output_dir <- fs::path(output_dir) / years

    # Create output directory
    fs::dir_create(output_dir)

    # Prepare urls
    files <- stringr::str_replace(files, "&dl=0", "&dl=1")

    # Prepare file names
    filenames <- basename(sub("\\?.*$", "", files))
    destfiles <- fs::path(output_dir, filenames)

    # Download the files
    purrr::walk2(files, destfiles, function(file, destfile) {
        # If file exist, just return it
        if (fs::file_exists(destfile)) {
            return(destfile)
        }

        # Download file
        download.file(file, destfile)
    })

    # Return files
    destfiles
}
