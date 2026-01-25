#' @title Load complete Flux configuration
#'
#' @description Function that loads all configuration from multiple sources:
#'   - \code{.restore.json} file (experiment config)
#'   - Environment variables (API URL, token, machine ID)
#'   - Git repository (commit hash)
#'   - R environment (list of packages)
#'
#' @param config_file Path to \code{.restore.json} file
#'
#' @returns List with complete configuration ready for API calls.
#'
#' @keywords internal
.flux_load_config <- function(config_file = ".restore.json") {
    # Check if `.restore.json` exists
    if (!file.exists(config_file)) {
        stop(sprintf("Configuration file not found: %s", config_file))
    }

    # Load file
    file_config <- jsonlite::fromJSON(config_file, simplifyVector = FALSE)

    # Load environment variables
    base_url <- Sys.getenv("FLUX_API_URL")
    token <- Sys.getenv("FLUX_ADMIN_TOKEN")
    machine_id <- Sys.getenv("FLUX_MACHINE_ID")

    # Validate environment variables
    if (token == "") {
        stop("`FLUX_ADMIN_TOKEN` environment variable is required")
    }

    if (token == "") {
        stop("`FLUX_ADMIN_TOKEN` environment variable is required")
    }

    if (machine_id == "") {
        stop("`FLUX_MACHINE_ID` environment variable is required")
    }

    # Validate required fields in config file
    if (is.null(file_config$id) || file_config$id == "") {
        stop("Configuration file must contain `id` field")
    }

    if (is.null(file_config$group) || is.null(file_config$group$id)) {
        stop("Configuration file must contain `group.id` field")
    }

    # Get git hash if requested
    git_available <- tryCatch(
        {
            system2("git", "--version", stdout = FALSE, stderr = FALSE)
            TRUE
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
    )

    # If git is available, get current commit hash
    if (git_available) {
        git_hash <- tryCatch(
            {
                system2(
                    "git",
                    c("rev-parse", "--short", "HEAD"),
                    stdout = TRUE,
                    stderr = FALSE
                )
            },
            error = function(e) NULL,
            warning = function(w) NULL
        )
    }

    # Get R packages
    loaded_packages <- loadedNamespaces()

    packages <- sapply(loaded_packages, function(pkg) {
        tryCatch(
            as.character(packageVersion(pkg)),
            error = function(e) "unknown"
        )
    }, USE.NAMES = TRUE, simplify = FALSE)

    # Build complete configuration
    config <- list(
        # From .restore.json
        exp_id = file_config$id,

        title = file_config$title,
        description = file_config$description,

        group_id = file_config$group$id,
        group_name = file_config$group$name,
        group_description = file_config$group$description,

        roi = file_config$roi,

        # From environment
        machine_id = machine_id,
        base_url = base_url,
        token = token,

        # From git and R environment
        git_hash = git_hash,
        packages = packages,

        # Storage from .restore.json
        storage_path = file_config$storage$path,
        storage_provider = file_config$storage$provider,

        # Settings from .restore.json
        num_machines = file_config$settings$machines,
        priority = file_config$settings$priority
    )

    return(config)
}


#' @title Build experiment body
#'
#' @description Build required body to create an experiment in Flux
#'
#' @param config     Config object
#'
#' @returns List with experiment body.
#' @keywords internal
.flux_build_experiment_body <- function(config) {
    # Build context object - We decided to build it manually instead of
    # asking a complete json file, to make things easier for users
    context <- list(
        title = config$title,
        description = config$description,
        machine_id = config$machine_id,
        git_hash = config$git_hash,
        packages = config$packages,
        roi = config$roi
    )

    # Check storage path
    if (!is.null(config$storage_path)) {
        context$storage <- list(
            path = config$storage_path,
            provider = config$storage_provider
        )
    }

    # Build settings object
    settings <- list()

    # Check number of machines
    if (!is.null(config$num_machines)) {
        settings$number_of_machines <- as.integer(config$num_machines)
    }

    # Check priority
    if (!is.null(config$priority)) {
        settings$priority <- config$priority
    }

    # Build request body
    body <- list(
        id = config$exp_id,
        group_id = config$group_id,
        context = context,
        settings = settings
    )
}

#' @title Create or register a Flux experiment
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Register a new experiment in the Flux system.
#'
#' @param config_file  Path to .restore.json file (defaults to ".restore.json")
#'
#' @returns            List with experiment details (id, status, context, settings)
#' @export
flux_experiment <- function(config_file = ".restore.json") {
    # Load complete configuration
    config <- .flux_load_config(config_file = config_file)

    # Build request body
    body <- .flux_build_experiment_body(config = config)

    # Make API request
    endpoint <- sprintf("%s/api/v1/experiments", config$base_url)

    # Request flux
    response <- httr2::request(endpoint) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
            "Content-Type" = "application/json",
            "Authorization" = sprintf("Bearer %s", config$token)
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

    # Check response status
    status_code <- httr2::resp_status(response)

    # Inform user
    if (status_code %in% c(200, 201)) {
        # Response body
        result <- httr2::resp_body_json(response)

        # Inform user
        message(sprintf("Experiment '%s' registered successfully (status: %s)",
                        result$id, result$status))

        # Return!
        return(invisible(result))
    } else {
        error_body <- httr2::resp_body_json(response)

        stop(
            sprintf(
                "Failed to create experiment: %s (HTTP %d)",
                error_body$error, status_code
            )
        )
    }
}


#' @title Start experiment run on this machine
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Register this machine as running the experiment. Captures
#'   machine-specific metadata (git hash, R packages). Configuration is
#'   automatically loaded from .restore.json and environment variables.
#'
#' @param status Status of the experiment in the current machine
#' @param config_file  Path to .restore.json file (defaults to ".restore.json")
#'
#' @returns            List with updated experiment details
#' @export
flux_experiment_status <- function(status, config_file = ".restore.json") {
    # Define valid experiment statuses
    valid_experiment_status <- c("running", "completed", "failed")

    # Validate user input
    if (!status %in% valid_experiment_status) {
        stop(
            "status must be one of the following values: ",
            paste0(valid_experiment_status, collapse = ", ")
        )
    }

    # Load complete configuration
    config <- .flux_load_config(config_file = config_file)

    # Build request body
    body <- list(
        machine_id = config$machine_id,
        status = status
    )

    # Make API request
    endpoint <- sprintf(
        "%s/api/v1/experiments/%s/status", config$base_url, config$exp_id
    )

    # Request flux
    response <- httr2::request(endpoint) |>
        httr2::req_method("PATCH") |>
        httr2::req_headers(
            "Content-Type" = "application/json",
            "Authorization" = sprintf("Bearer %s", config$token)
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

    # Check response status
    status_code <- httr2::resp_status(response)

    if (status_code %in% c(200, 201)) {
        # Response body
        result <- httr2::resp_body_json(response)

        # Inform user
        message(sprintf("Experiment '%s' registered successfully (status: %s)",
                        result$id, result$status))

        # Return!
        return(invisible(result))
    } else {
        error_body <- httr2::resp_body_json(response)

        stop(
            sprintf(
                "Failed to create experiment: %s (HTTP %d)",
                error_body$error, status_code
            )
        )
    }
}
