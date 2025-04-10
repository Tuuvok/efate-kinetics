#' get residue data defined by user
#'
#' @param residue_data_path string with path to residue data
#' @return residue_data; dataframe containing time and concentration measurements
get_residue_data <- function(residue_data_path) {
    
    if (length(residue_data_path) == 0) {
        stop("No residue data file was selected. Exiting.")
    }
    
    residue_data <- tryCatch(
        read.table(residue_data_path, header = TRUE, sep = "\t", check.names = FALSE),
        error = function(e) stop("Error in reading the file. Make sure it is tab-separated and contains a header.")
    )
    
    num_columns <- ncol(residue_data)
    if (!(num_columns == 2 || num_columns == 3)) {
        stop(paste("Invalid file structure: Expected 2 or 3 columns, but found", num_columns, "columns."))
    }
    
    expected_first_column <- "time"
    if (colnames(residue_data)[1] != expected_first_column) {
        stop(paste("Invalid header: The first column must be named '", expected_first_column, "'.", sep = ""))
    }
    
    if (any(nchar(colnames(residue_data)[-1]) == 0)) {
        stop("Invalid header: Column headers (other than 'time') must not be empty.")
    }
    
    if (!all(sapply(residue_data[-1], is.numeric))) {
        stop("Invalid residue data: All entries (excluding the header) must contain numeric values.")
    }
    
    return(residue_data)
}


#' get time measurements
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @return time_data; vector containing time measurements
get_time_data <- function(residue_data) {
    time_data <- as.vector(as.matrix(residue_data["time"]))
    return(time_data)
}


#' get concentration measurements
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @return observation_data; vector containing concentration measurements of all compounds
get_observation_data <- function(residue_data) {
    observation_columns <- residue_data[!names(residue_data) %in% "time"]
    observation_data <- as.vector(as.matrix(observation_columns))
    return(observation_data)
}


#' get compound names
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @return compound_data; vector containing names of all compounds
get_compound_data <- function(residue_data) {
    compound_data <- unlist(
        lapply(names(residue_data[-1]), function(compound_name) {
            compound_vector <- rep(compound_name, nrow(residue_data))
            return(compound_vector)
        })
    )
    return(compound_data)
}


#' get setup data defined by user
#'
#' @param user_setup_data_path string with path to user setup data
#' @return user_setup_data; string with setup data provided by user
get_user_setup_data <- function(user_setup_data_path) {
    
    if (length(user_setup_data_path) == 0) {
        stop("No setup data file was selected. Exiting.")
    }
    
    user_setup_data <- user_setup_data_path |> 
        readLines() |> 
        (\(x) x[!grepl("^#", x)])() |>  # remove lines starting with #
        trimws() |>                     # trim whitespace
        (\(x) x[x != ""])()             # remove empty strings
    return(user_setup_data)
}


#' process user setup data and structure and store it in a list
#'
#' @param user_setup_data string with setup data provided by user
#' @return setup_data; list containing setup data relevant for nlsLM modeling
create_setup_data <- function(user_setup_data) {
    setup_data <- list(
        model_type = NULL,
        start_parms = list(),
        control_parms = list()
    )
    
    for (line in user_setup_data) {
        parts <- strsplit(line, "=")[[1]]
        if (length(parts) != 2) {
            stop("Invalid format: each line should contain a key-value pair separated by '='")
        }
        
        key <- trimws(parts[1])
        value <- trimws(parts[2])
        
        if (key == "model_type") {
            if (!is.character(value) || value == "" || grepl("^[0-9]+$", value)) {
                stop("Invalid input for model_type: must be a non-empty string")
            }
            setup_data$model_type <- value
        } else if (key %in% c("Cp0", "kp", "kp1", "kp2", "g", "km", "kfm")) {
            if (value == "NA") {
                next # skip to avoid irrelevant parameters for model
            }
            if (!grepl("^[0-9eE.-]+$", value)) {
                stop(paste("Invalid input for start_parms key", key, ": must be numeric"))
            }
            setup_data$start_parms[[key]] <- as.numeric(value)
        } else if (key %in% c("ftol", "ptol", "gtol", "factor", "maxiter")) {
            if (!grepl("^[0-9eE.-]+$", value)) {
                stop(paste("Invalid input for control_parms key", key, ": must be numeric"))
            }
            setup_data$control_parms[[key]] <- as.numeric(value)
        } else {
            stop(paste("Unknown key:", key))
        }
    }
    
    return(setup_data)
}
