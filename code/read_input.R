#' get residue data defined by user
#'
#' @return residue_data; dataframe containing time and concentration measurements
get_residue_data <- function() {
    residue_data <- read.table(file.choose(), header = TRUE, sep = "\t")
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
#' @return user_setup_data; string with setup data provided by user
get_user_setup_data <- function() {
    user_setup_data <- file.choose() |> 
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
        key <- trimws(parts[1])
        value <- trimws(parts[2])
        
        if (value == "NA") {
            value <- NA
        } else if (grepl("^[0-9]+$", value)) { # check if it contains only numeric characters
            value <- as.numeric(value)
        } else {
            value <- value
        }
        
        if (key == "model_type") {
            setup_data$model_type <- value
        } else if (key %in% c("kp", "kp1", "kp2", "g", "km", "kfm")) {
            setup_data$start_parms[[key]] <- value
        } else if (key %in% c("maxiter", "tol", "minFactor", "ftol", "ptol", "gtol", "nprint")) {
            setup_data$control_parms[[key]] <- value
        }
    }
    
    return(setup_data)
}
