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
