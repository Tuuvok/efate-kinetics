library(minpack.lm)


#' get residue data defined by user
#'
#' @return residue_data; dataframe containing time and concentration measurements
get_residue_data <- function() {
    residue_data <- read.table(file.choose(), header = TRUE, sep = "")
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


#' SFO parent model
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp degradation rate of parent
sfo_parent_model <- function(time, Cp0, kp) {
    Cp0 * exp(-kp * time)
}


#' SFO metabolite model
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp degradation rate of parent
#' @param km degradation rate of metabolite
#' @param kfm formation rate of metabolite
sfo_metabolite_model <- function(time, Cp0, kp, km, kfm) {
    (kfm * Cp0 / (km - kp)) * (exp(-kp * time) - exp(-km * time))
}


#' SFO model
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp degradation rate of parent
#' @param km degradation rate of metabolite
#' @param kfm formation rate of metabolite
sfo_model <- function(time, Cp0, kp, km, kfm) {
    parent_prediction <- sfo_parent_model(time, Cp0, kp)
    metabolite_prediction <- sfo_metabolite_model(time, Cp0, kp, km, kfm)
    c(parent_prediction, metabolite_prediction)
}


#' fit model to observation data
#'
#' @param residue_data dataframe containing time and concentration measurements
fit_model <- function(residue_data) {
    time_data <- get_time_data(residue_data)
    observation_data <- get_observation_data(residue_data)
    fit <- nlsLM(observation_data ~ sfo_model(time_data, Cp0, kp, km, kfm),
                 start = list(Cp0 = 100, kp = 0.1, km = 0.05, kfm = 0.01),
                 control = nls.lm.control(maxiter = 1024))
}

