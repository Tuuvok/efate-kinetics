#' model for SFO
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp degradation rate of parent
model_sfo <- function(time, Cp0, kp) {
    Cp0 * exp(-kp * time)
}


#' model for DFOP
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp1 degradation rate of parent compartment 1
#' @param kp2 degradation rate of parent compartment 2
#' @param g fraction of Cp0 applied to compartment 1
model_dfop <- function(time, Cp0, kp1, kp2, g) {
    Cp0 * ((g * exp(-kp1 * time)) + ((1-g) * exp(-kp2 * time)))
}


#' model for SFO-SFO
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp degradation rate of parent
#' @param km degradation rate of metabolite
#' @param kfm formation rate of metabolite
model_sfo_sfo <- function(time, Cp0, kp, km, kfm) {
    parent_prediction <- model_sfo(time, Cp0, kp)
    metabolite_prediction <- (kfm * Cp0 / (km - kp)) * (exp(-kp * time) - exp(-km * time))
    c(parent_prediction, metabolite_prediction)
}


#' model for DFOP-SFO
#'
#' @param time elapsed time
#' @param Cp0 initial concentration of parent
#' @param kp1 degradation rate of parent compartment 1
#' @param kp2 degradation rate of parent compartment 2
#' @param g fraction of Cp0 applied to compartment 1
#' @param km degradation rate of metabolite
#' @param kfm formation rate of metabolite
model_dfop_sfo <- function(time, Cp0, kp1, kp2, g, km, kfm) {
    parent_prediction <- model_dfop(time, Cp0, kp1, kp2, g)
    metabolite_prediction <- (kfm * Cp0 / (km - kp1)) * (g * (exp(-kp1 * time) - exp(-km * time))) + 
        (kfm * Cp0 / (km - kp2)) * ((1-g) * (exp(-kp2 * time) - exp(-km * time)))
    c(parent_prediction, metabolite_prediction)
}
