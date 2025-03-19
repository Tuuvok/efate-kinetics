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


#' determine the parameters of a selected kinetic model running nlsLM
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param setup_data list containing setup data relevant for nlsLM modeling
#' @return reg_model; nonlinear regression model
fit_model <- function(residue_data, setup_data) {
    
    time_data <- get_time_data(residue_data)
    observation_data <- get_observation_data(residue_data)
    
    formula <- select_formula(observation_data, time_data, setup_data$model_type)
    
    if (is.null(setup_data$start_parms)) {
        start_parms <- get_default_start_parms(setup_data$model_type)
        message("Start parameters not provided. Using default parameters.")
    } else {
        start_parms <- setup_data$start_parms
    }
    
    if (is.null(setup_data$control_parms)) {
        control_parms <- get_default_control_parms()
        message("Control parameters not provided. Using default parameters.")
    } else {
        control_parms <- setup_data$control_parms
    }
    
    bounds_data <- select_bounds(setup_data$model_type, observation_data)
    
    reg_model <- nlsLM(
        formula,
        start = start_parms,
        control = do.call(nls.lm.control, control_parms),
        lower = bounds_data$lower,
        upper = bounds_data$upper,
        # trouble-shooting
        # trace = TRUE
    )
    
    return(reg_model)
}


#' select boundary conditions for estimated kinetic model parameters
#'
#' @param model_type string determining the kinetic model type
#' @param observation_data vector containing concentration measurements of all compounds
#' @return bounds_data; list containing boundary conditions for estimated kinetic model parameters
select_bounds <- function(model_type, observation_data) {
    bounds_data <- list(
        lower = NULL,
        upper = NULL
    )
    if (model_type == "SFO") {
        bounds_data$lower <- c(Cp0 = 0, kp = 0)
        bounds_data$upper <- c(Cp0 = max(observation_data * 2), kp = 10)
    } else if (model_type == "DFOP") {
        bounds_data$lower <- c(Cp0 = 0, kp1 = 0, kp2 = 0, g = 0)
        bounds_data$upper <- c(Cp0 = max(observation_data * 2), kp1 = 10, kp2 = 10, g = 2)
    } else if (model_type == "SFO-SFO") {
        bounds_data$lower <- c(Cp0 = 0, kp = 0, km = 0, kfm = 0)
        bounds_data$upper <- c(Cp0 = max(observation_data * 2), kp = 10, km = 10, kfm = 10)
    } else if (model_type == "DFOP-SFO") {
        bounds_data$lower <- c(Cp0 = 0, kp1 = 0, kp2 = 0, g = 0, km = 0, kfm = 0)
        bounds_data$upper <- c(Cp0 = max(observation_data * 2), kp1 = 10, kp2 = 10, g = 2, km = 10, kfm = 10)
    } else {
        stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
    }
    return(bounds_data)
}
    

#' select equations for selected kinetic model
#'
#' @param observation_data vector containing concentration measurements of all compounds
#' @param time_data vector containing time measurements
#' @param model_type string determining the kinetic model type
#' @return formula; equations for selected kinetic model
select_formula <- function(observation_data, time_data, model_type) {
    if (model_type == "SFO") {
        model_func <- model_sfo
        formula <- observation_data ~ model_func(time_data, Cp0, kp)
    } else if (model_type == "DFOP") {
        model_func <- model_dfop
        formula <- observation_data ~ model_func(time_data, Cp0, kp1, kp2, g)
    } else if (model_type == "SFO-SFO") {
        model_func <- model_sfo_sfo
        formula <- observation_data ~ model_func(time_data, Cp0, kp, km, kfm)
    } else if (model_type == "DFOP-SFO") {
        model_func <- model_dfop_sfo
        formula <- observation_data ~ model_func(time_data, Cp0, kp1, kp2, g, km, kfm)
    } else {
        stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
    }
    return(formula)
}


#' get default start parameters for selected kinetic model
#'
#' @param model_type string determining the kinetic model type
#' @return start_parms; list containing default start parameters for selected kinetic model
get_default_start_parms <- function(model_type) {
    if (model_type == "SFO") {
        start_parms <- list(Cp0 = 100, kp = 0.1)
    } else if (model_type == "DFOP") {
        start_parms <- list(Cp0 = 100, kp1 = 0.1, kp2 = 0.2, g = 0.5)
    } else if (model_type == "SFO-SFO") {
        start_parms <- list(Cp0 = 100, kp = 0.1, km = 0.05, kfm = 0.5)
    } else if (model_type == "DFOP-SFO") {
        start_parms <- list(Cp0 = 100, kp1 = 0.1, kp2 = 0.2, g = 0.5, km = 0.05, kfm = 0.5)
    } else {
        stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
    }
    return(start_parms)
}


#' get default control parameters
#'
get_default_control_parms <- function() {
    list(
        ftol = 1e-10,
        ptol = 1e-10,
        gtol = 1e-10,
        factor = 100,
        maxiter = 200
    )
}
