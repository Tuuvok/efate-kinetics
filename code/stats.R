#' create dataframe with parameters of regression model
#'
#' @param reg_model nonlinear regression model
#' @return parameter_data; dataframe containing model estimates and statistics
create_parameter_data <- function(reg_model) {
    reg_model_summary <- summary(reg_model)
    parameter_data <- reg_model_summary$coefficients
    return(parameter_data)
}


#' create dataframe with observation, prediction and residuals
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param reg_model nonlinear regression model
#' @return fitting_data; dataframe containing observation, prediction and residuals
create_fitting_data <- function(residue_data, reg_model) {
    fitting_data <- data.frame(
        time = rep(residue_data$time, length(residue_data)-1),
        compound = get_compound_data(residue_data),
        observation = get_observation_data(residue_data),
        prediction = fitted(reg_model),
        residuals = residuals(reg_model)
    )
    return(fitting_data)
}


#' create dataframe with endpoints required by FOCUS Kinetics 2014
#'
#' @param model_type string determining the kinetic model type
#' @param reg_model nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return endpoint_data; dataframe containing endpoints
create_endpoint_data <- function(model_type, reg_model, fitting_data) {
    endpoint_data <- data.frame(
        row.names = c(unique(fitting_data$compound)),
        DT50 = get_DT_values(model_type, reg_model, fitting_data, 2),
        DT90 = get_DT_values(model_type, reg_model, fitting_data, 10),
        chi2 = get_chi2_values(fitting_data),
        t_test_k1 = get_p_values(model_type, reg_model, fitting_data, k1 = T),
        t_test_k2 = get_p_values(model_type, reg_model, fitting_data, k1 = F),
        ff = get_ff_values(reg_model, fitting_data)
    )
    return(endpoint_data)
}


#' create dataframe with no of iterations and conv tolerance
#'
#' @param reg_model nonlinear regression model
#' @return iteration_data; named vector with no of iterations and conv tolerance
create_iteration_data <- function(reg_model) {
    iteration_data <- c(reg_model$convInfo$finIter, reg_model$convInfo$finTol)
    names(iteration_data) <- c("iterations to convergence", "convergence tolerance")
    return(iteration_data)
}


#' get DT values for each compound
#'
#' @param model_type string determining the kinetic model type
#' @param reg_model nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @param target_value numeric value to transform rate constant into specific degradation time
#' @return DT_values; named numeric vector with DT value for each compound
get_DT_values <- function(model_type, reg_model, fitting_data, target_value) {
    parameters <- get_parameters(reg_model)
    iteration <- 1
    DT_values <- sapply(unique(fitting_data$compound), function(compound_i) {
        if (iteration == 1) {
            if (model_type %in% c("SFO", "SFO-SFO")) {
                k <- parameters["kp", "Estimate"]
                DT <- calculate_DT(target_value, k)
            } else if (model_type %in% c("DFOP", "DFOP-SFO")) {
                DT <- calculate_DFOP_DT(reg_model, target_value)
            } else {
                stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
            }
        } else {
            k <- parameters["km", "Estimate"]
            DT <- calculate_DT(target_value, k)
        }
        iteration <<- iteration + 1
        return(DT)
    })
    return(DT_values)
}

    
#' calculate degradation time for SFO model
#'
#' @param target_value numeric value to transform rate constant into specific degradation time
#' @param k numeric value for rate constant
#' @return DT; numeric value for degradation time
calculate_DT <- function(target_value, k) {
    DT <- round(
        log(target_value)/k,
        3)
    return(DT)
}


#' calculate degradation time for DFOP model
#'
#' @param reg_model nonlinear regression model
#' @param target_value numeric value to transform rate constant into specific degradation time
#' @return DT; numeric value for degradation time
calculate_DFOP_DT <- function(reg_model, target_value) {
    if (target_value == 2) {
        model_func <- model_dfop_DT50
    } else {
        model_func <- model_dfop_DT90
    }
    model_parms <- set_model_parms(reg_model, model_func)
    decay_time <- uniroot(model_func, interval = c(0, 1000), 
                          Cp0 = model_parms["Cp0"], 
                          kp1 = model_parms["kp1"], 
                          kp2 = model_parms["kp2"], 
                          g = model_parms["g"]
                          )
    DT <- decay_time$root
    return(DT)
}


#' get chi2 values for each compound
#'
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return chi2_values; named numeric vector with chi2 value for each compound
get_chi2_values <- function(fitting_data) {
    chi2_values <- sapply(unique(fitting_data$compound), function(compound_i) {
        fitting_data_filtered <- fitting_data %>%
            filter(compound == compound_i) %>%
            group_by(time, compound) %>%
            summarise(
                observation = mean(observation),
                prediction = mean(prediction),
                .groups = 'drop'
            )
        predicted <- fitting_data_filtered$prediction
        observed <- fitting_data_filtered$observation
        error_percentage <- calculate_error_percentage(predicted, observed)
        chi2 <- round(
            calculate_chi2(predicted, observed, error_percentage),
            3)
        return(chi2)
    })
    return(chi2_values)
}


#' calculate error percentage
#'
#' @param predicted vector of predicted values by the model
#' @param observed vector of observed values from experiment 
#' @return error_percentage; vector of measurement error percentage
calculate_error_percentage <- function(predicted, observed) {
    error_percentage <- ifelse(observed == 0,
                               0,
                               (predicted - observed) / observed * 100)
    return(error_percentage)
}


#' calculate chi2
#'
#' @param predicted vector of predicted values by the model
#' @param observed vector of observed values from experiment
#' @param error_percentage vector of measurement error percentage
#' @return chi2; numeric value
calculate_chi2 <- function(predicted, observed, error_percentage) {
    chi2 <- sum(
        ifelse(observed == 0,
               0,
               (predicted - observed) ^2 / 
                   (error_percentage/100 * mean(observed)) ^2
        )
    )
    return(chi2)
}


#' get p values for each compound
#'
#' @param model_type string determining the kinetic model type
#' @param reg_model nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @param k1 boolean determining rate selection
#' @return p_values; named numeric vector with p value for each compound
get_p_values <- function(model_type, reg_model, fitting_data, k1) {
    parameters <- get_parameters(reg_model)
    iteration <- 1
    p_values <- sapply(unique(fitting_data$compound), function(compound_i) {
        if (iteration == 1) {
            if (k1 == T) {
                if (model_type %in% c("SFO", "SFO-SFO")) {
                    p <- formatC(parameters["kp", "Pr(>|t|)"], format = "E", digits = 2)
                } else if (model_type %in% c("DFOP", "DFOP-SFO")) {
                    p <- formatC(parameters["kp1", "Pr(>|t|)"], format = "E", digits = 2)
                } else {
                    stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
                }
            } else {
                if (model_type %in% c("SFO", "SFO-SFO")) {
                    p <- "-"
                } else if (model_type %in% c("DFOP", "DFOP-SFO")) {
                    p <- formatC(parameters["kp2", "Pr(>|t|)"], format = "E", digits = 2)
                } else {
                    stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
                }
            }
        } else {
            if (k1 == T) {
                p <- formatC(parameters["km", "Pr(>|t|)"], format = "E", digits = 2)
            } else {
                p <- "-"
            }
        }
        iteration <<- iteration + 1
        return(p)
    })
    return(p_values)
}


#' get formation fraction for each compound
#'
#' @param reg_model nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return ff_values; named string vector with ff value for each compound
get_ff_values <- function(reg_model, fitting_data) {
    parameters <- get_parameters(reg_model)
    iteration <- 1
    ff_values <- sapply(unique(fitting_data$compound), function(compound_i) {
        if (iteration == 1) {
            ff <- "-"
        } else {
            ff <- round(parameters["kfm", "Estimate"], 3)
        }
        iteration <<- iteration + 1
        return(ff)
    })
    return(ff_values)
}
