#' create dataframe with observation, prediction and residuals
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param fit nonlinear regression model
#' @return fitting_data; dataframe containing observation, prediction and residuals
create_fitting_data <- function(residue_data, fit) {
    fitting_data <- data.frame(
        time = rep(residue_data$time, length(residue_data)-1),
        compound = get_compound_data(residue_data),
        observation = get_observation_data(residue_data),
        prediction = fitted(fit),
        residuals = residuals(fit)
    )
    return(fitting_data)
}


#' create dataframe with no of iterations and conv tolerance
#'
#' @param fit nonlinear regression model
#' @return iteration_data; named vector with no of iterations and conv tolerance
create_iteration_data <- function(fit) {
    iteration_data <- c(fit$convInfo$finIter, fit$convInfo$finTol)
    names(iteration_data) <- c("iterations to convergence", "convergence tolerance")
    return(iteration_data)
}


#' get DT50 values for each compound
#'
#' @param fit nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return DT50_values; named numeric vector with DT50 value for each compound
get_DT50_values <- function(fit, fitting_data) {
    fit_summary <- summary(fit)
    parameters = fit_summary$coefficients
    iteration <- 1
    DT50_values <- sapply(unique(fitting_data$compound), function(compound_i) {
        if (iteration == 1) {
            k <- parameters["kp", "Estimate"]
        } else {
            k <- parameters["km", "Estimate"]
        }
        DT50 <- log(2)/k
        iteration <<- iteration + 1
        return(DT50)
    })
    return(DT50_values)
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
        chi2 <- calculate_chi2(predicted, observed, error_percentage)
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
