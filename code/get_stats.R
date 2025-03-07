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
