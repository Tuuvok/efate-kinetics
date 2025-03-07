#' generate output list
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param fit nonlinear regression model
#' @return output_list; list containing output for user
generate_output_list <- function(residue_data, fit) {
    fit_summary <- summary(fit)
    output_list <- list(
        parameters = fit_summary$coefficients,
        fitting = create_fitting_data(residue_data, fit),
        iteration = create_iteration_data(fit),
    )
    return(output_list)
}
