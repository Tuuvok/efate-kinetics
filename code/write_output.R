#' generate output list
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param reg_model nonlinear regression model
#' @return output_list; list containing output for user
generate_output_list <- function(residue_data, reg_model) {
    reg_model_summary <- summary(reg_model)
    output_list <- list(
        parameters = reg_model_summary$coefficients,
        fitting = create_fitting_data(residue_data, reg_model),
        iteration = create_iteration_data(reg_model)
    )
    return(output_list)
}
