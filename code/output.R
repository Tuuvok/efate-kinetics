#' generate output list
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param model_type string determining the kinetic model type
#' @param reg_model nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return output_list; list containing output for user
generate_output_list <- function(residue_data, model_type, reg_model, fitting_data) {
    output_list <- list(
        parameters = get_parameters(reg_model),
        fitting = create_fitting_data(residue_data, reg_model),
        endpoints = create_endpoint_data(model_type, reg_model, fitting_data),
        iteration = create_iteration_data(reg_model)
    )
    return(output_list)
}
