library(tcltk)
library(minpack.lm)
library(dplyr)
library(ggplot2)


#' main function run by user
#'
run_kinetics <- function() {
    
    tryCatch({
        # read and prepare input
        path_list <- create_path_list()
        residue_data <- get_residue_data(path_list)
        user_setup_data <- get_user_setup_data(path_list)
        setup_data <- create_setup_data(user_setup_data)
        model_type <- setup_data$model_type
        # fit model
        reg_model <- fit_model(residue_data, setup_data)
        # extract output and run additional stats
        fitting_data <- create_fitting_data(residue_data, reg_model)
        endpoint_data <- create_endpoint_data(model_type, reg_model, fitting_data)
        iteration_data <- create_iteration_data(reg_model)
        plot_data <- create_plot_data(fitting_data)
        smooth_data <- create_smooth_data(model_type, reg_model, plot_data)
        fit_graph <- create_fit_graph(model_type, plot_data, smooth_data)
        # prepare and write output
        output_list <- generate_output_list(residue_data, model_type, reg_model, fitting_data)
        write_output(path_list, model_type, output_list)
        write_graph(path_list, model_type, fit_graph)
        message("Done!")
    }, 
    error = function(e) {
        message("Error: ", conditionMessage(e))
    })
    
}


#' create a list of strings with respective path information to read/write data
#'
#' @return path_list; list of strings with respective path information to read/write data
create_path_list <- function() {
    
    message("Select files and output directory. If the selection dialogs are hidden, minimize your active applications.\n
            1. Please select text file with residue data.\n
            2. Please select text file with setup data.\n
            3. Please select directory for output.")
    
    residue_data_path <- tk_choose.files(caption = "Select text file with residue data", multi = FALSE)
    user_setup_data_path <- tk_choose.files(caption = "Select text file with setup data", multi = FALSE)
    output_path <- tk_choose.dir(caption = "Select directory for output")
    
    path_list <- list(
        residue_data_path = residue_data_path,
        user_setup_data_path = user_setup_data_path,
        output_path = output_path
    )
    return(path_list)
}
