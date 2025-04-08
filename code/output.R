#' generate output list
#'
#' @param residue_data dataframe containing time and concentration measurements
#' @param model_type string determining the kinetic model type
#' @param reg_model nonlinear regression model
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return output_list; list containing model output for user (parameters etc.)
generate_output_list <- function(residue_data, model_type, reg_model, fitting_data) {
    output_list <- list(
        parameters = get_parameters(reg_model),
        fitting = create_fitting_data(residue_data, reg_model),
        endpoints = create_endpoint_data(model_type, reg_model, fitting_data),
        iteration = create_iteration_data(reg_model)
    )
    return(output_list)
}


#' write output list into text file
#'
#' @param path_list list of strings with file and directory paths
#' @param model_type string determining the kinetic model type
#' @param output_list list containing model output for user (parameters etc.)
write_output <- function(path_list, model_type, output_list) {
    
    output_path <- file.path(path_list$output_path, paste0("results_", model_type, ".txt"))
    
    if (length(output_path) == 0) {
        stop("No output path was selected. Exiting.")
    }
    
    file_conn <- file(output_path, open = "w")
    

    for (name in names(output_list)) {

        writeLines(paste0("# ", name), file_conn)
        
        element <- output_list[[name]]
        
        if (is.matrix(element)) {
            write.table(format(as.data.frame(element), justify = "right"), 
                        file_conn, sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)
        } else if (is.data.frame(element)) {
            write.table(format(as.data.frame(element), justify = "right"), 
                        file_conn, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        } else {
            write.table(format(as.data.frame(element), justify = "right"), 
                        file_conn, sep = "\t", row.names = TRUE, col.names = FALSE, quote = FALSE)
        }
        
        writeLines("\n", file_conn)
    }
    
    close(file_conn)
}


#' write graph into jpeg file
#'
#' @param path_list list of strings with file and directory paths
#' @param model_type string determining the kinetic model type
#' @param fit_graph plot with observation and prediction
write_graph <- function(path_list, model_type, fit_graph) {
    output_path <- path_list$output_path
    ggsave(filename = file.path(output_path, paste0(model_type, ".jpg")), 
       plot = fit_graph, 
       width = 6, height = 4, dpi = 300, device = "jpeg")
}
