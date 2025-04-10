#' write output list into results text file
#'
#' @param output_path string with path to output directory
#' @param model_type string determining the kinetic model type
#' @param results list containing model output for user (parameters etc.)
write_results <- function(output_path, model_type, results) {
    
    if (length(output_path) == 0) {
        stop("No output path was selected. Exiting.")
    }
    
    output <- file.path(output_path, paste0("results_", model_type, ".txt"))
    
    file_conn <- file(output, open = "w")

    for (name in names(results)) {

        writeLines(paste0("# ", name), file_conn)
        
        element <- results[[name]]
        
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
#' @param output_path string with path to output directory
#' @param model_type string determining the kinetic model type
#' @param graph plot with observation and prediction
write_graph <- function(output_path, model_type, graph) {
    
    if (length(output_path) == 0) {
        stop("No output path was selected. Exiting.")
    }
    
    ggsave(filename = file.path(output_path, paste0(model_type, ".jpg")), 
       plot = graph, 
       width = 6, height = 4, dpi = 300, device = "jpeg")
}
