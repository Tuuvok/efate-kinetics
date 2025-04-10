library(tcltk)
library(minpack.lm)
library(dplyr)
library(ggplot2)


#' Path Manager Class
#'
#' @field residue_data_path character. 
#' @field user_setup_data_path character. 
#' @field output_path character. 
PathManager <- setRefClass(
    "PathManager",
    fields = list(
        residue_data_path = "character",
        user_setup_data_path = "character",
        output_path = "character"
    ),
    methods = list(
        selectPaths = function() {
            message("Select files and output directory. If the selection dialogs are hidden, minimize your active applications.\n
              1. Please select text file with residue data.\n
              2. Please select text file with setup data.\n
              3. Please select directory for output.")
            
            residue_data_path <<- tk_choose.files(caption = "Select text file with residue data", multi = FALSE)
            user_setup_data_path <<- tk_choose.files(caption = "Select text file with setup data", multi = FALSE)
            output_path <<- tk_choose.dir(caption = "Select directory for output")
        },
        
        getPaths = function() {
            return(list(
                residue_data_path = residue_data_path,
                user_setup_data_path = user_setup_data_path,
                output_path = output_path
            ))
        },
        
        showPaths = function() {
            message("Residue Data Path: ", residue_data_path)
            message("User Setup Data Path: ", user_setup_data_path)
            message("Output Directory: ", output_path)
        }
    )
)


#' Residue Manager Class
#'
#' @field data data.frame. 
ResidueManager <- setRefClass(
    "ResidueManager",
    fields = list(data = "data.frame"),
    methods = list(
        loadData = function(residue_data_path) {
            data <<- get_residue_data(residue_data_path)
        },
        getData = function() {
            return(data)
        }
    )
)


#' Setup Manager Class
#'
#' @field data list. 
#' @field model_type character. 
SetupManager <- setRefClass(
    "SetupManager",
    fields = list(data = "list", model_type = "character"),
    methods = list(
        prepareSetup = function(user_setup_data_path) {
            user_data <- get_user_setup_data(user_setup_data_path)
            data <<- create_setup_data(user_data)
            model_type <<- data$model_type
        },
        
        getSetup = function() {
            return(data)
        },
        
        getModelType = function() {
            return(model_type)
        }
    )
)


#' Model Manager Class
#'
#' @field reg_model ANY. 
#' @field parameter_data matrix. 
#' @field fitting_data data.frame. 
#' @field endpoint_data data.frame. 
#' @field iteration_data numeric. 
#' @field plot_data data.frame. 
#' @field smooth_data data.frame. 
#' @field graph ANY. 
ModelManager <- setRefClass(
    "ModelManager",
    fields = list(
        reg_model = "ANY",
        parameter_data = "matrix",
        fitting_data = "data.frame",
        endpoint_data = "data.frame",
        iteration_data = "numeric",
        plot_data = "data.frame",
        smooth_data = "data.frame",
        graph = "ANY"
    ),
    methods = list(
        fitModel = function(residue_data, setup_data) {
            reg_model <<- fit_model(residue_data, setup_data)
        },
        
        processData = function(model_type, residue_data) {
            parameter_data <<- create_parameter_data(reg_model)
            fitting_data <<- create_fitting_data(residue_data, reg_model)
            endpoint_data <<- create_endpoint_data(model_type, reg_model, fitting_data)
            iteration_data <<- create_iteration_data(reg_model)
            plot_data <<- create_plot_data(fitting_data)
            smooth_data <<- create_smooth_data(model_type, reg_model, plot_data)
            graph <<- create_graph(model_type, plot_data, smooth_data)
        },
        
        getResults = function() {
            return(list(
                parameter_data = parameter_data,
                fitting_data = fitting_data,
                endpoint_data = endpoint_data,
                iteration_data = iteration_data
                # plot_data = plot_data,
                # smooth_data = smooth_data,
                # fit_graph = fit_graph
            ))
        },
        
        getGraph = function() {
            return(graph)
        }
    )
)


#' Output Manager Class
#'
OutputManager <- setRefClass(
    "OutputManager",
    methods = list(
        writeOutput = function(output_path, model_type, results, graph) {
            # output_list <- generate_output_list(residue_data, model_type, reg_model, fitting_data)
            write_results(output_path, model_type, results)
            write_graph(output_path, model_type, graph)
        }
    )
)


#' User Function
#'
run_kinetics <- function() {
    
    tryCatch({
        
        path_manager <- PathManager$new()
        path_manager$selectPaths()
        paths <- path_manager$getPaths()
        
        residue_manager <- ResidueManager$new()
        residue_manager$loadData(paths$residue_data_path)
        residue_data <- residue_manager$getData()
        
        setup_manager <- SetupManager$new()
        setup_manager$prepareSetup(paths$user_setup_data_path)
        setup_data <- setup_manager$getSetup()
        model_type <- setup_manager$getModelType()
        
        model_manager <- ModelManager$new()
        model_manager$fitModel(residue_data, setup_data)
        model_manager$processData(model_type, residue_data)
        results <- model_manager$getResults()
        graph <- model_manager$getGraph()
        
        output_manager <- OutputManager$new()
        output_manager$writeOutput(paths$output_path, model_type, results, graph)
        
        message("Done!")
    },
    
    error = function(e) {
        message("Error: ", conditionMessage(e))
    })
    
}
