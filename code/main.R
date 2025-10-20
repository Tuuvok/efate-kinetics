library(R6)
library(tcltk)
library(minpack.lm)
library(dplyr)
library(ggplot2)


# Path Manager Class ####
PathManager <- R6Class("PathManager",
                       public = list(
                           residue_data_path = NULL,
                           user_setup_data_path = NULL,
                           output_path = NULL,
                           
                           initialize = function() {
                               self$residue_data_path <- ""
                               self$user_setup_data_path <- ""
                               self$output_path <- ""
                           },
                           
                           selectPaths = function() {
                               message("Select files and output directory. 
                                       If the selection dialogs are hidden, minimize your active applications.\n",
                                       "1. Please select text file with residue data.\n",
                                       "2. Please select text file with setup data.\n",
                                       "3. Please select directory for output.")
                               
                               r_path <- tk_choose.files(caption = "Select text file with residue data", multi = FALSE)
                               if (length(r_path) == 0 || r_path == "") {
                                   stop("Residue data file selection was cancelled or invalid.")
                               }
                               if (!file.exists(r_path)) {
                                   stop("Residue data file does not exist: ", r_path)
                               }
                               
                               u_path <- tk_choose.files(caption = "Select text file with setup data", multi = FALSE)
                               if (length(u_path) == 0 || u_path == "") {
                                   stop("Setup data file selection was cancelled or invalid.")
                               }
                               if (!file.exists(u_path)) {
                                   stop("Setup data file does not exist: ", u_path)
                               }
                               
                               o_path <- tk_choose.dir(caption = "Select directory for output")
                               if (is.null(o_path) || o_path == "") {
                                   stop("Output directory selection was cancelled or invalid.")
                               }
                               if (!dir.exists(o_path)) {
                                   stop("Output directory does not exist: ", o_path)
                               }
                               
                               self$residue_data_path <- r_path
                               self$user_setup_data_path <- u_path
                               self$output_path <- o_path
                           },
                           
                           getPaths = function() {
                               list(
                                   residue_data_path = self$residue_data_path,
                                   user_setup_data_path = self$user_setup_data_path,
                                   output_path = self$output_path
                               )
                           },
                           
                           showPaths = function() {
                               message("Residue Data Path: ", self$residue_data_path)
                               message("User Setup Data Path: ", self$user_setup_data_path)
                               message("Output Directory: ", self$output_path)
                           }
                       )
)


# Residue Manager Class ####
ResidueManager <- R6Class("ResidueManager",
                          public = list(
                              residue_data = NULL,
                              
                              initialize = function() {
                                  self$residue_data <- data.frame()
                              },
                              
                              loadResidueData = function(residue_data_path) {
                                  if (!file.exists(residue_data_path)) {
                                      stop("The file does not exist: ", residue_data_path)
                                  }
                                  
                                  residue_data <- tryCatch({
                                      get_residue_data(residue_data_path)
                                  }, error = function(e) {
                                      stop("Failed to load residue data: ", e$message)
                                  })
                                  
                                  if (!is.data.frame(residue_data)) {
                                      stop("get_residue_data() did not return a data.frame")
                                  }
                                  
                                  self$residue_data <- residue_data
                              },
                              
                              getResidueData = function() {
                                  self$residue_data
                              }
                          )
)


# Setup Manager Class ####
SetupManager <- R6Class("SetupManager",
                        public = list(
                            setup_data = NULL,
                            model_type = NULL,
                            
                            initialize = function() {
                                self$setup_data <- list()
                                self$model_type <- ""
                            },
                            
                            loadSetupData = function(user_setup_data_path) {
                                if (!file.exists(user_setup_data_path)) {
                                    stop("The file does not exist: ", user_setup_data_path)
                                }
                                
                                user_setup_data <- tryCatch({
                                    get_user_setup_data(user_setup_data_path)
                                }, error = function(e) {
                                    stop("Failed to load user setup data: ", e$message)
                                })
                                
                                setup_data <- tryCatch({
                                    create_setup_data(user_setup_data)
                                }, error = function(e) {
                                    stop("Failed to create setup data: ", e$message)
                                })
                                
                                if (!is.list(setup_data)) {
                                    stop("create_setup_data() did not return a list")
                                }
                                
                                if (!"model_type" %in% names(setup_data)) {
                                    stop("setup data does not contain 'model_type'")
                                }
                                
                                self$setup_data <- setup_data
                                self$model_type <- setup_data$model_type
                            },
                            
                            getSetupData = function() {
                                self$setup_data
                            },
                            
                            getModelType = function() {
                                self$model_type
                            }
                        )
)


# Model Manager Class ####
ModelManager <- R6Class("ModelManager",
                        public = list(
                            reg_model = NULL,
                            parameter_data = NULL,
                            fitting_data = NULL,
                            endpoint_data = NULL,
                            iteration_data = NULL,
                            plot_data = NULL,
                            smooth_data = NULL,
                            graph = NULL,
                            
                            initialize = function() {
                                self$reg_model <- NULL
                                self$parameter_data <- matrix()
                                self$fitting_data <- data.frame()
                                self$endpoint_data <- data.frame()
                                self$iteration_data <- numeric()
                                self$plot_data <- data.frame()
                                self$smooth_data <- data.frame()
                                self$graph <- NULL
                            },
                            
                            fitModel = function(residue_data, setup_data) {
                                tryCatch({
                                    if (is.null(residue_data) || is.null(setup_data)) {
                                        stop("Both `residue_data` and `setup_data` must be provided.")
                                    }
                                    self$reg_model <- fit_model(residue_data, setup_data)
                                }, error = function(e) {
                                    message("Error in fitModel: ", e$message)
                                })
                            },
                            
                            processData = function(model_type, residue_data) {
                                tryCatch({
                                    if (is.null(self$reg_model)) stop("Model must be fitted before processing data.")
                                    if (is.null(model_type) || is.null(residue_data)) {
                                        stop("`model_type` and `residue_data` must be provided.")
                                    }
                                    
                                    self$parameter_data <- create_parameter_data(self$reg_model)
                                    self$fitting_data <- create_fitting_data(residue_data, self$reg_model)
                                    self$endpoint_data <- create_endpoint_data(model_type, self$reg_model, self$fitting_data)
                                    self$iteration_data <- create_iteration_data(self$reg_model)
                                    self$plot_data <- create_plot_data(self$fitting_data)
                                    self$smooth_data <- create_smooth_data(model_type, self$reg_model, self$plot_data)
                                    self$graph <- create_graph(model_type, self$plot_data, self$smooth_data)
                                }, error = function(e) {
                                    message("Error in processData: ", e$message)
                                })
                            },
                            
                            getResults = function() {
                                return(list(
                                    parameter_data = self$parameter_data,
                                    fitting_data = self$fitting_data,
                                    endpoint_data = self$endpoint_data,
                                    iteration_data = self$iteration_data
                                ))
                            },
                            
                            getGraph = function() {
                                return(self$graph)
                            }
                        )
)


# Output Manager Class ####
OutputManager <- R6Class("OutputManager",
                         public = list(
                             writeOutput = function(output_path, model_type, results, graph) {
                                 tryCatch({
                                     if (!is.character(output_path) || length(output_path) != 1) {
                                         stop("`output_path` must be a single string.")
                                     }
                                     if (!is.character(model_type) || length(model_type) != 1) {
                                         stop("`model_type` must be a single string.")
                                     }
                                     if (is.null(results)) {
                                         stop("`results` cannot be NULL.")
                                     }
                                     if (is.null(graph)) {
                                         stop("`graph` cannot be NULL.")
                                     }
                                     
                                     write_results(output_path, model_type, results)
                                     write_graph(output_path, model_type, graph)
                                     
                                 }, error = function(e) {
                                     message("Error in writeOutput: ", e$message)
                                 })
                             }
                         )
)


# User Function ####
run_kinetics <- function() {
    
    tryCatch({
        path_manager <- PathManager$new()
        path_manager$selectPaths()
        paths <- path_manager$getPaths()
        
        residue_manager <- ResidueManager$new()
        residue_manager$loadResidueData(paths$residue_data_path)
        residue_data <- residue_manager$getResidueData()
        
        setup_manager <- SetupManager$new()
        setup_manager$loadSetupData(paths$user_setup_data_path)
        setup_data <- setup_manager$getSetupData()
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
