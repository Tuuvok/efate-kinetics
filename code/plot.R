#' select observation from fitting data
#'
#' @param fitting_data dataframe containing observation, prediction and residuals
#' @return plot_data; dataframe containing observation
create_plot_data <- function(fitting_data) {
    plot_data <- fitting_data %>%
        select(time, observation)
    return(plot_data)
}


#' run kinetic model with estimated parameters on a fine time scale to create smooth data
#'
#' @param model_type string determining the kinetic model type
#' @param reg_model nonlinear regression model
#' @param plot_data dataframe containing observation
#' @return smooth_data; dataframe containing prediction on a fine time scale
create_smooth_data <- function(model_type, reg_model, plot_data) {
    
    model_func <- switch(
        model_type,
        "SFO" = model_sfo,
        "DFOP" = model_dfop,
        "SFO-SFO" = model_sfo_sfo,
        "DFOP-SFO" = model_dfop_sfo,
        stop("Invalid model type. Valid model types: SFO, DFOP, SFO-SFO, DFOP-SFO")
    )
    
    model_parms <- set_model_parms(reg_model, model_func)
    smooth_time <- seq(min(plot_data$time), max(plot_data$time), length.out = 500)
    smooth_prediction <- do.call(model_func, c(as.list(model_parms), list(time = smooth_time)))
    
    smooth_data <- data.frame(
        time = smooth_time,
        prediction = smooth_prediction
    )
    
    return(smooth_data)
}


#' get parameter estimates from regression model and check them against selected model function
#'
#' @param reg_model nonlinear regression model
#' @param model_func kinetic model function
#' @return model_parms; vector with parameter estimates from regression model
set_model_parms <- function(reg_model, model_func) {
    parameters <- get_parameters(reg_model)
    estimates <- parameters[,"Estimate"]
    model_args <- setdiff(names(formals(model_func)), "time")
    missing_parms <- setdiff(model_args, names(estimates))
    if (length(missing_parms) > 0) {
        stop(paste("Error: Missing parameters in 'estimates':", paste(missing_parms, collapse = ", ")))
    }
    model_parms <- estimates[names(estimates) %in% model_args]
    return(model_parms)
}


#' plot observed residues against predicted degradation
#'
#' @param model_type string determining the kinetic model type
#' @param plot_data dataframe containing observation
#' @param smooth_data dataframe containing prediction on a fine time scale
#' @return fit_graph; plot with observation and prediction
create_fit_graph <- function(model_type, plot_data, smooth_data) {
    
    fit_graph <- ggplot() +
        geom_point(data = plot_data, 
                   aes(x = time, 
                       y = observation, 
                       color = "Observation"),
                   size = 2) +
        geom_line(data = smooth_data, 
                  aes(x = time, 
                      y = prediction, 
                      color = "Prediction"),
                  linewidth = 1) +
        scale_color_manual(values = c("Observation" = "black", "Prediction" = "darkgray")) +
        labs(
            title = model_type,
            x = "Time",
            y = "Residue",
            color = NULL
        ) +
        xlim(min(plot_data$time), max(plot_data$time)) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "top",
            legend.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, size = 14), # center the plot title
            axis.title.x = element_text(margin = margin(t = 10)), # move x-axis title
            axis.title.y = element_text(margin = margin(r = 10))  # move y-axis title
        )
    
    return(fit_graph)
}
