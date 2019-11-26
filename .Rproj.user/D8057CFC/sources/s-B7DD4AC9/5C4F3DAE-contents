#' Plot model output.

#' This function plots the output of a fitted model data frame.
#' @param output data frame output from solve_eqns().
#' @param linesize numeric value for line width in ggplot.
#' @param textsize numeric value for textsize in ggplot.
#' @param levels character vector of the variable names in the order they should be plotted. Default is to obtain the order from the initial conditions vector 'ics'.
#' @param ... extra arguments to be passed through to ggplot scale_colour_discrete e.g. 'labels' to change the legend names.
#' @return ggplot object
#' @import ggplot2

plot_model <- function(output, linesize, textsize, levels, ...){

    output$variable <- factor(output$variable, levels = levels)

    ggplot(output, aes(x = time, y = value, colour = as.factor(variable))) +
            geom_line(size = linesize) +
            scale_colour_discrete("Compartment", ...) +
            ylab("Number of individuals") + xlab("Time") +
            theme_bw() + theme(axis.text = element_text(size = textsize),
                               axis.title= element_text(size = textsize + 2),
                               legend.text = element_text(size = textsize),
                               legend.title = element_text(size = textsize + 2) )

}
