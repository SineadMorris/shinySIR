#' Solve equations
#'
#' This function solves an ODE model using 'deSolve' and returns the output as a data frame.
#' @param model name of the model to be solved. Examples of built-in models are: "SIR", "SIRS", "SIS", "SIRvaccination". Call 'default_models()' to view the names of all built-in models along with a short description. Default model is "SIR".
#' @param neweqns function specifying the equations of the user-defined model. Only to be used if a model is required that is not built-in. Default is NULL.
#' @param ics named numeric vector specifying the initial conditions i.e. the initial values of all model variables. Default is c(S = 9999, I = 1, R = 0) for the SIR model.
#' @param tstart numerical value of form c(tmin, tmax) indicating the time to start simulations. Default value is 0.
#' @param timestep numerical value indicating time step be used when solving equations. Default value is 1/365.
#' @param tmax numerical value indicating maximum time point to be considered.
#' @param parm0 named numeric vector of starting parameter values. Names must correspond to those used in the model equations.
#' @param parm_names character vector of parameter names to be displayed in shiny menu. Must be in the same order as 'parm0'.
#' @param parm_min named numeric vector of minimum parameter values.
#' @param parm_max named numeric vector of maximum parameter values.
#' @param sigfigs number of significant figures to round parameter input vectors. Default is 4.
#' @param showtable logical TRUE/FALSE. Should the table of transformed parameters be shown? Only applies to built-in models. Default is TRUE.
#' @param linesize numeric value for line width in ggplot output. Default is 1.2.
#' @param textsize numeric value for textsize in ggplot output. Default is 14.
#' @param xlabel character string for x axis plotting label. Default is "Time".
#' @param ylabel character string for y axis plotting label. Default is "Number of individuals".
#' @param legend_title character string for legend title. Default is "Compartment".
#' @param slider_steps numeric vector of step size to include between slider input values. Should be NULL or a vector with an entry for each parameter input. Default is NULL.
#' @param values vector specifying manual color scale (if desired). Length must equal the number of model variables.
#' @param ... extra argument to be passed through to ggplot scale_colour_manual: use 'labels' to change the legend names.
#' @return data frame of model solutions in long format.
#' @import shiny
#' @export
#' @examples
#' \donttest{
#' run_shiny(model = "SIR")
#' }
#'

run_shiny <- function(model = "SIR", neweqns = NULL,
                      ics = NULL,
                      tstart = 0, timestep = 1, tmax = 365,
                      parm0 = NULL,
                      parm_names = NULL,
                      parm_min = NULL,
                      parm_max = NULL,
                      sigfigs = 4,
                      showtable = TRUE,
                      linesize = 1.2, textsize = 14,
                      xlabel = "Time", ylabel = "Number of individuals",
                      legend_title = "Compartment",
                      slider_steps = NULL,
                      values = NULL, ...
                      ){

    # Get eqns & display name
    name <- model

    if (exists(model) & is.null(neweqns)) {
        eqns <- get(model, mode = "function")
        name <- get_name(model)
    } else if (exists(model) & !is.null(neweqns)) {
        warning("Your model name matches one of the built-in models. You can rename it using the 'model' argument.")
        eqns <- neweqns
    } else if (!exists(model) & is.null(neweqns)) {
        stop("Model name not recognized. Try one of the built-in models or specify your own with the 'neweqns' argument.")
    } else {
        eqns <- neweqns
    }

    # Get parameters for built-in models
    if (is.null(parm0) & is.null(neweqns)) {
        params <- get_params(model)

        parm0 <- params$parm0
        parm_names <- params$parm_names

        parm_min <- params$parm_min
        parm_max <- params$parm_max
    }

    # Check parameter vectors when user-specified model is defined
    if (!is.null(neweqns) & ( is.null(parm0))) {
        stop("Missing parameter vector 'parm0'")
    }

    if (!is.null(neweqns) & ( is.null(parm_min))) {
        warning("Missing parameter vector 'parm_min': using 0.5 * parm0 as default.")
        parm_min <- parm0 * 0.5
    }

    if (!is.null(neweqns) & ( is.null(parm_max))) {
        warning("Missing parameter vector 'parm_max': using 1.5 * parm0 as default.")
        parm_max <- parm0 * 1.5
    }

    if (is.null(names(parm_min)) | is.null(names(parm_max)) | is.null(names(parm0))) {
        stop("parm0, parm_min, and parm_max must be named vectors.")
    }

    if (!is.null(neweqns) & ( is.null(parm_names))) {
        parm_names <- names(parm0)
        warning("Could not find names of parameters for interactive menu ('parm_names'). Using names of 'parm0' instead.")
    }

    # Check parameters appear in the same order in all vectors
    if ( !( all(sapply(list(names(parm0), names(parm_min), names(parm_max)), function(x) x == names(parm0)))) ){
        stop("The parameters in parm0, parm_min, and parm_max must have the same names, and appear in the same order.")
    }

    # Check parameter values
    if (any(parm_min >= parm_max)) {
        stop("All entries in parm_min must be less than their corresponding entries in parm_max.")
    }

    if (any(parm0 > parm_max) | any(parm0 < parm_min)) {
        warning("All entries in parm0 should be within the bounds of their corresponding entries in parm_min, parm_max.")
    }

    parm0 <- signif(parm0, sigfigs)
    parm_min <- signif(parm_min, sigfigs)
    parm_max <- signif(parm_max, sigfigs)


    # Get initial conditions
    if (is.null(ics) & is.null(neweqns)) {
        ics <- get_ics(model)
    } else if (is.null(ics) & !is.null(neweqns)) {
        stop("You must specify initial conditions for your own model using the 'ics' argument.")
    }
    if (is.null(names(ics))) {
        stop("ics must be a named vector.")
    }

    # get default ggplot colours
    if (is.null("values")) {
        gghues <- seq(15, 375, length = length(ics) + 1)
        values <- hcl(h = gghues, l = 65, c = 100)[1:length(ics)]
    }

    if (length(values) != (length(ics))) {
        warning("The length of the manual colour scale vector ('values') must equal the number of model variables. Using default ggplot colours instead.")

        gghues <- seq(15, 375, length = length(ics) + 1)
        values <- hcl(h = gghues, l = 65, c = 100)[1:length(ics)]
    }

    # User Interface (UI)
    ui <- pageWithSidebar(
        headerPanel(name),
        sidebarPanel(
            lapply(seq_along(parm0),
                   function(x) sliderInput(inputId = names(parm0)[x], label = parm_names[x],
                                           value = parm0[x], min = parm_min[x], max = parm_max[x],
                                           step = slider_steps[x])
                   )
        ),
        mainPanel(
            plotOutput("plot1"),
            br(), br(), br(),
            tableOutput("table1")
        )
    )


    # Behind the scenes code (Server)
    server <- function(input, output){

        # Get initial population size (doesn't change with user input)
        START.N <- as.numeric(sum(ics))

        output$plot1 <- renderPlot({

            # Get parameters from user input
            parms_vector <- unlist(reactiveValuesToList(input))

            parms_vector <- c(parms_vector, N = START.N)

            # Time vector (total length input from shiny interface)
            times_vector <- seq(from = tstart, to = tmax, by = timestep)

            # Run ODE solver
            ODEoutput <- solve_eqns(eqns, ics, times = times_vector, parms = parms_vector)

            # Plot output
            plot_model(ODEoutput, linesize, textsize, xlabel, ylabel, legend_title, levels = names(ics), values, ...)
        })

        output$table1 <- renderTable({

            parms_vector <- unlist(reactiveValuesToList(input))
            parms_vector <- c(parms_vector, N = START.N)

            if ((model %in% c("SIR", "SIS")) & showtable == TRUE) {
                data.frame(
                    Parameter = c("gamma", "beta"),
                    Value = c(1/parms_vector["Ip"],
                              parms_vector["R0"] * (1/parms_vector["Ip"]) / parms_vector["N"])
                )
            } else if ((model %in% c("SIRS")) & showtable == TRUE) {
                data.frame(
                    Parameter = c("gamma", "delta", "beta"),
                    Value = c(1/parms_vector["Ip"],
                              1/parms_vector["Rp"],
                              parms_vector["R0"] * (1/parms_vector["Ip"]) / parms_vector["N"])
                )
            } else if ((model %in% c("SIRbirths", "SISbirths", "SIRvaccination")) & showtable == TRUE) {
                data.frame(
                    Parameter = c("gamma", "beta"),
                    Value = c(1/parms_vector["Ip"],
                              parms_vector["R0"] * (1/parms_vector["Ip"] + parms_vector["mu"]) / parms_vector["N"])
                )
            } else if ((model %in% c("SIRSbirths", "SIRSvaccination")) & showtable == TRUE) {
                data.frame(
                    Parameter = c("gamma", "delta", "beta"),
                    Value = c(1/parms_vector["Ip"],
                              1/parms_vector["Rp"],
                              parms_vector["R0"] * (1/parms_vector["Ip"] + parms_vector["mu"]) / parms_vector["N"])
                )
            }

        }, digits = -1, bordered = TRUE)

    }

    shinyApp(ui, server)
}
