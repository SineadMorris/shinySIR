#' Solve equations
#'
#' This function solves an ODE model using 'deSolve' and returns the output as a data frame.
#' @param model name of the model to be solved. Examples of built-in models are: "SIR", "SIR vaccination". Default is "SIR".
#' @param neweqns function specifying the equations of the user-definied model. Only to be used if a model is required that is not built-in. Default is NULL.
#' @param ics named numeric vector specifying the initial conditions i.e. the initial values of all model variables. Default is c(S = 9999, I = 1, R = 0) for the SIR model.
#' @param tstart numerical value of form c(tmin, tmax) indicating the time to start simulations. Default value is 0.
#' @param timestep numerical value indicating time step be used when solving equations. Default value is 1/365.
#' @param tmax numerical value indicating maximum time point to be considered.
#' @param parm0 named numeric vector of starting parameter values. Names must correspond to those used in the model equations.
#' @param parm_names character vector of parameter names to be displayed in shiny menu. Must be in the same order as 'parm0'.
#' @param parm_min named numeric vector of minimum parameter values.
#' @param parm_max named numeric vector of maximum parameter values.
#' @param sigfigs number of significant figures to round parameter input vectors. Default is 4.
#' @param linesize numeric value for line width in ggplot output. Default is 1.2.
#' @param textsize numeric value for textsize in ggplot output. Default is 12.
#' @param ... extra arguments to be passed through to ggplot scale_colour_discrete e.g. 'labels' to change the legend names.
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
                      linesize = 1.2, textsize = 14, ...
                      ){

    if (exists(model) & is.null(neweqns)){
        eqns <- get(model, mode = "function")
    } else if (exists(model) & !is.null(neweqns)){
        warning("Your model name matches one of the built-in models. You can rename it using the 'model' argument.")
        eqns <- neweqns
    } else if (!exists(model) & is.null(neweqns)) {
        stop("Model name not recognized. Try one of the built-in models or specify your own with the 'neweqns' argument.")
    } else {
        eqns <- neweqns
    }

    if (is.null(parm0) & is.null(neweqns)) {
        params <- get_params(model)

        parm0 <- params$parm0
        parm_names <- params$parm_names

        parm_min <- params$parm_min
        parm_max <- params$parm_max
    }

    if (is.null(ics) & is.null(neweqns)) {
        ics <- get_ics(model)
    }

    parm0 <- signif(parm0, sigfigs)
    parm_min <- signif(parm_min, sigfigs)
    parm_max <- signif(parm_max, sigfigs)

    # Check parameters appear in the same order in all vectors
    if ( !( all(sapply(list(names(parm0), names(parm_min), names(parm_max)), function(x) x == names(parm0)))) ){
        stop("the parameters in parm0, parm_names, parm_min, and parm_max must appear in the same order.")
    }

    # User Interface (UI)
    ui <- pageWithSidebar(
        headerPanel(paste("Interactive model:", model)),
        sidebarPanel(
            lapply(seq_along(parm0),
                   function(x) sliderInput(inputId = names(parm0)[x], label = parm_names[x],
                                           value = parm0[x],min = parm_min[x], max = parm_max[x])
                   )
        ),
        mainPanel(
            plotOutput("plot1"),
            tableOutput("table1"),
            tags$footer("created with shinySIR",
                        align = "left",
                        type = "text",
                        style = "bottom:100; padding: 10px"
            )
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
            plot_model(ODEoutput, linesize, textsize, levels = names(ics), ...)
        })

        output$table1 <- renderTable({

            parms_vector <- unlist(reactiveValuesToList(input))
            parms_vector <- c(parms_vector, N = START.N)

            if (model %in% c("SIR", "SIS")) {
                data.frame(
                    gamma = 1/parms_vector["Ip"],
                    beta = parms_vector["R0"] * (1/parms_vector["Ip"]) / parms_vector["N"]
                )
            } else if (model %in% c("SIRS")) {
                data.frame(
                    gamma = 1/parms_vector["Ip"],
                    delta = 1/parms_vector["Rp"],
                    beta = parms_vector["R0"] * (1/parms_vector["Ip"]) / parms_vector["N"]
                )
            } else if (model %in% c("SIRbirths", "SISbirths", "SIRvaccination")) {
                data.frame(
                    gamma = 1/parms_vector["Ip"],
                    beta = parms_vector["R0"] * (1/parms_vector["Ip"] + parms_vector["mu"]) / parms_vector["N"]
                )
            } else if (model %in% c("SIRSbirths", "SIRSvaccination")) {
                data.frame(
                    gamma = 1/parms_vector["Ip"],
                    delta = 1/parms_vector["Rp"],
                    beta = parms_vector["R0"] * (1/parms_vector["Ip"] + parms_vector["mu"]) / parms_vector["N"]
                )
            }

        }, digits = -1 )

    }

    shinyApp(ui, server)
}
