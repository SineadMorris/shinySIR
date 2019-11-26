#' Solve equations
#'
#' This function solves an ODE model using 'deSolve' and returns the output as a data frame.
#' @param model name of the model to be solved. Examples include: "SIR", "SIR vaccination". Default is "SIR".
#' @param ics named numeric vector specifying the initial conditions i.e. the initial values of all model variables. Default is c(S = 9999, I = 1, R = 0) for the SIR model.
#' @param tstart numerical value of form c(tmin, tmax) indicating the time to start simulations. Default value is 0.
#' @param timestep numerical value indicating time step be used when solving equations. Default value is 1/365.
#' @param tmax numerical value indicating maximum time point to be considered.
#' @param parm0 named numeric vector of starting parameter values. Names must correspond to those used in the model equations.
#' @param parm_names character vector of parameter names to be displayed in shiny menu. Must be in the same order as 'parm0'.
#' @param parm_min named numeric vector of minimum parameter values.
#' @param parm_max named numeric vector of maximum parameter values.
#' @return data frame of model solutions in long format.
#' @import shiny
#' @export
#'

run_shiny1 <- function(model = "SIR", neweqns = NULL, ics = c(S = 9999, I = 1, R = 0),
                      tstart = 0, timestep = 1, tmax = 365,
                      parm0 = c(R0 = 3, Ip = 7, mu = 0.25/365),
                      parm_names = c("R0", "Infectious period", "Birth rate", "Proportion vaccinated"),
                      parm_min = c("R0" = 0, "Ip" = 1, "mu" = 0),
                      parm_max = c("R0" = 20, "Ip" = 21, "mu" = 1/365),
                      linesize = 1.2, textsize = 14
){

    # User Interface (UI)
    ui <- pageWithSidebar(
        headerPanel(paste("Interactive", model, "model")),
        sidebarPanel(
            lapply(seq_along(parm0),
                   function(x) sliderInput(inputId = names(parm0)[x], label = parm_names[x],
                                           value = parm0[x],min = parm_min[x], max = parm_max[x]) )
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
    server <- function(input, output) {

        START.N <- as.numeric(sum(ics))

        output$plot1 <- renderPlot({

            if (exists(model) & is.null(neweqns)){
                eqns <- get(model, mode = "function")
            } else if (exists(model) & !is.null(neweqns)){
                warning("Your model name matches one of the built-in models. You can rename it using the 'model' argument.")
                eqns <- neweqns
            } else if (!exists(model) & is.null(neweqns)) {
                stop("If using your own model, you must specify a function for the equations with the 'neweqns' argument.")
            } else {
               eqns <- neweqns
            }


            # Get parameters from user input
            parms_vector <- unlist(reactiveValuesToList(input))

            parms_vector <- c(parms_vector, N = START.N)

            # Time vector (total length input from shiny interface)
            times_vector <- seq(from = tstart, to = tmax, by = timestep)

            # Run ODE solver
            SIRoutput <- solve_eqns(eqns, ics, times = times_vector, parms = parms_vector)

            # Plot output
            # **** put factor order as it comes in ics - so user can control how presented in legend
            plot_model(SIRoutput, linesize, textsize)
        })

        output$table1 <- renderTable({

            parms_vector <- unlist(reactiveValuesToList(input))

            parms_vector <- c(parms_vector, N = START.N)

            if(model == "SIR"){
                data.frame(
                    gamma = 1/parms_vector["Ip"],
                    beta = parms_vector["R0"] * ((1/parms_vector["Ip"]) + parms_vector["mu"]) / parms_vector["N"]
                    )
            }

        }, digits = -1 )

    }

    shinyApp(ui, server)
}

run_shiny1()


