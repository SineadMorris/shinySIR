#' Solve equations
#'
#' This function solves an ODE model using 'deSolve' and returns the output as a data frame.
#' @param eqns name of the model to be solved. Examples include: SIR and SIR vaccination.
#' @param ics named numeric vector specifying the initial conditions i.e. the initial values of all model variables.
#' @param times numerical vector indicating the time points at which the equation should be solved.
#' @param parms named numeric vector of parameter values.
#' @import dplyr ggplot2
#' @importFrom deSolve lsoda
#' @importFrom tidyr gather
#' @return data frame of model solutions in long format.
#'
solve_eqns <- function(eqns, ics, times, parms){

    trySolve <- tryCatch(deSolve::lsoda(y = ics,
                                   times = times,
                                   func = eqns,
                                   parms = parms),
                    error = function(e) e,
                    warning = function(w) w)

    if (inherits(trySolve, "condition")) {
        stop("ODE solutions are unreliable. Check model attributes e.g. equations, parameterization, and initial conditions.")
    } else {
        soln <- deSolve::lsoda(y = ics,
                               times = times,
                               func = eqns,
                               parms = parms)
    }

    output <- data.frame(soln) %>% tbl_df() %>%
        tidyr::gather(variable, value, 2:ncol(.))

    return(output)
}
