#' SIR model without no births, deaths, or vaccination.

#' These equations describe the classic SIR model with constant population size, and no births or deaths.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SIR <- function(t, y, parms) {

    if (!all(c("R0", "Ip", "mu") %in% names(parms))) {
        stop("Missing parameters - the SIR model requires R0, Ip (infectious period), and mu (birth/death rate).")
    }
    # Parameter manipulations
    gamma <- 1/parms["Ip"]

    beta <- parms["R0"] * (gamma + parms["mu"]) / parms["N"]

    # Change in Susceptibles
    dS <- - beta * y["S"] * y["I"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"]

    return(list(c(dS, dI, dR)))

}


#' SIR model with vaccination at birth

#' These equations describe the classic SIR model with births and deaths, constant population size, and (optional) vaccination at birth.
#' @param t numeric vector of time points
#' @param y numeric vector of variables
#' @param parms named vector of model parameters.
#' @return equation list

SIRvaccination <- function(t, y, parms) {

    if (!all(c("R0", "Ip", "mu", "p") %in% names(parms))) {
        stop("Parameters missing - the SIR model w/ vaccination requires R0, Ip (infectious period), mu (birth/death rate), and p (proportion vaccinated).")
    }

    # Parameter manipulations
    gamma <- 1/parms["Ip"]

    beta <- parms["R0"] * (gamma + parms["mu"]) / parms["N"]

    # Change in Susceptibles
    dS <- parms["mu"] * (y["S"] + y["I"] + y["R"]) * (1 - parms["p"]) -
        beta * y["S"] * y["I"] -
        parms["mu"] * y["S"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"] -
        parms["mu"] * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"] -
        parms["mu"] * y["R"] +
        parms["p"] * parms["mu"] * (y["S"] + y["I"] + y["R"])

    return(list(c(dS, dI, dR)))

}



#' SIS model without no births, deaths, or vaccination.

#' These equations describe the classic SIS model with constant population size, and no births or deaths.
#' @param t numeric vector of time points
#' @param y numeric vector of variables
#' @param parms named vector of model parameters.
#' @return equation list

SIS <- function(t, y, parms) {

    if (!all(c("R0", "Ip") %in% names(parms))) {
        stop("Missing parameters - the SIS model requires R0 and Ip (infectious period).")
    }

    # Parameter manipulations
    gamma <- 1/parms["Ip"]

    beta <- parms["R0"] * gamma / parms["N"]

    # Change in Susceptibles
    dS <- - beta * y["S"] * y["I"] +
        gamma * y["I"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"]

    return(list(c(dS, dI)))

}

