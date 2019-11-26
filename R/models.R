#' SIR model

#' These equations describe the classic SIR model with no births or deaths.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SIR <- function(t, y, parms) {

    if (!all(c("R0", "Ip") %in% names(parms))) {
        stop("Missing parameters - the SIR model requires R0 and Ip (infectious period).")
    }
    # Parameter manipulations
    gamma <- 1/parms["Ip"]

    beta <- parms["R0"] * gamma / parms["N"]

    # Change in Susceptibles
    dS <- - beta * y["S"] * y["I"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"]

    return(list(c(dS, dI, dR)))

}


#' SIR model with demography

#' These equations describe the classic SIR model with equal births and deaths.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SIRbirths <- function(t, y, parms) {

    if (!all(c("R0", "Ip") %in% names(parms))) {
        stop("Missing parameters - the SIR model with demography requires R0, Ip (infectious period), and mu (birth/death rate).")
    }
    # Parameter manipulations
    gamma <- 1/parms["Ip"]

    beta <- parms["R0"] * (gamma + parms["mu"]) / parms["N"]

    # Change in Susceptibles
    dS <- parms["mu"] * (y["S"] + y["I"] + y["R"]) -
        beta * y["S"] * y["I"] -
        parms["mu"] * y["S"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"] -
        parms["mu"] * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"] -
        parms["mu"] * y["R"]

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
        stop("Parameters missing - the SIR model with vaccination requires R0, Ip (infectious period), mu (birth/death rate), and p (proportion vaccinated).")
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



#' SIS model

#' These equations describe the classic SIS model with no births or deaths.
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


#' SIS model with demography

#' These equations describe the classic SIR model with equal births and deaths.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SISbirths <- function(t, y, parms) {

    if (!all(c("R0", "Ip", "mu") %in% names(parms))) {
        stop("Missing parameters - the SIS model with demography requires R0, Ip (infectious period), and mu (birth/death rate).")
    }
    # Parameter manipulations
    gamma <- 1/parms["Ip"]

    beta <- parms["R0"] * (gamma + parms["mu"]) / parms["N"]

    # Change in Susceptibles
    dS <- parms["mu"] * (y["S"] + y["I"]) -
        beta * y["S"] * y["I"] -
        parms["mu"] * y["S"] +
        gamma * y["I"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"] -
        parms["mu"] * y["I"]

    return(list(c(dS, dI)))

}


#' SIRS model

#' These equations describe the classic SIRS model without births or deaths.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SIRS <- function(t, y, parms) {


    if (!all(c("R0", "Ip", "Rp") %in% names(parms))) {
        stop("Missing parameters - the SIRS model requires R0, Ip (infectious period) and Rp (duration of immunity).")
    }

    # Parameter manipulations
    gamma <- 1/parms["Ip"]
    delta <- 1/parms["Rp"]

    beta <- parms["R0"] * gamma / parms["N"]


    # Change in Susceptibles
    dS <- - beta * y["S"] * y["I"] +
        delta * y["R"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"] -
        delta * y["R"]

    return(list(c(dS, dI, dR)))

}


#' SIRS model with demography

#' These equations describe the classic SIRS model with equal birth and death rates.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SIRSbirths <- function(t, y, parms) {


    if (!all(c("R0", "Ip", "Rp", "mu") %in% names(parms))) {
        stop("Missing parameters - the SIRS model with demography requires R0, Ip (infectious period) , Rp (duration of immunity), and mu (birth rate).")
    }

    # Parameter manipulations
    gamma <- 1/parms["Ip"]
    delta <- 1/parms["Rp"]

    beta <- parms["R0"] * (gamma + parms["mu"]) / parms["N"]


    # Change in Susceptibles
    dS <- parms["mu"] * (y["S"] + y["I"] + y["R"]) -
        beta * y["S"] * y["I"] +
        delta * y["R"] -
        parms["mu"] * y["S"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"] -
        parms["mu"] * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"] -
        delta * y["R"] -
        parms["mu"] * y["R"]

    return(list(c(dS, dI, dR)))

}



#' SIRS model with vaccination at birth

#' These equations describe the classic SIRS model with equal birth and death rates and vaccination at birth.
#' @param t numeric vector of time points.
#' @param y numeric vector of variables.
#' @param parms named vector of model parameters.
#' @return equation list

SIRSvaccination <- function(t, y, parms) {


    if (!all(c("R0", "Ip", "Rp", "mu", "p") %in% names(parms))) {
        stop("Missing parameters - the SIRS model with demography requires R0, Ip (infectious period) , Rp (duration of immunity), mu (birth rate), and p (proportion vaccinated).")
    }

    # Parameter manipulations
    gamma <- 1/parms["Ip"]
    delta <- 1/parms["Rp"]

    beta <- parms["R0"] * (gamma + parms["mu"]) / parms["N"]


    # Change in Susceptibles
    dS <- parms["mu"] * (y["S"] + y["I"] + y["R"]) * (1 - parms["p"]) -
        beta * y["S"] * y["I"] +
        delta * y["R"] -
        parms["mu"] * y["S"]

    # Change in Infecteds
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"] -
        parms["mu"] * y["I"]

    # Change in Recovereds
    dR <- gamma * y["I"] -
        delta * y["R"] -
        parms["mu"] * y["R"] +
        parms["p"] * parms["mu"] * (y["S"] + y["I"] + y["R"])

    return(list(c(dS, dI, dR)))

}
