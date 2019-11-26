require(shinySIR)

mySIRS <- function(t, y, parms) {

    # Change in Susceptibles
    dS <- - parms["beta"] * y["S"] * y["I"] +
        parms["delta"] * y["R"]

    # Change in Infecteds
    dI <- parms["beta"] * y["S"] * y["I"] -
        parms["gamma"] * y["I"]

    dR <- parms["gamma"] * y["I"] - parms["delta"] * y["R"]

    return(list(c(dS, dI, dR)))

}


mySIRS1 <- function(t, y, parms) {

    with(as.list(c(y, parms)),{

        # Change in Susceptibles
        dS <- - beta * S * I + delta * R

        # Change in Infecteds
        dI <- beta * S * I - gamma * I

        # Change in Recovereds
        dR <- gamma * I - delta * R

    return(list(c(dS, dI, dR)))
    })
}

# Should work
run_shiny(model = "SIRS (w/out demography)", neweqns = mySIRS1,
           parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
           parm_names = c("Transmission rate", "Recovery rate", "Immunity loss rate"),
           parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
           parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))


# Should get error about no eqns supplied
# Should work
run_shiny(model = "SIRS", neweqns = mySIRS,
          parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
          parm_names = c("Transmission rate", "Recovery rate", "Immunity loss rate"),
          parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
          parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))


# Should work
run_shiny(neweqns = mySIRS,
          parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
          parm_names = c("Transmission rate", "Recovery rate", "Immunity loss rate"),
          parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
          parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))
