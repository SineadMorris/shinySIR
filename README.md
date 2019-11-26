
<!-- README.md is generated from README.Rmd. Please edit that file -->
shinySIR
========

`shinySIR` provides interactive plotting for mathematical models of infectious disease spread. Users can choose from a variety of common built-in ODE models (such as the SIR, SIRS, and SIS models), or create their own. The package is a useful teaching tool as students can visualize how changing different parameters can impact model dynamics, with minimal knowledge of coding in R.

Basic information
-----------------

###### Author and maintainer

Sinead E Morris

###### Citing this package

Citation information can be found with `citation("shinySIR")`.

###### Getting help

If you encounter any bugs related to this package please contact the author directly. Additional descriptions of the mathematical theory and package functionality can be found in the vignette. Further details on the mathematical theory can also be found in the references cited below.

Quick start example
-------------------

To install the package from Github, first install and load `devtools`, then install `shinySIR` as follows

``` r
install.packages("devtools")
library("devtools")

install_github("SineadMorris/shinySIR")
```

To create an interactive plot of the SIR (susceptible-infected-recovered) model simply load the package and use the `run_shiny()` command. A window, similar to the one below, will appear. This shows the dynamics of the SIR model at the default parameter starting values; you can then change these values to explore their impact on model dynamics.

``` r
library(shinySIR)

run_shiny(model = "SIR")
```

<img src="shinySIRscreenshot.png" style="width:100.0%" />

Model specification
-------------------

A number of common models are supplied with the package. These include the SIR, SIRS, and SIS models. These can be accessed using the `model` argument, as shown above for the SIR model. Users can also specify their own models using the `neweqns` argument. `neweqns` takes a function containing the equations for the new model, with syntax as outlined in the example below. Note the syntax follows that used by the popular ODE solver `deSolve`.

#### Example of user-defined model

``` r
mySIRS <- function(t, y, parms) {

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
```

The interactive plot can then be created by calling this function with `neweqns`, and specifying vectors for the parameter attributes, including parameters starting values (`parm0`), names (`parm_names`), and minimum and maximum values for the shiny app (`parm_min` and `parm_max`, respectively).

``` r

run_shiny(model = "SIRS (w/out demography)", neweqns = mySIRS,
           parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
           parm_names = c("Transmission rate", "Recovery rate", "Loss of immunity"),
           parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
           parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))
```

<img src="shinySIRSscreenshot.png" style="width:100.0%" />