#' Model help
#'
#' This function prints a list of all built in models, along with their parameter arguments and a short description.
#' @return data frame of model descriptions.
#' @export
#' @examples
#' default_models()

default_models <- function(){
    print(
        data.frame(
            Model = c("SIR",
                      "SIRbirths",
                      "SIRvaccination",
                      "SIS",
                      "SISbirths",
                      "SIRS",
                      "SIRSbirths",
                      "SIRSvaccination"),
            Parameters = c("R0, infectious period (Ip)",
                           "R0, infectious period (Ip), birth rate (mu)",
                           "R0, infectious period (Ip), birth rate (mu), proportion vaccinated (p)",
                           "R0, infectious period (Ip)",
                           "R0, infectious period (Ip), birth rate (mu)",
                           "R0, infectious period (Ip), immune period (Rp)",
                           "R0, infectious period (Ip), immune period (Rp), birth rate (mu)",
                           "R0, infectious period (Ip), immune period (Rp), birth rate (mu), proportion vaccinated (p)"),
            Description = c("SIR model: lifelong immunity",
                            "SIR model with demography (equal birth and death rates)",
                            "SIR model with demography and vaccination at birth",
                            "SIS model: no immunity",
                            "SIS model with demography (equal birth and death rates)",
                            "SIRS model: temporary immunity",
                            "SIRS model with demography (equal birth and death rates)",
                            "SIRS model with demography and vaccination at birth")
        )
    )
}
