#' Get model display names
#'
#' This function gets the display name for any built-in model.
#' @param model character specifying the name of the built-in model.
#' @return character of the corresponding display name.
#' @export
#' @examples
#' get_name(model = "SIR")

get_name <- function(model){
        tmp <- data.frame(
            Model = c("SIR",
                      "SIRbirths",
                      "SIRvaccination",
                      "SIS",
                      "SISbirths",
                      "SIRS",
                      "SIRSbirths",
                      "SIRSvaccination"),
            Name = c("Interactive SIR model",
                     "Interactive SIR model with demography",
                     "Interactive SIR model with vaccination",
                     "Interactive SIS model",
                     "Interactive SIS model with demography",
                     "Interactive SIRS model",
                     "Interactive SIRS model with demography",
                     "Interactive SIRS model with vaccination"),
            stringsAsFactors = FALSE
        )

        tmp$Name[tmp$Model == model]
}
