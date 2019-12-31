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
            Name = c("SIR",
                     "SIR with demography",
                     "SIR with vaccination",
                     "SIS",
                     "SIS with demography",
                     "SIRS",
                     "SIRS with demography",
                     "SIRS with vaccination"),
            stringsAsFactors = FALSE
        )

        tmp$Name[tmp$Model == model]
}
