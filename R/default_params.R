#' Get default parameters
#'
#' This function returns the default parameter vectors for a particular built-in model.
#' @param model name of the model to be solved. Examples include: SIR and SIR vaccination.
#' @return list of default parameter vectors.
#'
get_params <- function(model){

    params <- NULL

    ## SIR variants -------------------

    if(model == "SIR"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7),
            parm_names = c("R0", "Infectious period"),
            parm_min = c(R0 = 0, Ip = 1),
            parm_max = c(R0 = 20, Ip = 21)
        )
    }

    if(model == "SIRbirths"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7, mu = round(0.25/365, 3)),
            parm_names = c("R0", "Infectious period", "Birth rate"),
            parm_min = c(R0 = 0, Ip = 1, mu = 0),
            parm_max = c(R0 = 20, Ip = 21, mu = round(10/365, 3))
        )
    }


    if(model == "SIRvaccination"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7, mu = round(0.25/365, 3), p = 0.75),
            parm_names = c("R0", "Infectious period", "Birth rate", "Proportion vaccinated"),
            parm_min = c(R0 = 0, Ip = 1, mu = 0, p = 0),
            parm_max = c(R0 = 20, Ip = 21, mu = round(10/365, 3), p = 1)
        )
    }

    ## SIS variants -------------------

    if(model == "SIS"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7),
            parm_names = c("R0", "Infectious period"),
            parm_min = c(R0 = 0, Ip = 1),
            parm_max = c(R0 = 20, Ip = 21)
        )
    }

    if(model == "SISbirths"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7, mu = round(0.25/365, 3)),
            parm_names = c("R0", "Infectious period", "Birth rate"),
            parm_min = c(R0 = 0, Ip = 1, mu = 0),
            parm_max = c(R0 = 20, Ip = 21, mu = round(10/365, 3))
        )
    }

    ## SIRS variants -------------------

    if(model == "SIRS"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7, Rp = 365),
            parm_names = c("R0", "Infectious period", "Duration of immunity"),
            parm_min = c(R0 = 0, Ip = 1, Rp = 30),
            parm_max = c(R0 = 20, Ip = 21, Rp = 30 * 365)
        )
    }

    if(model == "SIRSbirths"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7, Rp = 365, mu = round(0.25/365, 3)),
            parm_names = c("R0", "Infectious period", "Duration of immunity", "Birth rate"),
            parm_min = c(R0 = 0, Ip = 1, Rp = 30, mu = 0),
            parm_max = c(R0 = 20, Ip = 21, Rp = 30 * 365, mu = round(10/365, 3))
        )
    }

    if(model == "SIRSvaccination"){
        params <- list(
            parm0 = c(R0 = 3, Ip = 7, Rp = 365, mu = round(0.25/365, 3), p = 0.75),
            parm_names = c("R0", "Infectious period", "Duration of immunity", "Birth rate", "Proportion vaccinated"),
            parm_min = c(R0 = 0, Ip = 1, Rp = 30, mu = 0, p = 0),
            parm_max = c(R0 = 20, Ip = 21, Rp = 30 * 365, mu = round(10/365, 3), p = 1)
        )
    }
    return(params)
}

