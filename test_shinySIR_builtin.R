require(shinySIR)

## SIR without vaccination --------------------------

run_shiny(model = SIR, parm0 = c(R0 = 13, Ip = 7, mu = 0.25/365),
          tmax = 600,
          parm_names = c("R0", "Infectious period", "Birth rate"),
          parm_min = c("R0" = 5, "Ip" = 1, "mu" = 0),
          parm_max = c("R0" = 20, "Ip" = 21, "mu" = 1/365))


## SIR with vaccination --------------------------

# Should work
run_shiny(model = SIRvaccination, parm0 = c(R0 = 3, Ip = 7, mu = 0.25/365, p = 0),
          parm_names = c("R0", "Infectious period", "Birth rate", "Proportion vaccinated"),
          parm_min = c("R0" = 0, "Ip" = 1, "mu" = 0, "p" = 0),
          parm_max = c("R0" = 20, "Ip" = 21, "mu" = 1/365, "p" = 1)
)


# Shouldn't work - CHECK
run_shiny(model = SIRvaccination, parm0 = c(R0 = 3, Ip = 7, mu = 0.25/365),
          parm_names = c("R0", "Infectious period", "Birth rate"),
          parm_min = c("R0" = 0, "Ip" = 1, "mu" = 0),
          parm_max = c("R0" = 20, "Ip" = 21, "mu" = 1/365)
)


## SIS --------------------------

run_shiny(model = SIS, ics = c(S = 999, I = 1), parm0 = c(R0 = 3, Ip = 7),
          parm_names = c("R0", "Infectious period"),
          parm_min = c("R0" = 0, "Ip" = 1),
          parm_max = c("R0" = 20, "Ip" = 21)
)
