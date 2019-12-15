#' Launch a shiny-app simulating the SEIRS model
#'
#' This launches an app running the SEIRS model i.e. a model incorporating latency and loss of immunity.
#' @importFrom deSolve ode
#' @importFrom graphics abline axis curve legend lines mtext par plot title
#' @importFrom stats D
#' @export
#' @details
#' Launch app for details
#' @examples
#' \dontrun{seirs.app}


seirs.app <- shinyApp(
    # This creates the User Interface (UI)
    ui <- pageWithSidebar(
            headerPanel("SEIRS periodicity"),
            sidebarPanel(
            sliderInput("beta", "Transmission (yr^-1):", 500,
                          min = 0, max = 3000),
            sliderInput("oneoveromega", "Immune duration (years):", 4,
                          min = 0, max = 100),
            sliderInput("Ip", "Infectious period (days)", 5,
                          min = 1, max = 100),
            sliderInput("oneoversigma", "Latent period (days):", 8,
                          min = 1, max = 100),
            sliderInput("oneovermu", "Life expectancy (years):", 10,
                          min = 1, max = 100),
            sliderInput("T", "Time range:",
                              min = 0, max = 100, value = c(0,20)),
            checkboxInput("lg", "un-Log", TRUE)
    ),
        mainPanel(
          tabsetPanel(
              tabPanel("Time", plotOutput("plot1")),
              tabPanel("Phase plane", plotOutput("plot2")),
               tabPanel("Equations",
                   withMathJax(
                    helpText("Susceptible $$\\frac{dS}{dt} = \\mu (N - S) - \\frac{\\beta I S}{N} + \\omega R$$"),
                    helpText("Exposed $$\\frac{dE}{dt} = \\frac{\\beta I S}{N} - (\\mu+\\sigma) E$$"),
                    helpText("Infectious $$\\frac{dI}{dt} = \\sigma E - (\\mu+\\gamma) I$$"),
                   helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu R - \\omega R$$"),
                   helpText("Reproductive ratio $$R_0 =  \\frac{\\sigma}{\\sigma +\\mu} \\frac{\\beta}{\\gamma+\\mu}$$"),
                   helpText("")
                   ))

          )
        )
    ),

    # This creates the 'behind the scenes' code (Server)
    server <- function(input, output) {

        textsize <- 14

        mytheme <- theme(legend.position = "top",
                         axis.title = element_text(size = textsize + 2),
                         axis.text = element_text(size = textsize),
                         legend.title = element_text(size = textsize + 2),
                         legend.text = element_text(size = textsize))

        seirsmod <- function(t, y, params){

          with(as.list(c(y, params)),{

          dS = mu * (1 - S) - beta * S * I + omega * R
          dE = beta * S * I - (mu + sigma) * E
          dI = sigma * E - (mu + gamma) * I
          dR = gamma * I - mu * R - omega * R

          list(c(dS, dE, dI, dR))
          })
        }

      output$plot1 <- renderPlot({

        times  <- seq(0, input$T[2], by = 1/100)
        paras  <- c(mu = 1/input$oneovermu, beta =  input$beta, sigma = 365/input$oneoversigma,
                     gamma = 365/input$Ip, omega = 1/input$oneoveromega)
        ystart <- c(S = 0.539, E = 0, I = 0.001, R = 0.46)

        R0 <- with(as.list(paras),{beta * sigma / ((mu + sigma) * (mu + gamma))})

        Sstar <- 1/R0
        Istar <- paras["mu"] * (1 - Sstar) / (paras["beta"] * Sstar - (paras["omega"] * paras["gamma"]) / (paras["mu"] + paras["omega"]))
        Estar <- (paras["mu"] + paras["gamma"]) * Istar / paras["sigma"]
        Rstar <- paras["gamma"] * Istar / (paras["mu"] + paras["omega"])

        star <- as.list(c(S = Sstar, E = Estar, I = Istar, R = Rstar, paras))
        names(star)[1:4] <- c("S", "E", "I", "R")

        fns <- list(quote(mu * (1 - S) - beta * S * I + omega * R),
                    quote(beta * S * I - (mu + sigma) * E),
                    quote(sigma * E - (mu + gamma) * I),
                    quote(gamma * I - mu * R - omega * R))

        aa <- as.vector(c(sapply(fns, D, "S"), sapply(fns, D, "E"), sapply(fns, D, "I"), sapply(fns, D, "R")))

        JJ <- matrix(sapply(aa, eval, star), ncol = 4)

        EE <- eigen(JJ)$values
        WW <- which.max(Im(EE))
        rp <- 2*pi/Im(EE[WW])

        out <- ode(y = ystart, times = times, func = seirsmod, parms = paras) %>%
            as.data.frame() %>% filter(time > input$T[1])

        out %>% ggplot() +
            geom_line(aes(x = time, y = S, colour = "aS"), size = 1, alpha = 0.7) +
            geom_line(aes(x = time, y = R, colour = "dR"), size = 1, alpha = 0.7) +
            geom_line(aes(x = time, y = E * 50, colour = "bE"), size = 1, alpha = 0.7) +
            geom_line(aes(x = time, y = I * 50, colour = "cI"), size = 1, alpha = 0.7) +
            scale_y_continuous("Fraction S / R", limits = c(0, 1),
                               sec.axis = sec_axis(~./50, name = "Fraction E / I")) +
            scale_x_continuous("Time") +
            scale_colour_discrete("Compartment", labels = c("S", "E", "I", "R")) +
            theme_bw() + mytheme + theme(legend.position = "top")

    })

    output$plot2 <- renderPlot({

        times <- seq(0, input$T[2], by = 1/100)
        paras <- c(mu = 1/input$oneovermu, beta = input$beta, sigma = 365/input$oneoversigma,
                   gamma = 365/input$Ip, omega = 1/input$oneoveromega)
        ystart <- c(S = 0.539, E = 0, I = 0.001, R = 0.46)

        R0 <- with(as.list(paras),{ beta * sigma / ((mu + sigma) * (mu + gamma)) })

        Sstar <- 1/R0
        Istar <- paras["mu"] * (1 - Sstar) / (paras["beta"] * Sstar - (paras["omega"] * paras["gamma"]) / (paras["mu"] + paras["omega"]))
        Estar <- (paras["mu"] + paras["gamma"]) * Istar / paras["sigma"]
        Rstar <- paras["gamma"] * Istar / (paras["mu"] + paras["omega"])

        star <- as.list(c(S = Sstar, E = Estar, I = Istar, R = Rstar, paras))
        names(star)[1:4] = c("S", "E", "I", "R")

        fns <- list(quote(mu * (1  - S)  - beta * S * I  + omega * R),
                    quote(beta * S * I - (mu + sigma) * E),
                    quote(sigma * E - (mu + gamma) * I),
                    quote(gamma * I - mu * R - omega * R)
                    )

        aa <- as.vector(c(sapply(fns, D, "S"), sapply(fns, D, "E"), sapply(fns, D, "I"), sapply(fns, D, "R")))

        JJ <- matrix(sapply(aa, eval, star), ncol = 4)

        EE <- eigen(JJ)$values
        WW <- which.max(Im(EE))
        rp <- 2*pi/Im(EE[WW])


        out <- ode(y = ystart, times = times, func = seirsmod, parms = paras) %>%
            as.data.frame() %>% filter(time > input$T[1])

        linesize <- 0.8

        round_axes <- function(decimals = 0){
            function(x) as.character(round(x, decimals))
        }

        out %>% ggplot(aes(x = S, y = I)) + geom_path(size = linesize, alpha = 0.7) +
            geom_vline(aes(xintercept = 1/R0, colour = "S-isocline"), size = linesize + 0.2, show.legend = FALSE) +
            geom_line(aes(x = S,
                          y = paras["mu"] * (1 - S) / (paras["beta"] * S - (paras["omega"] * paras["gamma"]) / (paras["mu"] + paras["omega"]) ),
                          colour = "I-isocline"), size = linesize + 0.2) +
            scale_y_continuous("Fraction I", trans = ifelse(input$lg == TRUE, "log", "identity"),
                               limits = c(min(out$I), max(out$I)), labels = round_axes(4)) +
            scale_x_continuous("Fraction S", trans = ifelse(input$lg == TRUE, "log", "identity"),
                               limits = c(min(out$S), max(out$S)), labels = round_axes(4)) +
            ggtitle(paste("R0 =", round(R0, 1), ", Period =", round(rp,2))) +
            scale_colour_manual("", values = c("sienna", "forestgreen")) +
            theme_bw() + mytheme  + theme(legend.position = "right")

      })
    }
)

