#' Launch a shiny-app simulating the seasonal SEIR model
#'
#' #' This launches an app running the SEIR model i.e. a model incorporating latency and seasonal forcing in transmission.
#'
#' @details
#' Launch app for details
#' @export
#' @examples
#' \dontrun{seir.app}

seir.app <- shinyApp(
    # This creates the User Interface (UI)
    ui = pageWithSidebar(
            headerPanel("Seasonally forced SEIR"),
            sidebarPanel(
            sliderInput("beta0", "Transmission (yr^-1):", 1000,
                          min = 0, max = 3000),
            sliderInput("beta1", "Seasonality:", 0,
                          min = 0, max = 1),
            sliderInput("Ip", "Infectious period (days)", 5,
                          min = 1, max = 100),
            sliderInput("oneoversigma", "Latent period (days):", 8,
                          min = 1, max = 100),
            sliderInput("mu", "birth rate (per 1000):", 0.02,
                          min = 0, max = .1),
            sliderInput("T", "Time range:",
                              min = 0, max = 100, value = c(0,20)),
            checkboxInput("lg", "un-Log", TRUE)
            ),
            mainPanel(
              tabsetPanel(
                  tabPanel("Time", plotOutput("plot1")),
                  tabPanel("Phase plane", plotOutput("plot2")),
                   tabPanel("Details",
                       withMathJax(
                        helpText("Susceptible $$\\frac{dS}{dt} = \\mu (N - S) - \\frac{\\beta(t) I S}{N}$$"),
                        helpText("Exposed $$\\frac{dE}{dt} = \\frac{\\beta(t) I S}{N} - (\\mu+\\sigma) E$$"),
                        helpText("Infectious $$\\frac{dI}{dt} = \\sigma E - (\\mu+\\gamma) I$$"),
                       helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu R$$"),
                       helpText("Seasonality $$\\beta(t) =  \\beta_0 (1 + \\beta_1 cos(2 \\pi t))$$"),
                       helpText("Reproductive ratio $$R_0 =  \\frac{\\sigma}{\\sigma +\\mu} \\frac{\\beta}{\\gamma+\\mu}$$"),
                        helpText("Reference: DJD Earn, P Rohani, BM Bolker, BT Grenfell (2000) A simple model for complex dynamical transitions in epidemics.
                         Science 287: 667-670"),
                       helpText("")
                       ))
              )
            )
    ),

    # This creates the 'behind the scenes' code (Server)
    server = function(input, output) {

        textsize <- 14

        mytheme <- theme(legend.position = "top",
                         axis.title = element_text(size = textsize + 2),
                         axis.text = element_text(size = textsize),
                         legend.title = element_text(size = textsize + 2),
                         legend.text = element_text(size = textsize))

        seirmod2 <- function(t, y, params){

              with(as.list(c(y, params)),{

              dS <- mu * (N - S) - beta0 * (1 + beta1 * cos(2*pi*t)) * S * I / N
              dE <- beta0 * (1 + beta1 * cos(2*pi*t)) * S * I / N - (mu + sigma) * E
              dI <- sigma * E - (mu + gamma) * I
              dR <- gamma * I - mu * R

              list(c(dS, dE, dI, dR))
              })
        }

      output$plot1 <- renderPlot({

          times <- seq(0, input$T[2], by = 1/100)

          paras <- c(mu = input$mu, N = 1, beta0 = input$beta0, beta1 = input$beta1,
                     sigma = 365/input$oneoversigma, gamma = 365/input$Ip)

          ystart <- c(S = 0.06, E = 0, I = 0.001, R = 0.939)

          R0 <- round(with(as.list(paras), sigma / (sigma + mu) * beta0 / (gamma + mu)), 1)

          out <- ode(y = ystart, times = times, func = seirmod2, parms = paras) %>%
              as.data.frame() %>% filter(time > input$T[1])

          out %>% ggplot() +
              geom_line(aes(x = time, y = S, colour = "aS"), size = 1, alpha = 0.7) +
              geom_line(aes(x = time, y = R, colour = "dR"), size = 1, alpha = 0.7) +
              geom_line(aes(x = time, y = E * 100, colour = "bE"), size = 1, alpha = 0.7) +
              geom_line(aes(x = time, y = I * 100, colour = "cI"), size = 1, alpha = 0.7) +
              scale_y_continuous("Fraction S and R", limits = c(0, 1),
                                 sec.axis = sec_axis(~./100, name = "Fraction E and I")) +
              scale_x_continuous("Time") +
              scale_colour_discrete("Compartment", labels = c("S", "E", "I", "R")) +
              theme_bw() + mytheme + theme(legend.position = "top")

    })

    output$plot2 <- renderPlot({
        times <- seq(0, input$T[2], by = 1/100)

        paras <- c(mu = input$mu, N = 1, beta0 = input$beta0, beta1 = input$beta1,
                 sigma = 365/input$oneoversigma, gamma = 365/input$Ip)

        ystart <- c(S = 0.06, E = 0, I = 0.001, R = 0.939)

        R0 <- with(as.list(paras), sigma/(sigma + mu) * beta0 / (gamma + mu) )

        out <- ode(y = ystart, times = times, func = seirmod2, parms = paras) %>%
            as.data.frame() %>% filter(time > input$T[1])

        linesize <- 0.8

        round_axes <- function(decimals = 0){
            function(x) as.character(round(x, decimals))
        }

        out %>% ggplot(aes(x = S, y = I)) + geom_path(size = linesize, alpha = 0.7) +
            geom_vline(aes(xintercept = 1/R0, colour = "S-isocline"), size = linesize + 0.2, show.legend = FALSE) +
            geom_line(aes(x = S,
                          y = paras["mu"] * (1 - S) / (paras["beta0"] * S),
                          colour = "I-isocline"), size = linesize + 0.2) +
            scale_y_continuous("Fraction I", trans = ifelse(input$lg == TRUE, "log", "identity"),
                               limits = c(min(out$I), max(out$I)), labels = round_axes(4)) +
            scale_x_continuous("Fraction S", trans = ifelse(input$lg == TRUE, "log", "identity"),
                               limits = c(min(out$S), max(out$S)), labels = round_axes(4)) +
            ggtitle(paste("R0 =", round(R0, 1))) +
            scale_colour_manual("", values = c("sienna", "forestgreen")) +
            theme_bw() + mytheme  + theme(legend.position = "right")

      })
    }
)
