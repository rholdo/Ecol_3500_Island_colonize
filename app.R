# Island colonization simulator
library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(tidyr)

col <- function(alpha){
  SP <- 10 # Number of species
  maxT <- 100 # Number of time steps
  X <- matrix(0, nrow = maxT, ncol = SP) # Matrix of X positions
  Y <- X
  theta <- runif(SP, min = -pi/2, max = pi/2)
  #theta <- rep(thetainit, SP)
  for (t in 2:maxT){
    # Move
    theta <- theta + (runif(SP) * 2 - 1) * pi * alpha
    # Keep theta between -pi and +pi
    ifelse(theta > pi, -(2 * pi - theta),
           ifelse(theta < -pi, 2 * pi + theta, theta))
    X[t,] = X[t-1,] + sin(theta);
    Y[t,] = Y[t-1,] + cos(theta);
  }
  X <- data.frame(X)
  Y <- data.frame(Y)
  names(X) <- paste0('SP',1:SP)
  names(Y) <- paste0('SP',1:SP)
  X.l <- X %>% pivot_longer(
    cols = everything(),
    names_to = 'SP',
    values_to = 'X'
  )
  Y.l <- Y %>% pivot_longer(
    cols = everything(),
    names_to = 'SP',
    values_to = 'Y'
  )
  X.l$Time <- rep(1:maxT, each = SP)
  Y.l$Time <- rep(1:maxT, each = SP)
  df <- left_join(X.l,Y.l)
  df <- df[,c(3,1,2,4)]
  return(df)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Island colonization"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("tor", "Tortuosity:",
                    min = 0, max = 0.2, value = 0),
        actionButton("run", "Colonize!")
      ),
      mainPanel(
        plotOutput("colPlot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$run, {
    df <- col(input$tor)
    output$colPlot <- renderPlot({
      ggplot(df) + geom_path(aes(x=X, y=Y, color=SP)) +
        coord_fixed(ratio = 1) +
        xlim(-49,49) +
        ylim(1,49) +
        theme_bw() +
        theme(legend.position = "none") +
        annotate("point", x = 0, y = 10, size = 20, color = 'gray', alpha = 0.5) +
        annotate("point", x = 0, y = 25, size = 20, color = 'gray', alpha = 0.5) +
        annotate("point", x = 0, y = 40, size = 20, color = 'gray', alpha = 0.5) +
        annotate("text", x = 0, y = 10, label = 'N') +
        annotate("text", x = 0, y = 25, label = 'I') +
        annotate("text", x = 0, y = 40, label = 'F')
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
