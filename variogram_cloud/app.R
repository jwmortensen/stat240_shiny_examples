#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(EnviroStat)
library(MASS)

source("http://www.stat.washington.edu/peter/PASI/deformloglik.l1.R")
source("http://www.stat.washington.edu/peter/591/labs/Lab2/visualize.tps.warps.R")
source("http://www.stat.washington.edu/peter/591/labs/Lab2/draw2.R")  # to replace a function in EnviroStat

Pluie <- read.csv("http://www.stat.washington.edu/peter/PASI/pluieNA.novdec.csv")
Pluie <- Pluie[, -1]
PluieStations <- read.csv("http://www.stat.washington.edu/peter/PASI/pluieStations.Practicum.csv")
X <- as.matrix(PluieStations[, c("x", "y")])
dimnames(X) <- list(PluieStations$numero, c("x", "y"))

S <- cov(log(Pluie + 1), use = "pair")  # Pairwise covariances for log precip
t <- nrow(Pluie)

Xs <- scale(X,scale=FALSE)
Xs <- Xs/sqrt(sum(Xs^2))
Xsvd <- svd(Xs)
# Here we make sure diaonal elements of rotation matrix are positive
# by scaling columns by +/- 1.  This is to keep the rotation from flipping
# the orientation entirely.
Xsvd$v[,1] <- Xsvd$v[,1] * sign(Xsvd$v[1,1]) #
Xsvd$v[,2] <- Xsvd$v[,2] * sign(Xsvd$v[2,2])
Xt <- Xs %*% Xsvd$v
xscaling <- list(mean=attr(Xs,"scaled:center"),
                 #scale=attr(Xs,"scaled:scale")*sqrt(2*(n-1)),
                 scale=1,
                 rotation=Xsvd$v)
X0 <- X   # Save initial coordinates
X <- Xt   # Replace X with center, scaled, rotated coordinates

n <- nrow(X)

corr <- S/sqrt(diag(S) %o% diag(S))
dispersion <- 2 - 2 * corr
geo_dist <- as.matrix(dist(X, upper = T, diag = T))

vario_df <- cbind(data.frame(dist = as.numeric(geo_dist), disp = as.numeric(dispersion)),
                     # expand.grid(X[, 1], X[, 2]),
                     expand.grid(PluieStations$numero, PluieStations$numero))
location_df <- data.frame(x = X[, 1], y = X[, 2], id = PluieStations$numero)
names(vario_df) <- cbind("x", "y", "location1_id", "location2_id")

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Variogram Data"),
   fluidRow(column(width = 12,
                   helpText("Below we see the variogram cloud and transformed locations",
                            "for a French 10-day rainfall dataset",
                            "for 39 sites in the Languedoc-Roussillon region of southern France.",
                            "To see the locations associated with a point in the variogram cloud,",
                            "click on a point in the variogram. To see the variogram points",
                            "associated with a given",
                            "location, click on the location in the bottom plot."))),

   fluidRow(
     column(width = 12,
            plotOutput("variogram",
                       click = "vario_click")
     ),
     column(width = 12,
            plotOutput("locations",
                       click = "loc_click"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  np <- reactiveValues(vario_data = NULL, loc_data = NULL)
  observeEvent(input$vario_click, {
    np$vario_data <- nearPoints(vario_df, input$vario_click,
                          xvar = "x", yvar = "y",
                          threshold = 1000, maxpoints = 1)
    np$loc_data <- NULL
  })
  observeEvent(input$loc_click, {
    np$loc_data <- nearPoints(location_df, input$loc_click,
                              xvar = "x", yvar = "y",
                              threshold = 1000, maxpoints = 1)
    np$vario_data <- NULL
  })

  output$variogram <- renderPlot({
    plot(geo_dist, dispersion,
         ylab = "Dispersion", xlab = "Geographic Distance",
         main = "Variogram Cloud")
    if (!is.null(np$vario_data)) {
      points(np$vario_data$x, np$vario_data$y, col = "red", pch = 20)
    } else if (!is.null(np$loc_data)) {
      idx <- which(vario_df$location1_id %in% np$loc_data$id |
                     vario_df$location2_id %in% np$loc_data$id)
      tmp_df <- vario_df[idx, ]
      points(tmp_df$x, tmp_df$y, col = "red", pch = 20)
    }
    })
  output$locations <- renderPlot({
    plot(X[,1],X[,2], asp=1, type="n",
         xlab = "X", ylab = "Y", main = "Location")

    if (!is.null(np$vario_data)) {
      idx <- which(rownames(X) %in% np$vario_data[, c("location1_id", "location2_id")])
      text(X[-idx, 1],X[-idx, 2],dimnames(X)[[1]][-idx])
      text(X[idx, 1], X[idx, 2], dimnames(X)[[1]][idx], col = "red")
    } else if (!is.null(np$loc_data)) {
      idx <- which(rownames(X) %in% np$loc_data$id)
      text(np$loc_data$x, np$loc_data$y, np$loc_data$id, col = "red")
      text(X[-idx, 1], X[-idx, 2], dimnames(X)[[1]][-idx])
    } else {
      text(X[, 1],X[, 2], dimnames(X)[[1]])
    }

  })
}

# Run the application
shinyApp(ui = ui, server = server)

