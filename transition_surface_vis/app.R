#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

n_vert <- 79
source("./src/useful_functions.R")
if (n_vert == 116) {
  load("./data/location_df.RData")
} else if (n_vert == 79) {
  load("./data/location79.RData")
}

team_list <- list("None" = NA,
                  "Cleveland" = 5,
                  "Golden State" = 9)
team_vec <- unlist(team_list)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Transition Surface Visualization"),
  fluidRow(
    column(width = 6,
           h3("Select Teams:"),
           selectInput("surf1", label = NULL,
                       choices = team_list, selected = 5),
           selectInput("surf2", label = NULL,
                       choices = team_list, selected = 9),
           checkboxInput("diff", label = "Difference Surface",
                         value = F)),
    column(width = 6,
           plotOutput("half_court", 
                      hover = hoverOpts(id = "xy_hover",
                                        delayType = "throttle",
                                        delay = 100),
                      height = "200px")
    )
  ),
  fluidRow(
    column(width = 6, 
           h3(textOutput("label1")),
           imageOutput("surface", height = "344px")),
    column(width = 6, 
           conditionalPanel(condition = "input.surf2 != 'NA' & input.diff == false",
                            h3(textOutput("label2")),
                            imageOutput("surface2", height ="344px")))
  )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$half_court <- renderPlot({
    par(mar = c(0,0,0,0))
    plot_half_court("", T)}, 
    width = 280, height = 200)
  # output$info <- renderPrint(input$xy_hover)
  # output$info <- 
  img1 <- reactive({
    np <- nearPoints(location_df, input$xy_hover, 
                     xvar = "x", yvar = "y", 
                     maxpoints = 1, threshold = 1000, addDist = TRUE)
    dir <- ifelse(input$diff == T, "diff", input$surf1)
    if (nrow(np) == 1) {
      paste("./images/", n_vert, "/", dir, "/inla", np$ID, ".png", sep = "")
    } else {
      paste("./images/", n_vert, "/", dir, "/inla15.png", sep = "")
    }})
  
  img2 <- reactive({
    np <- nearPoints(location_df, input$xy_hover, 
                     xvar = "x", yvar = "y", 
                     maxpoints = 1, threshold = 1000, addDist = TRUE)
    if (nrow(np) == 1) {
      paste("./images/", n_vert, "/", input$surf2, "/inla", np$ID, ".png", sep = "")
    } else {
      paste("./images/", n_vert, "/", input$surf2, "/inla15.png", sep = "")
    }})
  
  
  output$surface <- renderImage(
    list(src = img1(),
         contentType = "image/png",
         width = 482,
         height = 344
    ), deleteFile = F)
  output$surface2 <- renderImage(
    list(src = img2(),
         contentType = "image/png",
         width = 482,
         height = 344), 
    deleteFile = F
  )
  
  output$label1 <- renderText({
    if (input$diff) {
      paste0(names(team_vec)[!is.na(team_vec) & team_vec == input$surf1],
             " - ", names(team_vec)[!is.na(team_vec) & team_vec == input$surf2])
    } else {
      names(team_vec)[!is.na(team_vec) & team_vec == input$surf1]
    }
  })
  
  output$label2 <- renderText({
    names(team_vec)[!is.na(team_vec) & team_vec == input$surf2]
  })
})


# Run the application 
shinyApp(ui = ui, server = server)

