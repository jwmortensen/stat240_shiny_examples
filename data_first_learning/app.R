#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
load("data_first_learning.RData")
question_selection <- as.list(questions$id)
names(question_selection) <- paste0("Q", questions$id, ". ", questions$text)
grades <- c("B-", "B", "B+", "A-", "A", "A+")

barchart <- function(Q, dat) {
  Q_start <- sprintf("Q%s_start", Q)
  Q_end <- sprintf("Q%s_end", Q)

  pairs <- dat[, c(Q_start, Q_end)]
  pairs <- na.omit(pairs)
  dat <- rbind(
    data.frame(Response = pairs[, 1], Time = "Beginning of course"),
    data.frame(Response = pairs[, 2], Time = "End of course")
    )
  
  ggplot(dat, aes(x = Response, fill = Time)) + 
          geom_bar(alpha = 0.5) +
          # geom_bar(aes_string(x = Q_end), fill = "blue", alpha = 0.5) +
          ggtitle(sprintf("Responses for Q%s", Q)) + 
          xlab("Response") +
          ylab("Count") +
          scale_x_discrete(drop = T) +
          theme_bw()
}

crosstab <- function(Q, dat) {
  Q_start <- sprintf("Q%s_start", Q)
  Q_end <- sprintf("Q%s_end", Q)
  as.data.frame.matrix(xtabs(~Q1_start + Q1_end, dat))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Learning from Big Data Survey Results"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       selectInput("question", "Question:", question_selection),
       sliderTextInput("grades",
                       "Student grade range:",
                       choices = grades,
                       selected = c("B-", "A+"),
                       grid = T,
                       from_fixed = F,
                       to_fixed = F),
       radioButtons("plot_or_table", label = "Display plot or table?", 
                    choices = list("Plot", "Table"),
                    selected = "Plot")
      ),
      
      # Show a plot of the generated distribution
     mainPanel(     
       conditionalPanel(
         condition = "input.plot_or_table == 'Plot'",
         plotOutput("barplot")
       ),
       conditionalPanel(
         condition = "input.plot_or_table == 'Table'",
         textOutput("crosstab_title"),
         tableOutput("crosstab")
       ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  idx <- reactive({input$grades[1] <= dat$grade & dat$grade <= input$grades[2]})
  
   output$barplot <- renderPlot({
      barchart(input$question, dat[idx(), ])
   })
   
   output$crosstab <- renderTable({
     crosstab(input$question, dat[idx(), ])
   }, rownames = T, bordered = T, caption = paste("Responses from the beginning of the course are given by the rows.",
                                                  "Responses at the end are given by the columns."))
   crosstab_title <- reactive({sprintf("Crosstab for Q%s", input$question) })
   output$crosstab_title <- renderText(crosstab_title())
}

# Run the application 
shinyApp(ui = ui, server = server)

