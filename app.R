#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readxl)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)



# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Top U.S. Liberal Arts Colleges Since 1984"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    # Sidebar with a slider input for number of bins     # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("num",
                  "Top # in 2023:",
                  min = 1,
                  max = 50,
                  value = 30),
      # Input: Numeric entry for number of obs to view ----
      sliderInput(inputId = "range",
                  label = "Range:",
                  min=1984,
                  max=2023,
                  value = c(1984,2023),
                  sep = ""),
      h4("Hover over the plot to see the names of each college."),
      br(),
      p("Data from: Andrew G. Reiter, “U.S. News & World Report Historical Liberal Arts College and University Rankings.”",
        style = "font-size:10px;"),
      
      # checkboxGroupInput(inputId = "colleges",
      #             label = "Choose colleges to show:",
      #             choices= names,
      #             selected = df_plot[df_plot$Year==2023,]$Name)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Show a plot of the generated distribution
      plotlyOutput("linePlot"),
    ),
    position = c("left","right")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$linePlot <- renderPlotly({
    # generate time series line plots based on input$num from ui.R
    #create line plot of rankings of liberal arts colleges over time
    
    US_News_Rankings_Liberal_Arts_Colleges_Through_2023 <- read_excel("US-News-Rankings-Liberal-Arts-Colleges-Through-2023.xlsx")
    df1<-data.frame(US_News_Rankings_Liberal_Arts_Colleges_Through_2023)
    df2 = subset(df1, select = -c(IPEDS.ID,State))
    df_long<- gather(df2, key="X", value="Ranking", 2:39)
    colnames(df_long)[colnames(df_long) == "X"] = "Year"
    colnames(df_long)[colnames(df_long) == "College.Name"] = "Name"
    df_long$Year <-sub(".", "", df_long$Year, )
    df_long <- df_long[order(df_long$Name),]
    format(as.Date(df_long$Year, format="%Y"), "%Y")
    df_plot <- df_long
    df_plot <- df_plot[df_plot$Ranking != "NA", ]
    data_new <- df_plot[!is.na(as.numeric(df_plot$Ranking)), ] # Delete rows
    data_new$Ranking <- as.numeric(as.character(data_new$Ranking))
    data_new$Year <- as.numeric(as.character(data_new$Year))
    data_new <- data_new[order(-data_new$Year, data_new$Ranking),]
    
    
    names <-  c(data_new[data_new$Year == 2023 & data_new$Ranking<=input$num,]$Name)# create data frame subset
    years <-  c(data_new[data_new$Year>=input$range[1] & data_new$Year<=input$range[2],]$Year)# create data frame subset
    # colleges <- c(data_new[data_new$Year>=input$range[1] & data_new$Year<=input$range[2]&data_new$Name %in% input$colleges,]$Name)# create data frame subset
    df_plot <- data_new[data_new$Name %in% names,]
    df_plot <- df_plot[df_plot$Year %in% years,]
    #df_plot <- data_new[data_new$Name %in% colleges,]
    
    # draw the time series with the specified number of schools
    p <- ggplot(df_plot, aes(x=Year, y=Ranking, color=Name)) + geom_line(alpha=1) + theme(legend.position = "none")
    ggplotly(p)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

