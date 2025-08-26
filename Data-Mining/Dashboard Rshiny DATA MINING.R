# DASHBOARD

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(corrplot)

# Load data
stroke_data <- read.csv("D:\\COLLEGE\\SEMESTER 6\\FP\\DATMIN\\FP\\FileClean_Stroke.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Prediksi Stroke"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Visualisasi", tabName = "visualisasi", icon = icon("chart-bar")),
      menuItem("Authors", tabName = "authors", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              div(h1(strong("Welcome to the Stroke Dashboard")),style="text-align: center;")
      ),
      tabItem(tabName = "dataset",
              h3("Keterangan"),
              br(),
              downloadButton("downloadData", "Download Data"),
              downloadButton("downloadPDF", "Download PDF"),
              downloadButton("downloadCSV", "Download CSV"),
              br(),
              br(),
              h3("Data"),
              br(),
              dataTableOutput("strokeTable"),
              br(),
              tabsetPanel(
                tabPanel("Struktur", verbatimTextOutput("struktur")),
                tabPanel("Summary", verbatimTextOutput("summary"))
              )
      ),
      tabItem(tabName = "visualisasi",
              fluidRow(
                valueBoxOutput("total_patients"),
                valueBoxOutput("stroke_cases"),
                valueBoxOutput("stroke_rate")
              ),
              fluidRow(
                box(title = "Numerical Variable Histograms", width = 12,
                    plotOutput("numeric_histograms")
                )
              ),
              fluidRow(
                box(title = "Scatter Plot (Age vs. BMI)", width = 6,
                    plotOutput("scatter_age_bmi")),
                box(title = "Box Plot (BMI by Gender)", width = 6,
                    plotOutput("box_bmi_gender"))
              )
              
      ),
      # Author
      tabItem(tabName = "authors",
              div(h1(strong(("AUTHORS"))),style="text-align: center;"),
              div(("This dashboard was compiled by"),style="text-align: center;"),
              br(),
              box(width = 6,
                  status = NULL,
                  div(style = "text-align: center; margin-bottom: -180px;",
                      imageOutput("ferica")),
                  collapsible = TRUE,
                  collapsed = FALSE,
                  title = strong("Ferica Sunan Sari / 5003211109")
              ),
              
              box(width = 6,
                  status = NULL,
                  div(style = "text-align: center; margin-bottom: -180px;",
                      imageOutput("eli")),
                  collapsible = TRUE,
                  collapsed = FALSE,
                  title = strong("Elizabeth Sianturi / 5003211119")
              ),
              
              box(width = 6,
                  status = NULL,
                  div(style = "text-align: center; margin-bottom: -180px;",
                      imageOutput("glorya")),
                  collapsible = TRUE,
                  collapsed = FALSE,
                  title = strong("Glorya Debora Anggitha / 5003211147")
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Overview Tab
  output$total_patients <- renderValueBox({
    valueBox(nrow(stroke_data), "Total Patients", icon = icon("users"), color = "aqua")
  })
  
  output$stroke_cases <- renderValueBox({
    valueBox(sum(stroke_data$stroke == 1), "Stroke Cases", icon = icon("heartbeat"), color = "red")
  })
  
  output$stroke_rate <- renderValueBox({
    valueBox(round(sum(stroke_data$stroke == 1) / nrow(stroke_data) * 100, 2), "Stroke Rate (%)", icon = icon("percentage"), color = "yellow")
  })
  
  # Visualisasi Tab
  output$numeric_histograms <- renderPlot({
    numeric_vars <- sapply(stroke_data, is.numeric)
    numeric_data <- stroke_data[, numeric_vars]
    ggplot(stack(numeric_data), aes(x = values)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      facet_wrap(~ ind, scales = "free") +
      labs(title = "Histograms of Numerical Variables", x = "Values", y = "Frequency")
  })
  
  output$scatter_age_bmi <- renderPlot({
    ggplot(stroke_data, aes(x = age, y = bmi)) +
      geom_point(color = "steelblue") +
      labs(title = "Scatter Plot of Age vs. BMI", x = "Age", y = "BMI")
  })
  
  output$box_bmi_gender <- renderPlot({
    ggplot(stroke_data, aes(x = gender, y = bmi)) +
      geom_boxplot(fill = "steelblue") +
      labs(title = "Box Plot of BMI by Gender", x = "Gender", y = "BMI")
  })
  

  
  # Display the data table
  output$strokeTable <- renderDataTable({
    datatable(stroke_data,
              options = list(
                pageLength = 10,
                searching = TRUE
              ),
              rownames = FALSE
    )
  })
  
  # Display the structure of the data
  output$struktur <- renderPrint({
    str(stroke_data)
  })
  
  # Display the summary of the data
  output$summary <- renderPrint({
    summary(stroke_data)
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("stroke_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(stroke_data, file, row.names = FALSE)
    }
  )
  
  # Download PDF
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("stroke_data", ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      print(stroke_data)
      dev.off()
    }
  )
  
  # Download CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("stroke_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(stroke_data, file, row.names = FALSE)
    }
  )
  
#  output$ferica <- renderImage({
#    list(src="www/ferica.jpg",height = 220, width = 150)
#  },deleteFile = F)
#  output$eli <- renderImage({
#    list(src="www/eli.jpg",height = 220, width = 150)
#  },deleteFile = F)
#  output$glorya <- renderImage({
#    list(src="www/glorya.jpg",height = 220, width = 150)
#  },deleteFile = F)
}

# Run app
shinyApp(ui, server)