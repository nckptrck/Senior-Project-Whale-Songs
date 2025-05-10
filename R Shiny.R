library(shiny)
library(tuneR)
library(signal)
library(phonTools)
library(ggplot2)
library(dplyr)
library(data.table)
library(here)

source("AWS.R")
source("Packages.R")

options(shiny.maxRequestSize = 30*1024^2)  # 30 MB max

ui <- fluidPage(
  titlePanel("Whalesong Extraction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a WAV File",
                accept = c(".wav")),
      selectInput("method", "Feature Extraction Method",
                  choices = c("melfcc")),
      checkboxInput("annotated", "Include Annotation", value = TRUE),
      actionButton("process", "Run Extraction")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$process, {
    req(input$file)
    wav_path <- input$file$datapath
    file_name <- input$file$name
    
    output$status <- renderText({
      paste("Processing", file_name, "with", input$method, "...")
    })
    
    if (input$method == "STFT") {
      result <- merging_data_stft_csv(wav_path, annotated = input$annotated)
      output$status <- renderText({ paste("STFT processing complete:", result) })
      plot_data <- read.csv(sub("\\.wav$", "_stft.csv", wav_path))
    } else if (input$method == "melfcc") {
      result <- merging_data_melfcc_csv(wav_path, numcep = 13, maxfreq = 800,
                                        wintime = 0.2, hoptime = 0.1,
                                        annotated = input$annotated)
      output$status <- renderText({ paste("melfcc processing complete:", result) })
      plot_data <- read.csv(sub("\\.wav$", "_melfcc.csv", wav_path))
    }
    
    output$plot <- renderPlot({
      if (input$method == "STFT") {
        ggplot(plot_data, aes(x = time_start, y = annotation_num, fill = song)) +
          geom_tile() +
          labs(title = "STFT Annotation Map", x = "Time (s)", y = "Annotation #")
      } else {
        ggplot(plot_data, aes(x = time_start, y = 1, fill = song)) +
          geom_tile() +
          labs(title = "melfcc Annotation Map", x = "Time (s)", y = "")
      }
    })
  })
}


shinyApp(ui = ui, server = server)

