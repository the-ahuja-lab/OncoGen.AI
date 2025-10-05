library(shiny)
library(shinyFiles)
library(shinydashboard)

ui <- fluidPage(
  tabItem(tabName = "fastqqualityreport",
          fluidRow(
            column(width = 12,
                   box(
                     title = "Fastq Quality Check", status = "primary", width = 9,
                     tags$strong("Upload Your FASTQ Files", class = "required-label"), br(),
                     shinyFilesButton('fastq_file_check', "Browse...", title='Please select a file', multiple=TRUE), 
                     uiOutput("progressBar"), br()
                   )
            )
          ))
)

### Server Part
server <- function(input, output, session) {
  # Define the file roots
  roots <- c(wd='.')
  
  # Set up shinyFiles
  shinyFileChoose(input, 'fastq_file_check', roots = roots, filetypes = c('', 'fastq', 'gz', 'fq'))
  
  # Render the progress bar UI
  output$progressBar <- renderUI({
    req(input$fastq_file_check)
    fileinfo1 <- parseFilePaths(roots, input$fastq_file_check)
    
    if(nrow(fileinfo1) >0){
      tagList(
        div(style = "height: 30px; width: 20%; background-color: #f3f3f3; border-radius: 5px; overflow: hidden; position: relative;",
            div(style = "height: 100%; width: 100%; background-color: #4caf50; position: absolute; text-align: center; line-height: 30px; color: white;",
                "completed"
            )
        )
      )
    }else{
      tagList(
        div(style = "height: 30px; width: 20%; background-color: #f3f3f3; border-radius: 5px; overflow: hidden; position: relative;",
            div(style = "height: 100%; width: 1%; background-color: #4caf50; position: absolute; text-align: center; line-height: 30px; color: white;",
                "No file selected"
            )
        )
      )
    }
    
  })
}

shinyApp(ui, server)
