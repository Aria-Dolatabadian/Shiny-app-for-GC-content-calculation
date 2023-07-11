library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("GC Content Calculator"),
  sidebarLayout(
    sidebarPanel(
      textInput("sequence_input", "Enter DNA Sequence:", value = "ATCG"),
      actionButton("calculate_button", "Calculate GC Content"),
      downloadButton("download_button", "Download Results")
    ),
    mainPanel(
      verbatimTextOutput("gc_output")
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$calculate_button, {
    # Get the input DNA sequence
    sequence <- input$sequence_input
    
    # Calculate the GC content
    gc_content <- calculate_gc_content(sequence)
    
    # Display the GC content
    output$gc_output <- renderPrint({
      paste("GC Content (%):", gc_content)
    })
  })
  
  # Function to calculate the GC content
  calculate_gc_content <- function(sequence) {
    c_count <- nchar(gsub("[^C]", "", sequence))
    g_count <- nchar(gsub("[^G]", "", sequence))
    total_bases <- nchar(sequence)
    gc_content <- (c_count + g_count) / total_bases * 100
    round(gc_content, digits = 2)
  }
  
  # Download the results as a text file
  output$download_button <- downloadHandler(
    filename = "gc_content_results.txt",
    content = function(file) {
      sequence <- input$sequence_input
      gc_content <- calculate_gc_content(sequence)
      write(paste("GC Content (%):", gc_content), file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
