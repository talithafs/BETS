#' BETS search 
#' 
#' An interface for searching time series with possibility 
#' to extract the data in different extensions.
#' 
#' @import  shiny miniUI rstudioapi 
#' 
#' @export


BETS.addin <- function(){
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    gadgetTitleBar("BETS Search Addin"),
    fluidPage(
      # Create a new Row in the UI for selectInputs
      
      fluidRow(
        column(4,
               textInput("description", 
                         "Description:", c("Search")
               ) 
             ),
       
         column(2,
               selectInput("periodicity",
                           "Periodicity:",
                           c("All","M","A","Q","W","D")
               )       
        ),
        column(3,
               textInput("source",
                           "Source:",
                           c("All")
               )       
        )
      ),
      # Create a new row for the table.
      fluidRow(
        DT::dataTableOutput("table")
      )
      #para exportacao dos dados
     
    ) 
  )
  
  
  server <- function(input, output, session) {
    remove(data)
    data <- BETS.search("*",view=F)
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      if(input$description != "Search" || is.null(input$description)){
        
          data <- BETS.search(description = input$description,view=F)
        }
        
        if(input$periodicity!= "All"){
          data <- BETS.search(description = input$description,view=F)
          data <- data[data$periodicity == input$periodicity,]
        }
       
        if(input$source!= "All"){
          data = BETS.search(description = input$description,view=F,src=input$source)
        }
              
      data
    },options = list(pageLength = 5, dom = 'tip')))
    
    
    
    
  }
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}



