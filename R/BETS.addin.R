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
        column(1,
          checkboxInput("lang1","EN",TRUE),
          checkboxInput("lang2","PT",FALSE)
        ),
       
         column(2,
               selectInput("periodicity",
                           "Periodicity:",
                           list(`Periodicity`=c("All","M","A","Q","W","D"),
                                `Periodicidade`=c("All","M","A","D","T","S")
                           )
                          
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
    
    # Filter data based on selections
   
   
     output$table <- DT::renderDataTable(DT::datatable({
      data.addin <- BETS.search(description ="*",view=F)
      
      req(input$description) # tratamento para o input da descricao
      req(input$source)      # tratamento para o input da fonte
      
      
      if(input$lang1){
      
      
      if(input$description != "Search" || is.null(input$description)){
        
          data.addin <- BETS.search(description = input$description,view=F)
          
          if(input$periodicity!= "All"){
            data.addin <- BETS.search(description = input$description,view=F)
            data.addin <- data[data$periodicity == input$periodicity,]
          }
          
          if(input$source!= "All"){
            data.addin <- BETS.search(description = input$description,view=F)
            data.addin <- data[data$source == input$source,]
          }
          
          
      }
       data.addin = BETS.search("*",view=F)
         
        if(input$periodicity!= "All"){
          data.addin <- BETS.search(description = input$description,view=F)
          data.addin <- data[data$periodicity == input$periodicity,]
        }
       
        if(input$source!= "All"){
          data.addin <- BETS.search(description = input$description,view=F)
          data.addin <- data[data$source == input$source,]
        }
              
      data.addin
      
      }else if(input$lang2){
      
        
        if(input$description != "Search" || is.null(input$description)){
          
          data.addin <- BETS.search(description = input$description,view=F,lang="pt")
          
          if(input$periodicity!= "All"){
            data.addin <- BETS.search(description = input$description,view=F,lang="pt")
            data.addin <- data[data$periodicity == input$periodicity,]
          }
          
          if(input$source!= "All"){
            data.addin <- BETS.search(description = input$description,view=F,lang="pt")
            data.addin <- data[data$source == input$source,]
          }
          
          
        }
        data.addin = BETS.search("*",view=F,lang="pt")
        
        if(input$periodicity!= "All"){
          data.addin <- BETS.search(description = input$description,view=F,lang="pt")
          data.addin <- data[data$periodicity == input$periodicity,]
        }
        
        if(input$source!= "All"){
          data.addin <- BETS.search(description = input$description,view=F,lang="pt")
          data.addin <- data[data$source == input$source,]
        }
        
        data.addin
        
        
          
      }
    },options = list(pageLength = 6, dom = 'tip')))
    
    
  }
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}



