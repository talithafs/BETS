#' BETS search 
#' 
#' An interface for searching time series with possibility 
#' to extract the data in different extensions.
#' 
#' 
#' @export


BETS.addin_pt <- function(){
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    gadgetTitleBar("BETS Search Addin"),
    fluidPage(
      # Create a new Row in the UI for selectInputs
      
      fluidRow(
        column(4,
               textInput("description", 
                         "Descrição:", c("Pesquisa")
               ) 
             ),
        
       
         column(2,
               selectInput("periodicity",
                           "Periodicidade:",
                           c("Todas","M","A","T","S","D")
                          
               )       
        ),
        column(3,
               textInput("source",
                           "Fonte:",
                           c("Todas")
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
      
        req(input$description) # tratamento para o input da descricao
        req(input$source)      # tratamento para o input da fonte
      
        
        
        nomes = c("Código","Descrição","Unidade","Periodicidade","Início","Último Valor","Fonte")
        
        data.addin <- BETS.search(description ="*",view=F,lang="pt")
        names(data.addin) = nomes
        
        
        
        if(input$description != "Pesquisa"){
          print(input$description)
          req(input$description) # tratamento para o input da descricao
          data.addin <- BETS.search(description = input$description,view=F,lang="pt")
          if(is.character(data.addin)){
            data.addin = BETS.search(description="*",view=F,lang="pt")
            
          }else{
            data.addin 
          }
         
        }else{
          data.addin <- BETS.search(description ="*",view=F,lang="pt")
        }
        
        
        if(input$periodicity!= "Todas"){
          req(input$periodicity)      # tratamento para o input da fonte
          #data.addin <- BETS.search(description = input$description,view=F)
          data.addin <- data.addin[data.addin$periodicity == input$periodicity,]
          if(is.character(data.addin)){
            data.addin = BETS.search(description="*",view=F,lang="pt")
          }else{
            data.addin 
          }
        }
        
        if(input$source!= "Todas"){
          req(input$source)      # tratamento para o input da fonte
          #data.addin <- BETS.search(description = input$description,view=F)
          data.addin <- data.addin[data.addin$source == input$source,]
          if(is.character(data.addin)){
            data.addin = BETS.search(description="*",view=F,lang="pt")
          }else{
            data.addin 
          }
        }
        
        print(data.addin)
        
        if(is.character(data.addin)){
          data.addin = BETS.search(description="*",view=F,lang="pt")
        }else{
          data.addin 
        }
        
        names(data.addin) = nomes
        data.addin  
        
        
        
    },options = list(pageLength = 5, dom = 'tip')))
    
    
  }
  viewer <- dialogViewer("BETS search", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
}



