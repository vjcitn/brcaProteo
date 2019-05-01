#' define a shiny app to check consistency of iTRAQ, RNA-seq and RPPA
#' in BRCA
#' @import shiny
#' @export
pgconsis = function() {
 mae = brcaProteo::brint
 feats = rownames(mae)[[1]]
 ui = fluidPage(
  sidebarPanel(
   helpText("pgconsis: assess consistency of proteogeomics data in breast cancer"),
   selectInput("gene", "gene", choices=sort(feats), selected="BCL2"),
   width=3
  ),
 mainPanel(
  tabsetPanel(
   tabPanel("main",
    plotOutput("pairs")
    ),
   tabPanel("about",
    helpText("Derived from Mertens et al. 2016 PMID 27251275"),
    verbatimTextOutput("maeprint"),
    verbatimTextOutput("sess")
    )
   )
  )
 )
server = function(input, output) {
 output$pairs = renderPlot({
   mt = sapply(experiments(brint[input$gene,]), assay)
   pairs(mt)
   })
 output$maeprint = renderPrint({
   brint
   })
 output$sess = renderPrint({ sessionInfo() })
 }
runApp(list(ui=ui, server=server))
}

 
