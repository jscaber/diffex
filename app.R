# Check for availability of required packages, install if not available
list.of.packages <- c("shiny", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#initialize
library(shiny)
library(ggplot2)


# UI for app

ui<-(pageWithSidebar(
  # title
  headerPanel("Select Options"),
  
  #input
  sidebarPanel
  (
    # Input: Select a file ----
    selectInput("experiment", 
                label = "Choose an experiment",
                choices = c("ips_c9_sorted", 
                            "ips_c9_mixed",
                            "mouse_m337v", 
                            "ips_tdp",
                            "ips_sma23",
                            "ips_sma1"),
                selected = "ips_c9_mixed"),
    selectInput("gene", 
                label = "Choose a gene",
                choices = NULL),
    selectInput("contrast", 
                label = "Choose a contrast",
                choices = NULL),
  
    # Horizontal line ----
    tags$hr(),
    
  ),
  
  # output
  mainPanel(
    #h3(htmlOutput("caption")),
    uiOutput("plot"), # depends on input
    h3(textOutput("caption"))
    
  )
))


# shiny server side code for each call
server<-(function(input, output, session){
  

  
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  

  
  #plotting function using ggplot2
  output$p <- renderPlot({
    
    results <- results[!is.na(results$symbol),]
    ensemblgene <- results[results$symbol == input$gene, 1]
    
    dftemp <- as.data.frame(t(tpm[which(tpm[,1]==ensemblgene),-1]), rownames = "X")
    colnames(dftemp) <-"value"
    rownames(design) <- design$track
    dftemp$group <- design[rownames(dftemp),][,input$contrast]

    p<-    ggplot(dftemp, aes(x = group, y = value, color = group)) +
      geom_boxplot(outlier.shape = NA) + theme_bw() +
      geom_point(aes(color=group), alpha = 0.5, position = position_jitter(w = 0.2, h = 0))  +
      labs(color = input$contrast) + ggtitle(input$gene) +
      ylab("TPM") + xlab (input$contrast) 

    
    print(p)
  })
  
  output$caption<-renderText({
    results <- results[!is.na(results$symbol),]
    padj <- signif(results[results$symbol == input$gene, "padj"],digits = 3)
    if (is.na(padj)){
      paste0(input$gene," was not tested in the model (abundance likely too low) (p =", padj,")")
    } else if (padj <.05){
      paste0(input$gene," is differentially expressed (p =", padj,")")
    } else {
      paste0(input$gene," is not differentially expressed (p =", padj,")")
    }
  })
  
  # set uploaded file
  upload_design<- reactive({
    design <- paste0("./data/",input$experiment, "/design.tsv")
    if (is.null(design))
      return(NULL)
    #could also store in a reactiveValues
    read.csv(design,sep = "\t",header = TRUE)
  })
  upload_tpm<- reactive({
    tpm <- paste0("./data/",input$experiment, "/tpm.tsv")
    if (is.null(tpm))
      return(NULL)
    #could also store in a reactiveValues
    read.csv(tpm,sep = "\t",header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  })
  upload_results<- reactive({
    results <- paste0("./data/",input$experiment, "/results.tsv")
    if (is.null(results))
      return(NULL)
    #could also store in a reactiveValues
    read.csv(results,sep = "\t",header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  })
  


  observeEvent(input$experiment,{
    design<<-upload_design()
    updateSelectInput(session, "contrast", choices = colnames(design))
    })
  observeEvent(input$experiment,{
    results<<-upload_results()
    updateSelectInput(session, "gene", choices = results$symbol)
  })
  observeEvent(input$experiment,{
    tpm<<-upload_tpm()
  })

  
  
})

# Create Shiny app ----
shinyApp(ui, server)
