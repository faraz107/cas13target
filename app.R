
# INSERT Libraries HERE
library(shiny)
library(shinythemes)
library(fontawesome)
library(tidyverse)
library(shinycssloaders)
library(shinyBS)
library(here)
library(Biostrings)
library(DT)

# INSERT functions HERE
source(here("findScore.R"))
source(here("compareHumanRNA.R"))

##########################
##### User interface #####
##########################
ui <- fluidPage(
  
  #Shiny theme
  theme = shinytheme("sandstone"),
  
  # Title 
  titlePanel(
    title = h1(HTML("<b>pspCas13b guide RNA design tool</b>"), style = "padding-bottom: 20px"),
    windowTitle = "Cas13b target"
  ),
  
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(width = 4,
      textAreaInput(
        inputId = "inSeq",
        label = "Input Sequence", 
        resize = "both", 
        cols = 60
      ),
      numericInput(
        inputId = "spacerLength", 
        label = "Spacer Length",
        value =  30, 
        min = 24, 
        max = 30,
        step = 1
      ),
      textInput(
        inputId = "goodSeq", 
        label = "Consensus sequence of potent crRNAs ",
        value = "GGNNNNNNNNNNNNDDDNNNNNNNNNNNNN"),
      textInput(
        inputId = "badSeq", 
        label = "Consensus sequence of ineffective crRNAs",
        value = "CCCCNNNNNNCCNNCCCHNNNNNNNNNNNN"),
      textInput(
        inputId = 'filterMotif', 
        label = "Filtered Motif",
        value = "TTTT"),
      actionButton(
        inputId = "butExample",
        label = "Example", icon = icon("circle-question", lib="font-awesome")),
      actionButton(
        inputId = "butSubmit",
        label = "Find Guides", icon = icon("searchengin", lib="font-awesome")),
      
      shinyBS::bsPopover(
        id = "inSeq",
        title = "Enter any DNA/RNA sequence to find the Cas13b targets.",
        placement = "bottom", 
        trigger = "hover"),
    
    shinyBS::bsPopover(
      id = "goodSeq",
      title = "Formula defines the characteristics of a good Cas13b target.",
      placement = "bottom",
      trigger = "hover"),
    
    shinyBS::bsPopover(
      fontawesome::fa("info-circle", a11y = "deco", title = ""),
      id = "badSeq",
      title = "Formula defines the characteristics of a poor Cas13b target.",
      placement = "bottom", 
      trigger = "hover"),
    
    shinyBS::bsPopover(
      id = "butExample",
      title = "Computes guide crRNA for an arbitrary target sequence.",
      placement = "bottom", 
      trigger = "hover"),
    
    ),
  
    mainPanel(width = 8,
      shinycssloaders::withSpinner(ui_element = DT::dataTableOutput(outputId = "dataTable"), type = 6)
    )
  ),

  headerPanel(""),
 
  tags$div(class = "body", style = "text-align: center",
    tags$p("If you are using this Cas13 guide RNA design tool in your research, please cite:"),
    tags$em("'Design principles of PspCas13b for potent and off-target-free RNA silencing' by Hu et al., 2023 (under review)."),
  ),
 
  tags$hr(),
  
  tags$div(class = "footer", checked = NA, style = "text-align: center",
           tags$a(href = "https://www.petermac.org", 
                  "Copyright © 2024 Peter MacCallum Cancer Centre")
           )
)

###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  v <- reactiveValues(data = NULL)
  
  df <- data.frame(ID = vector(mode = "character"),
                   Sequence = vector(mode = "character"),
                   Score = vector(mode = "numeric"),
                   stringsAsFactors = FALSE)
  
  v$inSeq <- ""
  v$infilterMotif <- ""
  v$BUTTON <- ""
  v$goodSeq <- "GGNNNNNNNNNNNNDDDNNNNNNNNNNNNN"
  v$badSeq <- "CCCCNNNNNNCCNNCCCHNNNNNNNNNNNN"
  
  
  # Observe Event - Submit button
  observeEvent(
    input$butSubmit,{
      v$inSeq <- input$inSeq %>% str_squish() %>% toupper() %>% str_replace_all(pattern = "U", replacement = "T")
      v$infilterMotif <- input$filterMotif
      v$goodSeq <- input$goodSeq
      v$badSeq <- input$badSeq
      v$BUTTON <- "SUBMIT"
    })
  
  # Observe Event - Show Example button
  observeEvent(
    input$butExample,{
      v$inSeq <- "CGGCTCTGTAACAAAGACCCATGTGATGCTGGGGGCAGAG" %>% toupper() %>% str_replace_all(pattern = "U", replacement = "T")
      v$infilterMotif <- input$filterMotif
      v$goodSeq <- input$goodSeq
      v$badSeq <- input$badSeq
      v$BUTTON <- "EXAMPLE"
      
      updateTextAreaInput(inputId = "inSeq",
                          value = "CGGCTCTGTAACAAAGACCCATGTGATGCTGGGGGCAGAG")
      
    })
  
  
   
  # Output DT
  output$dataTable <- DT::renderDataTable({
    
    if (is.null(v$inSeq)) return()
    
    if (is.null(v$goodSeq)) return()
    
    if (is.null(v$badSeq)) return()
    
    if (is.null(v$infilterMotif)) return()
    
    if (
      (str_length(v$inSeq) < 30 || !str_split(v$inSeq %>% str_squish(), "") %>% unlist() %>% str_detect(Biostrings::IUPAC_CODE_MAP %>% names() %>% paste0(collapse = "|")) %>% all()) 
    ) {
      shiny::validate("Enter a valid RNA sequence at least 30 nucleotides long.")
    }
    
    if (
      (str_length(v$goodSeq) != 30 || !str_split(v$goodSeq %>% str_squish(), "") %>% unlist() %>% str_detect(Biostrings::IUPAC_CODE_MAP %>% names() %>% paste0(collapse = "|")) %>% all()) 
    ) {
      shiny::validate("Enter a valid good sequence 30 nucleotides long.")
    }
    
    if (
      (str_length(v$badSeq) != 30 || !str_split(v$badSeq %>% str_squish(), "") %>% unlist() %>% str_detect(Biostrings::IUPAC_CODE_MAP %>% names() %>% paste0(collapse = "|")) %>% all()) 
    ) {
      shiny::validate("Enter a valid bad sequence 30 nucleotides long.")
    }
    
    if (
      (str_length(v$infilterMotif) < 4 || !str_split(v$infilterMotif %>% str_squish(), "") %>% unlist() %>% str_detect(Biostrings::IUPAC_CODE_MAP %>% names() %>% paste0(collapse = "|")) %>% all()) 
    ) {
      shiny::validate("Enter a valid filter motif at least 4 nucleotides long.")
    }
    
    df_seqs = data.frame(Seqs = vector(mode = "character"), Score = vector(mode = "numeric"))
    
    seqs <- v$inSeq %>% DNAString() %>% reverseComplement()
    
    for (i in seq.int(1, str_length(seqs)-29, 1)){
      df_seqs <- add_row(df_seqs,
                         Seqs = seqs %>% as.character() %>% str_sub(start = i, end = i+29))
    }
    
    LENGTH = str_length(seqs)
    
    NUM_GUIDES = n_distinct(df_seqs)
    
    df_seqs <- df_seqs %>%
      rowwise() %>%
      mutate(Score = findScore(crSeq = Seqs, goodSeq = v$goodSeq, badSeq = v$badSeq)) %>%
      distinct() %>% 
      arrange(desc(Score)) %>% 
      filter(str_detect(string = Seqs, pattern = v$infilterMotif, negate = TRUE)) %>% 
      head(n = 10)
    
    if(dim(df_seqs)[1] > 0){
      df_seqs <- df_seqs %>% 
        rowwise() %>% 
        mutate(MatchON = compareHumanRNA(crSeq = Seqs, BUTTON = v$BUTTON)[[1]],
               MatchOFF = compareHumanRNA(crSeq = Seqs, BUTTON = v$BUTTON)[[2]])
    }
    
    NUM_TARGETS = n_distinct(df_seqs)
    
    colnames(df_seqs) <- c("30-nt crRNA sequence", "Score", 
                           "On-target in human transcriptome", 
                           "Off-target in human transcriptome")
    
    DT::datatable(data = df_seqs, 
                  caption = paste0("Examined ", " 30-nt crRNA sequence(s) constructed from the ", LENGTH, "-nt input sequence. \n", 
                                   "Showing upto top 10 crRNA sequences for targeting and their on-target/off-target in the human transcriptome."),
                  escape = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = "tB",
                    buttons = c('copy', 'csv'),
                    columnDefs = list(list(className = 'dt-center', targets = c(1, 2, 3, 4))),
                    pageLength = 10
                  )) %>%
      formatStyle('30-nt crRNA sequence', fontWeight = 'bold') %>% 
      formatStyle('On-target in human transcriptome',backgroundColor = styleEqual("None", c("#cdf0cb"))) %>% 
      formatStyle('Off-target in human transcriptome',backgroundColor = styleEqual("None", c("#f89b99")))
    
  })
  
  
  
}

##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)


