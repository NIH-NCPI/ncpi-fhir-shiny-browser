library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)
library(googleAuthR)

# Configure the OAuth request
# GCP API scope
scopes="https://www.googleapis.com/auth/cloud-platform"
# This is a test web application OAuth registration, requiring special access 
# Setup here: https://console.developers.google.com/apis/credentials
gar_set_client(web_json = "web_secret.json",
               scopes = scopes,
               activate="web")


ui <- fluidPage(
  
  
  # Application title
  titlePanel("NCPI FHIR Browser"),
  
  # Main page that sets up a workflow of tabs
  navbarPage("=>",
             tabPanel("Configuration",
                      helpText("Specify the Google FHIR service base URL"),
                      textInput("server",
                                "Server Loc:",
                                value = "")
                      
                      
             ),
             tabPanel("ResearchStudy Browser",
                      fluidPage(
                        fluidRow(
                          column(4,
                                 helpText("Select a study:"),
                                 DTOutput("study_table")
                          ),
                          column(8,
                                 navbarPage("Study Details:",
                                            tabPanel("Parsed view", fluidPage(
                                              fluidRow(textOutput("study_detail_header")),
                                              fluidRow(column(6,plotOutput("study_detail_race")),
                                                       column(6,plotOutput("study_detail_eth"))),
                                              fluidRow(column(4,tableOutput("study_detail_race_eth_tab")),
                                                       column(2,tableOutput("study_detail_gender_tab")),
                                                       column(6,plotOutput("study_detail_gender")))
                                            )
                                            ),
                                            tabPanel("JSON view",
                                                     verbatimTextOutput("study_json")
                                            )
                                 )
                          )
                        )
                        
                      )
             ),
             tabPanel("Participant Browser",fluidPage(
               fluidRow(
                 column(4,
                        textOutput("study_detail_header_part"),
                        helpText("List of participants in this study:"),
                        DTOutput("study_participant_table")
                 ),
                 column(8,
                        navbarPage("Participant Details:",
                                   tabPanel("Parsed view",
                                            tableOutput("participant_details"),
                                            helpText("Disease Summary"),
                                            tableOutput("participant_disease_summary"),
                                            helpText("Phenotype Summary"),
                                            tableOutput("participant_phenotype_summary")
                                   ),
                                   tabPanel("JSON view",
                                            verbatimTextOutput("participant_json")
                                   )
                        )
                 )
               )
             )
             ),
             tabPanel("DRS Browser",fluidPage(
               fluidRow(
                 column(4,
                        helpText("Particiant Information:"),
                        tableOutput("participant_details_drs"),
                        helpText("List of files for this participant:"),
                        DTOutput("drs_table")
                        
                 ),
                 column(8,
                        navbarPage("File Details:",
                                   tabPanel("JSON Resource view",
                                            verbatimTextOutput("drs_json")
                                   ),
                                   tabPanel("DRS JSON view",
                                            verbatimTextOutput("drs_resolved_json")
                                            
                                   )
                        )
                 )
               )
             )
             ),
             tabPanel("Monarch API query",
                      sidebarLayout(
                        #Controls re a participant
                        sidebarPanel(
                          textInput("participant_reference",
                                    "Participant Reference:",
                                    value = ""),
                          helpText("Particiant Information:"),
                          tableOutput("participant_details_monarch")
                          
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          h2("Monarch API Diseases"),
                          DTOutput("found_diseases"),
                          h2("Source HPO Terms"),
                          DTOutput("hpo_terms"),
                          h2("Source Conditions"),
                          DTOutput("listed_conditions")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
    # create a non-reactive access_token as we should never get past this if not authenticated
    gar_shiny_auth(session)
    
    ## FHIR server helper functions
    #Create some reading functions to query the fhir server and extract resource responses. g_g_get_all() returns a list of resources returned by the query, extracting from Bundles and paging as necessary.
    # Moved these to support_functions with google tag
  
    ## Load helper functions
    source("g_support_functions.R", local=TRUE)
    source("monarch_api_functions.R", local=TRUE)
  ## Create some reactive expressions for each step so the plots don't cause too many calls
  
  # Set the patient when a row is clicked
  observeEvent(input$study_participant_table_rows_selected, {
    updateTextInput(session, "participant_reference", 
                    value = paste0("Patient/",studyParticipantTable()[input$study_participant_table_rows_selected,"id"]))
  })
  
  # Patient
  patientInput <- reactive({
    req(input$server)
    req(input$participant_reference)
    g_get_all(input$participant_reference)
  })
  patientID <- reactive({
    paste0("Patient/",patientInput()[[1]]$id)
  })
  #Gather all conditions
  conditions <- reactive({
    g_get_all(sprintf("Condition/?patient=%s",patientID()))
  })
  
  conditionTable <- reactive({
    if(length(conditions())>0){
      my.data=data.frame(name=sapply(conditions(),function(x){ifelse(is.null(x$code$coding[[1]]$display),x$code$text,x$code$coding[[1]]$display)}),
                         code=sapply(conditions(),function(x){ifelse(is.null(x$code$coding[[1]]$code),"NULL",x$code$coding[[1]]$code)}),
                         status=sapply(conditions(),function(x){ifelse(is.null(x$verificationStatus$coding[[1]]$display),"NULL",x$verificationStatus$coding[[1]]$display)}),
                         is_hpo=sapply(conditions(),function(x){ifelse(!is.null(x$code$coding[[1]]$code)&&(x$code$coding[[1]]$system=="http://purl.obolibrary.org/obo/hp.owl"),TRUE,FALSE)}))
    } else {
      my.data = data.frame(name=NA, code=NA, status=NA, is_hpo=NA)
    }
    my.data
  })
  
  # Gather Phenotypes and create a nice table from the resources
  phenotypeTable <- reactive({
    my.data=filter(conditionTable(), is_hpo)
    if(nrow(my.data)==0) {
      my.data=data.frame(name="<No phenotypes>",
                         code="<No phenotypes>",
                         status="<No phenotypes>")
    }
    select(my.data, name, code, status)
  })
  #Gather Conditions and create a nice table from the resources
  diseases <- reactive({
    g_get_all(sprintf("Condition/?patient=%s&code:not-in=ValueSet/e0f600d5-7662-4094-96be-b11859ad5a59",patientID()))
  })
  diseaseTable <- reactive({
    my.data=filter(conditionTable(), !is_hpo)
    if(nrow(my.data)==0) {
      my.data=data.frame(name="<No diseases>",
                         code="<No diseases>",
                         status="<No diseases>")
    }
    select(my.data, name, code, status)
    
  })
  
  #Request info from Monarch API on button press
  simMatches <- reactive({
    sim_search((filter(phenotypeTable(),status=="Confirmed"))$code)
  })
  #Create table from API return
  simMatchTable <- reactive({
    list_sim_results(simMatches())
  })
  
  
  # Add the patient ID to the top of the output for clarity
  output$patientID <- renderText({
    patientID()
  })
  # Add the patient gender
  output$patientGender <- renderText({
    patientInput()[[1]]$gender
  })
  
  # Provide the table of HPO terms for reference
  output$hpo_terms <- renderDT( 
    datatable(phenotypeTable() %>% arrange(desc(status)))
  )
  
  # Provide the list of Conditions for reference
  output$listed_conditions <- renderDT( 
    datatable(diseaseTable() %>% arrange(desc(status)))
  )
  
  # Provide the list of potential matches
  output$found_diseases <- renderDT( 
    datatable(simMatchTable())
  )
  
  ###
  #Block for ResearchStudy Tab
  ###
  
  # Get the list of studies
  studies <- reactive({
    req(input$server)
    g_get_all("ResearchStudy")
  })
  
  # Summarize in a table
  studyTable <- reactive({
    bind_rows(lapply(studies(), function(x){
      data.frame(id=x$id,
                 title=x$title,
                 getStudyPatientCounts(x$id),
                 resource=unclass(toJSON(x,
                                         pretty = T, auto_unbox = T))
      )
    })) %>% 
      arrange(title)
  })
  
  # Create output
  output$study_table <- renderDT( 
    datatable(studyTable(), selection = "single", rownames=F,
              options=list(columnDefs = list(list(visible=FALSE, targets=3))))
  )
  
  ###
  #Block for research study detail tab
  ###
  selected_study_id <- reactive({
    req(input$study_table_rows_selected)
    studyTable()[input$study_table_rows_selected,"id"]
  })
  
  #Get ResearchSubjects for that ID
  study_research_subjects <- reactive({
    req(input$server)
    req(input$study_table_rows_selected)
    
    g_get_all(sprintf("ResearchSubject?study=%s",selected_study_id()))
    
  })
  #Get participants for that ID
  study_participants <- reactive({
    lapply(study_research_subjects(), function(x){
        gapi_get(x$individual$reference)
    })

    
  })
  
  
  # Summarize in a table
  studyParticipantTable <- reactive({
    parse_patients(study_participants())
  })
  
  # Create output
  output$study_participant_table <- renderDT( 
    datatable(studyParticipantTable(),selection = "single", rownames=F,
              options=list(columnDefs = list(list(visible=FALSE, targets=4))))
  )
  
  ##Study Summary tabs
  # Create the study summary
  output$study_detail_header_part <- output$study_detail_header <- renderText({
    sprintf("Study ID: %s\tStudy Title: %s",
            studyTable()[input$study_table_rows_selected,"id"],
            studyTable()[input$study_table_rows_selected,"title"])
  })
  
  output$study_detail_gender <- renderPlot({
    ggplot(studyParticipantTable(), aes(x="",fill=gender)) +
      geom_bar(stat="count", width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      ggtitle("Gender")
  })
  
  output$study_detail_race <- renderPlot({
    ggplot(studyParticipantTable(), aes(x=race,fill=race)) +
      geom_bar(stat="count") +
      ggtitle("Race") +
      theme_light() +
      theme(legend.position="bottom")
  })
  
  output$study_detail_eth <- renderPlot({
    ggplot(studyParticipantTable(),aes(x=ethnicity,fill=ethnicity)) +
      geom_bar(stat="count") +
      theme_light() +
      ggtitle("Ethnicity") +
      theme(legend.position="bottom")
  })
  
  output$study_detail_gender_tab <- renderTable({
    studyParticipantTable() %>% group_by(gender) %>% 
      summarize(n=n(), .groups = "drop")
  })
  
  output$study_detail_race_eth_tab <- renderTable({
    studyParticipantTable() %>% group_by(race, ethnicity) %>% 
      summarize(n=n(), .groups = "keep")
  })
  
  
  # Output study JSON
  output$study_json <- renderText({
    studyTable()[input$study_table_rows_selected,"resource"]
  })
  
  ##Participant Summary tabs
  
  # Create the participant summary
  
  output$participant_details_drs <- output$participant_details_monarch <- output$participant_details <- renderTable({
    req(input$study_participant_table_rows_selected)
    studyParticipantTable()[input$study_participant_table_rows_selected,] %>% 
      transmute(id=id, Gender=gender, Race=race, Ethnicity=ethnicity)
  })
  
  output$participant_phenotype_summary <- renderTable({
    
    phenotypeTable() %>% group_by(status) %>% 
      summarize(n=n())
  })
  output$participant_disease_summary <- renderTable({
    diseaseTable() %>% group_by(status) %>% 
      summarize(n=n())
  })
  
  
  # Output study JSON
  output$participant_json <- renderText({
    studyParticipantTable()[input$study_participant_table_rows_selected,"resource"]
  })
  
  ###
  # DocumentReferences
  ###
  
  #
  drsDocuments <- reactive({
    g_get_all(sprintf("DocumentReference/?patient=%s",patientID()))
  })
  drsDocumentTable <- reactive({
    my.data=data.frame(id=sapply(drsDocuments(),function(x){x$id}),
                       format=sapply(drsDocuments(),function(x){ifelse(is.null(x$content[[1]]$format$display),"NULL",x$content[[1]]$format$display)}),
                       reference=sapply(drsDocuments(),function(x){ifelse(is.null(x$content[[1]]$attachment$url),"NULL",x$content[[1]]$attachment$url)}),
                       resource=sapply(drsDocuments(),toJSON,pretty = T,auto_unbox = T))
    if(nrow(my.data)==0) {
      my.data=data.frame(id="<No files>",
                         format="<No files>",
                         reference="<No files>",
                         resource="<No files>")
    }
    my.data
    
  })
  
  output$drs_table <- renderDT({
    datatable(drsDocumentTable(),selection = "single", rownames=F,
              options=list(columnDefs = list(list(visible=FALSE, targets=3))))
  })
  
  output$drs_json <- renderText({
    drsDocumentTable()[input$drs_table_rows_selected,"resource"]
  })
  
  output$drs_resolved_json <- renderText({
    parts=strsplit(drsDocumentTable()[input$drs_table_rows_selected,"reference"],split = "/")[[1]]
    drs_https_url=sprintf("https://%s/ga4gh/drs/v1/objects/%s",parts[3],parts[4])
    
    drs_response=GET(drs_https_url)
    toJSON(content(drs_response,as="parsed",type="application/json"),auto_unbox = T, pretty = T)
  })
  
}

# Run the application 
shinyApp(ui = gar_shiny_ui(ui), server = server, options = list(port=1221))
