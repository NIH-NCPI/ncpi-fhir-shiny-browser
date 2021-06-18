library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)


ui <- fluidPage(


    # Application title
    titlePanel("NCPI FHIR Browser"),

    # Main page that sets up a workflow of tabs
    navbarPage("=>",
       tabPanel("Configuration",
                helpText("Load a json config file containing:",
                         "AWSELBAuthSessionCookie-0 and server",
                         "with the cookie and FHIR base URL"),
                fileInput("config.file", "Config File", accept = "application/json"),
                textInput("cookie",
                          "Cookie Auth:",
                          value = ""),
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
    ## Configuration options
    observeEvent(input$config.file, {
        req(input$config.file)
        config=read_json(input$config.file$datapath)
        updateTextInput(session, "cookie",value = config$`AWSELBAuthSessionCookie-0`)
        updateTextInput(session, "server",value = config$server)
    })
    
    ## Load helper functions
    source("support_functions.R", local=TRUE)
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
        get_all(input$participant_reference)
    })
    patientID <- reactive({
        paste0("Patient/",patientInput()[[1]]$id)
    })
    # Gather Phenotypes and create a nice table from the resources
    phenotypes <- reactive({
        get_all(sprintf("Condition/?patient=%s&_profile:below=https://ncpi-fhir.github.io/ncpi-fhir-ig/StructureDefinition/phenotype",patientID()))
    })
    phenotypeTable <- reactive({
        my.data=data.frame(name=sapply(phenotypes(),function(x){ifelse(is.null(x$code$coding[[1]]$display),x$code$text,x$code$coding[[1]]$display)}),
                   code=sapply(phenotypes(),function(x){ifelse(is.null(x$code$coding[[1]]$code),"NULL",x$code$coding[[1]]$code)}),
                   status=sapply(phenotypes(),function(x){ifelse(is.null(x$verificationStatus$text),"NULL",x$verificationStatus$text)}))
        if(nrow(my.data)==0) {
            my.data=data.frame(name="<No phenotypes>",
                               code="<No phenotypes>",
                               status="<No phenotypes>")
        }
        my.data
    })
    #Gather Conditions and create a nice table from the resources
    diseases <- reactive({
        get_all(sprintf("Condition/?patient=%s&_profile:below=https://ncpi-fhir.github.io/ncpi-fhir-ig/StructureDefinition/disease",patientID()))
    })
    diseaseTable <- reactive({
        my.data=data.frame(name=sapply(diseases(),function(x){ifelse(is.null(x$code$coding[[1]]$display),x$code$text,x$code$coding[[1]]$display)}),
                   code=sapply(diseases(),function(x){ifelse(is.null(x$code$coding[[1]]$code),"NULL",x$code$coding[[1]]$code)}),
                   status=sapply(diseases(),function(x){ifelse(is.null(x$verificationStatus$code$coding[[1]]$code),"NULL",x$verificationStatus$code$coding[[1]]$code)}))
        if(nrow(my.data)==0) {
            my.data=data.frame(name="<No diseases>",
                               code="<No diseases>",
                               status="<No diseases>")
        }
        my.data
        
    })
    
    #Request info from Monarch API on button press
    simMatches <- reactive({
        sim_search((filter(phenotypeTable(),status=="Positive"))$code)
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
        get_all("ResearchStudy")
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
                  options=list(columnDefs = list(list(visible=FALSE, targets=5))))
    )
    
    ###
    #Block for research study detail tab
    ###
    selected_study_id <- reactive({
        req(input$study_table_rows_selected)
        studyTable()[input$study_table_rows_selected,"id"]
    })
    
    #Get participants for that ID
    study_participants <- reactive({
        req(input$server)
        req(input$study_table_rows_selected)
        
        get_all(sprintf("Patient?_has:ResearchSubject:individual:study=%s",selected_study_id()))
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
        get_all(sprintf("DocumentReference/?patient=%s&_profile:below=https://ncpi-fhir.github.io/ncpi-fhir-ig/StructureDefinition/ncpi-drs-document-reference",patientID()))
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
shinyApp(ui = ui, server = server)
