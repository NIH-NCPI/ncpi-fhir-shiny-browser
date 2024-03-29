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


# Define UI for application that draws a histogram
ui <- fluidPage(


    # Application title
    titlePanel("NCPI FHIR and Monarch API Example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("Participant",
                    textInput("participant_reference",
                              "Participant Reference:",
                              value = "Patient/5aa93e5f-4603-4e28-901f-5d551a0ef194"),
                    helpText("Particiant Information:"),
                    textOutput("patientID"),
                    textOutput("patientGender")
                ),
                tabPanel("Configuration",
                     helpText("FHIR base URL"),
                     textInput("server",
                               "Server Loc:",
                               value = "https://healthcare.googleapis.com/v1/projects/ncpi-test-fhir/locations/us-central1/datasets/fhir-test/fhirStores/inferno2/fhir/")
                )
            )
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

server <- function(input, output, session) {
    # create a non-reactive access_token as we should never get past this if not authenticated
    gar_shiny_auth(session)
    
    ## FHIR server helper functions
    #Create some reading functions to query the fhir server and extract resource responses. get_all() returns a list of resources returned by the query, extracting from Bundles and paging as necessary.
    
    gapi_get=function(request) {
        res=GET(paste0(input$server,request),add_headers(Authorization=sprintf("Bearer %s",gar_token()$auth_token$credentials$access_token)))
        con=content(res,as="parsed",type="application/json")
        #If it fails unauthorized, refresh token and try again
        if(!is.null(con$error) && con$error$code==401) {
            gar_token()$auth_token$refresh()
            res=GET(paste0(input$server,request),add_headers(Authorization=sprintf("Bearer %s",gar_token()$auth_token$credentials$access_token)))
            con=content(res,as="parsed",type="application/json")
        }
        con
    }
    get_all=function(request) {
        next_request=request
        all_content=list()
        while(next_request!="") {
            my_content=gapi_get(next_request)
            if(my_content$resourceType=="Bundle") {
                all_content=append(all_content,lapply(my_content$entry,function(x){x$resource}))
                next_request=substring(paste0(sapply(my_content$link,function(x){ifelse(x$relation=="next",x$url,"")}),collapse = ""),22)
            }
            else {
                all_content=append(all_content,list(my_content))
                next_request=""
            }
        }
        all_content
    }
    
    ## Monarch API helper functions
    #These use the sim search endpoints documented here https://api.monarchinitiative.org/api to see what comes back. Does not currently support negated HPO ids.

    sim_search=function(hpo_ids) {
        searchURL="https://api.monarchinitiative.org/api/sim/search?is_feature_set=true&metric=phenodigm&limit=100&taxon=9606&id="
        request=paste0(hpo_ids,collapse = "&id=")
        res=GET(paste0(searchURL,request))
        content(res,as="parsed",type="application/json")
    }
    list_sim_results=function(sim_results) {
        data.frame(name=sapply(sim_results$matches,function(x){x$label}),
                   code=sapply(sim_results$matches,function(x){x$id}),
                   score=sapply(sim_results$matches,function(x){x$score}),
                   n_phenotypes=sapply(sim_results$matches,function(x){length(x$pairwise_match)}))
    }
    
    ## Create some reactive expressions for each step so the plots don't cause too many calls
    # Patient
    patientInput <- reactive({
        req(input$server)
        get_all(input$participant_reference)
    })
    patientID <- reactive({
        paste0("Patient/",patientInput()[[1]]$id)
    })
    # Gather Observations and create a nice table from the resources
    observations <- reactive({
        get_all(sprintf("Observation/?patient=%s",patientID()))
    })
    observationTable <- reactive({
        data.frame(name=sapply(observations(),function(x){ifelse(is.null(x$code$coding[[1]]$display),x$code$text,x$code$coding[[1]]$display)}),
                       code=sapply(observations(),function(x){x$code$coding[[1]]$code}),
                       status=sapply(observations(),function(x){x$interpretation[[1]]$coding[[1]]$code}))
    })
    #Gather Conditions and create a nice table from the resources
    conditions <- reactive({
        get_all(sprintf("Condition/?patient=%s",patientID()))
    })
    conditionTable <- reactive({
        data.frame(name=sapply(conditions(),function(x){ifelse(is.null(x$code$coding[[1]]$display),x$code$text,x$code$coding[[1]]$display)}),
                   code=sapply(conditions(),function(x){ifelse(is.null(x$code$coding[[1]]$code),"NULL",x$code$coding[[1]]$code)}),
                   status=sapply(conditions(),function(x){ifelse(is.null(x$verificationStatus$text),"NULL",x$verificationStatus$text)}))
    })
    
    #Request info from Monarch API on button press
    simMatches <- reactive({
        sim_search((filter(observationTable(),status=="POS"))$code)
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
        datatable(observationTable()%>% arrange(desc(status)))
        )
    
    # Provide the list of Conditions for reference
    output$listed_conditions <- renderDT( 
        datatable(conditionTable()%>% arrange(desc(status)))
    )
    
    # Provide the list of potential matches
    output$found_diseases <- renderDT( 
        datatable(simMatchTable())
    )
    
}

# Run the application 
shinyApp(ui = gar_shiny_ui(ui), server = server, options = list(port=1221))
