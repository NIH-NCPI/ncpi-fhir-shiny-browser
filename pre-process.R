
input = list(
  cookie = "",
  server= "https://kf-api-fhir-service.kidsfirstdrc.org/")

# Get the list of studies
studies <- get_all("ResearchStudy")

# Summarize in a table
get_study_details = function(studies) {
  bind_rows(lapply(studies, function(x){
    data.frame(id=x$id,
               title=x$title,
               getStudyPatientCounts(x$id),
               resource=unclass(toJSON(x,
                                       pretty = T, auto_unbox = T))
    )
  }))
}

study_details = get_study_details(studies)

library(doParallel)
registerDoParallel(cores=4)


patient_resources <- foreach(study = iterators::iter(study_details, by = "row"), .combine = append) %dopar% {

  get_all(sprintf("Patient?_has:ResearchSubject:individual:study=%s",study$id))
}

test_patient = patient_resources[1:10]


parse_patient_only = function(patient) {
  if(!is.null(patient$extension)) {
    ext=parse_extensions(patient)
    my.race_eth=get_race_eth(ext)
  } else {
    my.race_eth = data.frame(race="<Not available>",ethnicity="<Not available>")
  }
  data.frame(id=patient$id,
             gender=ifelse(is.null(patient$gender),"<Not available>",patient$gender),
             my.race_eth)
}

participants <- foreach(patient = iterators::iter(test_patient), .combine = bind_rows, .packages = c("tidyverse", "jsonlite")) %dopar% {
  parse_patient_only(patient)
}


##TODO: keep going

#Get participants for that ID
study_participants <- reactive({
  req(input$server)
  req(input$study_table_rows_selected)
  
 
})

# Summarize in a table
studyParticipantTable <- reactive({
  parse_patients(study_participants())
})
