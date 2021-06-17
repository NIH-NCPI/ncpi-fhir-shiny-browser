## FHIR server helper functions
#Create some reading functions to query the fhir server and extract resource responses. 

c_get=function(request) {
  my.request = paste0(input$server,request)
  res=GET(my.request,add_headers(Cookie=sprintf("AWSELBAuthSessionCookie-0=%s",input$cookie), ContentType="application/json"))
  content(res,as="parsed",type="application/json")
}

#Returns a list of resources returned by the query, extracting from Bundles and paging as necessary.
get_all=function(request) {
  next_request=request
  all_content=list()
  while(next_request!="") {
    my_content=c_get(next_request)
    if(my_content$resourceType=="Bundle") {
      all_content=append(all_content,lapply(my_content$entry,function(x){x$resource}))
      next_request=strsplit(paste0(sapply(my_content$link,function(x){ifelse(x$relation=="next",x$url,"")}),collapse = ""),
                            "?",fixed=T)[[1]][2]
      next_request=ifelse(is.na(next_request),"",paste0("?",next_request))
    }
    else {
      all_content=append(all_content,list(my_content))
      next_request=""
    }
  }
  all_content
}

#Helper to get some extra study information
getStudyPatientCounts = function(id) {
  data.frame(
    n_total=c_get(sprintf("Patient?_has:ResearchSubject:individual:study=%s&_summary=count",id))$total,
    n_female=c_get(sprintf("Patient?gender=female&_has:ResearchSubject:individual:study=%s&_summary=count",id))$total,
    n_male=c_get(sprintf("Patient?gender=male&_has:ResearchSubject:individual:study=%s&_summary=count",id))$total
  )
  
}


parse_extensions = function(resource){
  tibble(ext = resource$extension) %>% 
    unnest_wider(ext) %>% 
    unnest_longer(extension) %>% 
    hoist(extension, key=list("url")) %>% 
    hoist(extension, value=1)
}

get_race_eth = function(parsed_extensions){
  #Find race
  race = "<Not available>"
  #Check for ombCategory display names
  race.omb.values = parsed_extensions %>% filter( url=="http://hl7.org/fhir/us/core/StructureDefinition/us-core-race", 
                                                  key=="ombCategory") %>% unnest_wider(value)
  race.text =  (parsed_extensions %>% filter( url=="http://hl7.org/fhir/us/core/StructureDefinition/us-core-race", key=="text"))[["value"]]
  #If display names included, keep them.
  if(max(names(race.omb.values)=="display")==TRUE) {
    race=paste0(race.omb.values[["display"]], collapse=", ")
  } else if(length(race.text)>0) {
    race=race.text[[1]]
  }
  
  #Find ethnicity
  eth = "<Not available>"
  #Check for ombCategory display names
  eth.omb.values = parsed_extensions %>% filter( url=="http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity", 
                                                 key=="ombCategory") %>% unnest_wider(value)
  eth.text =  (parsed_extensions %>% filter( url=="http://hl7.org/fhir/us/core/StructureDefinition/us-core-ethnicity", key=="text"))[["value"]]
  #If display names included, keep them.
  if(max(names(eth.omb.values)=="display")==TRUE) {
    eth=paste0(eth.omb.values[["display"]], collapse=", ")
  } else if(length(eth.text)>0) {
    eth=eth.text[[1]]
  }
  data.frame(race=race,ethnicity=eth)
}

parse_patient = function(patient) {
  if(!is.null(patient$extension)) {
    ext=parse_extensions(patient)
    my.race_eth=get_race_eth(ext)
  } else {
    my.race_eth = data.frame(race="<Not available>",ethnicity="<Not available>")
  }
  data.frame(id=patient$id,
             gender=ifelse(is.null(patient$gender),"<Not available>",patient$gender),
             my.race_eth,
             resource=unclass(toJSON(patient,
                                     pretty = T,
                                     auto_unbox = T))
  )
}

parse_patients = function(patients){
  bind_rows(lapply(patients, parse_patient))
}
