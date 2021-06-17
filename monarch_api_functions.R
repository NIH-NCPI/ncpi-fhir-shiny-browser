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
