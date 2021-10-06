# prepare_data <- function(){
#   data(fcinetica) # togliere se non necessario
#   data(fdinamica) # togliere se non necessario
#   data(all_drugs) # togliere se non necessario
#
#   sample_info_table <- generate_sample_info_table("../web_app/samples_info.tsv", "../web_app/samples_genos.tsv")
#   patient <- sample_info_table$Sample[1]
# }

#' Read and format sample info and sample genotypes
#'
#' @param patient patient id
#' @param sample_info_table table obtained merging sample_info and sample_geno
#' @param remove_last internal use only: remove the last known drug
#' @export
#'
get_patient_clinical_guidelines <- function(patient, sample_info_table, remove_last=FALSE) {

  sample <- sample_info_table[patient, ,drop=F]

  # check_if_in_tau
  # if (sample$Group_PGx==0)
  #   return(list(clinitian=sample$Clinician, short="Paziente in Tau", long="Pazienta in Tau"))

  ## Check If this is needed anymore
  first_drug <- sample$first_drug
  ## Check this ###
  # ptz_drugs <- all_drugs[all_drugs != first_drug]
  ##

  ptz_drugs <- all_drugs
  if (remove_last) {
    ptz_drugs <- all_drugs[all_drugs != first_drug]
  }

  # drug=ptz_drugs[4]
  # extract_drug_guidelines(drug, sample)

  drugs_info <- lapply(ptz_drugs, extract_drug_guidelines, sample)

  # for(drug in ptz_drugs) {
  #   print(drug)
  #   extract_drug_guidelines(drug, sample)
  # }

  names(drugs_info) <- ptz_drugs

  lights <- extract_suggestion_table(drugs_info)

  details <- extract_detailed_suggestions(drugs_info)
  priority <- sapply(details, function(x) {x$category[1]})
  details <- details[order(priority)]

  list(clinitian=sample$Clinician, tau=sample$Group_PGx, short=lights, long=details)
}
