## Functions ##

#' Create the samples infos to create the Report
#'
#' @param file_info Get patients' file info form web_app generated through Prepare_data_Mac_v2.py
#' @param file_genos Get patients' genotype info file form web_app generated through Prepare_data_Mac_v2.py
#' @importFrom kableExtra kable
#' @importFrom rmarkdown render
#' @export
#'
create_reports <- function(file_info, file_genos) {

  require(kableExtra)
  require(rmarkdown)
  check_local_files()
  sample_info_table <- generate_sample_info_table(file_info, file_genos)
  patients <- sample_info_table$Sample

  # if (!file.exists("all_patients_report.RData")){
  #   all_patients_report <- lapply(patients, get_patient_clinical_guidelines, sample_info_table)
  #   names(all_patients_report) <- patients
  #   save(all_patients_report, file="all_patients_report.RData")
  # } else {
  #   load("all_patients_report.RData")
  # }
  all_patients_report <- lapply(patients, get_patient_clinical_guidelines, sample_info_table)
  names(all_patients_report) <- patients

  unformatted_reports <- all_patients_report

  for (patient in names(unformatted_reports)) {
    tbl <- unformatted_reports[[patient]]$short
    infos <- all_patients_report[[patient]]$long
    paziente <- patient

    output_dir = paste0("Report_PDF/", unformatted_reports[[patient]]$clinitian,
                        "/", unformatted_reports[[patient]]$clinitian)
    if (unformatted_reports[[patient]]$tau==0){
      output_dir = paste0(output_dir, "_TAU")
    } else {
      output_dir = paste0(output_dir, "_TGTG")
    }

    if (!unformatted_reports[[patient]]$tau %in% c(0,1))
      stop(paste("Unrecognize TAU value for patient",
                 paziente, "found", unformatted_reports[[patient]]$tau,
                 ". Expected 0 or 1"))

    if (!file.exists(paste0(output_dir, "/", paste0(patient, ".pdf")))){
      rmarkdown::render(input = "template.Rmd",
                        output_format = "pdf_document",
                        output_file = paste0(patient, ".pdf"),
                        output_dir = output_dir,
                        envir = new.env())
      detach("package:kableExtra", unload=TRUE)
    }
  }
}

#' Create the samples infos to create the Report (internal use only)
#'
#' @param file_info Get patients' file info form web_app generated through Prepare_data_Mac_v2.py
#' @param file_genos Get patients' genotype info file form web_app generated through Prepare_data_Mac_v2.py
#' @importFrom kableExtra kable
#' @importFrom rmarkdown render
#' @export
#'
create_internal_reports <- function(file_info, file_genos) {

  require(kableExtra)
  require(rmarkdown)
  # file_info <- "../web_app/samples_info.tsv"
  # file_genos <- "../web_app/samples_genos.tsv"
  check_local_files()
  suppressPackageStartupMessages(library(PharmGenBS))
  sample_info_table <- generate_sample_info_table(file_info, file_genos)
  patients <- sample_info_table$Sample

  all_patients_report <- lapply(patients, get_patient_clinical_guidelines, sample_info_table, remove_last=T)
  names(all_patients_report) <- patients
  unformatted_reports <- all_patients_report

  for (patient in names(unformatted_reports)) {
    tbl <- unformatted_reports[[patient]]$short
    infos <- all_patients_report[[patient]]$long
    paziente <- patient

    output_dir = paste0("Last_Drug_Report_PDF/", unformatted_reports[[patient]]$clinitian,
                        "/", unformatted_reports[[patient]]$clinitian)

    if (!unformatted_reports[[patient]]$tau %in% c(0,1))
      stop(paste("Unrecognize TAU value for patient",
                 paziente, "found", unformatted_reports[[patient]]$tau,
                 ". Expected 0 or 1"))

    if (unformatted_reports[[patient]]$tau==0){
      output_dir = paste0(output_dir, "_TAU")
    } else {
      output_dir = paste0(output_dir, "_TGTG")
    }

    if (!file.exists(paste0(output_dir, "/", paste0(patient, ".pdf")))){
      rmarkdown::render(input = "template.Rmd",
                        output_format = "pdf_document",
                        output_file = paste0(patient, ".pdf"),
                        output_dir = output_dir,
                        envir = new.env())
      detach("package:kableExtra", unload=TRUE)
    }
  }
}

check_local_files <- function() {
  if (!file.exists("template.Rmd")) {
    stop(paste0("Error: file 'template.Rmd' not found in '", getwd(),
                "'. Please download a copy 'from https://github.com/Martini-CompBio-DMMT/PharmGenBS_report_builder' and move it to the working directory"))
  }

  paths = c("images/rosso_small.png", "images/giallo_small.png",
    "images/verde_small.png", "images/white.png")
  for (name in paths) {
    if (!file.exists(paths[name])) {
      stop(paste0("Error: file ", name, " not found in '", getwd(),
                  "'. Please download dircetory images from 'from https://github.com/Martini-CompBio-DMMT/PharmGenBS_report_builder' and move it to the working directory"))
    }
  }
}
