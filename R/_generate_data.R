#' #' Create the data
#' #'
#' #' Create output data
#' #'
#' #' @param dir dirctory
#' #' @importFrom utils read.table
#' #'
#' .generate_data <- function(dir) {
#'
#'   setwd(dir)
#'   # Read DB infos
#'   all_drugs <- scan("AD_ITA.list", what="", sep="\n")
#'   guidelines_CYP <- read.table("GuideLines_table_CYP.csv", header=T, sep="\t", quote="", as.is=T)
#'   guidelines_SNP <- read.table("GuideLines_table_SNPs.csv", header=T, sep="\t", quote="", as.is=T)
#'
#'   # Read sample infos
#'   samples_info <- read.table("samples_info.tsv", header=T, as.is=T)
#'
#'   # Read Sample genotypes/phenotypes
#'   samples_genos <- read.table("samples_genos.tsv", header=T, as.is=T)
#'   samples_genos <- unique(samples_genos)
#'   if (any(duplicated(samples_genos$Sample)))
#'     stop("Sample in genotype is duplicated")
#'   row.names(samples_genos) <- samples_genos$Sample
#'
#'
#'   clinicians <- unique(samples_info$Clinician)
#'   # patients <- unique(samples_info$Sample[samples_info$Clinician==clinicians[1] & samples_info$Group_PGx==1])
#'   # tau <- sort(unique(samples_info$Sample[samples_info$Clinician == clinicians[1] & samples_info$Group_PGx == 0])
#'
#'   # extract info for a clinitian
#'   cli_patients <- lapply(clinicians, extract_samples_info, samples_info=samples_info)
#'   names(cli_patients) <- clinicians
#'
#'   data <- lapply(cli_patients, build_samples_report, all_drugs=all_drugs, samples_genos=samples_genos,
#'          guidelines_CYP=guidelines_CYP, guidelines_SNP=guidelines_SNP)
#'
#'   data
#' }
#'
#' #' Extract Patients for each clinician
#' #'
#' #'
#' .extract_samples_info <- function(clinician, samples_info) {
#'   # clinician = clinicians[1]
#'   grp_PGx <- samples_info[samples_info$Clinician==clinician & samples_info$Group_PGx==1, , drop=F]
#'   grp_PGx_tau <- samples_info[samples_info$Clinician==clinician & samples_info$Group_PGx==0, , drop=F]
#'
#'   list(PGx=grp_PGx, tau=grp_PGx_tau)
#' }
#'
#' #' Build samples report
#' #'
#' #'
#'  build_samples_report <- function(samples, all_drugs, samples_genos, guidelines_CYP, guidelines_SNP) {
#'   patients <- samples$PGx$Sample
#'   all_pt_info <- lapply(patients, extract_patient_info, samples=samples, all_drugs=all_drugs, samples_genos=samples_genos,
#'          guidelines_CYP=guidelines_CYP, guidelines_SNP=guidelines_SNP)
#'   names(all_pt_info) <- patients
#'   all_pt_info
#' }
#'
#' .extract_patient_info <- function(patient, samples, all_drugs, samples_genos, guidelines_CYP, guidelines_SNP) {
#'   sample_genos <- samples_genos[patient,,drop=F]
#'   first_drug <- samples$PGx$first_drug[samples$PGx$sample==patient]
#'   # mydrugs <- all_drugs[which(all_drugs != first_drug)]
#'   ptz_drugs <- all_drugs[all_drugs != first_drug]
#'   drugs_report <- lapply(ptz_drugs, extract_drug_guidelines, sample_genos=sample_genos,
#'                          guidelines_CYP=guidelines_CYP, guidelines_SNP=guidelines_SNP)
#'   names(drugs_report) <- ptz_drugs
#'   lights <- extract_suggestion_table(drugs_report)
#'
#'   details <- extract_detailed_suggestions(drugs_report)
#'   priority <- sapply(details, function(x) {x$category[1]})
#'   details <- details[order(priority)]
#'
#'   list(short=lights, long=details)
#' }
#'
#'
#' #' Extract minimal report
#' #'
#' #' @importFrom utils stack
#' .extract_suggestion_table <- function(drugs_report) {
#'   drug_light <- sapply(drugs_report, function(drug_rep) {
#'     suggestions <- lapply(drug_rep, "[[", "Category")
#'     suggestions <- do.call(c, suggestions)
#'     if ("red" %in% suggestions) {
#'       "red"
#'     } else if ("yellow" %in% suggestions) {
#'       "yellow"
#'     } else {
#'       "green"
#'     }
#'   })
#'   sugg <- stack(tapply(names(drug_light), drug_light, identity))
#'   sugg$ind <- factor(as.character(sugg$ind), levels=c("red", "yellow", "green"))
#'   sugg <- sugg[order(sugg$ind), ]
#'   colnames(sugg) <- c("Drug", "TI")
#'   row.names(sugg) <- NULL
#'   sugg
#' }
#'
#' #' Extract full report
#' #'
#' .extract_detailed_suggestions <- function(drugs_report) {
#'   drugs_info <- lapply(drugs_report, function(drug_rep) {
#'     info_num <- lapply(drug_rep, nrow)
#'     drug_rep[info_num!=0]
#'   })
#'   drugs_info_num <- lapply(drugs_info, length)
#'   drug_sugg <- drugs_info[drugs_info_num!=0]
#'   reformat_drug_sugg <- lapply(drug_sugg, reformat_info)
#' }
#'
#' #' Reformat the info
#' #'
#' .reformat_info <- function(info) {
#'   by_drug <- lapply(info, function(gene_info) {
#'     by_gene <- lapply(seq_len(nrow(gene_info)), function(idx){
#'       sample_info_line <- gene_info[idx, ,drop=F]
#'
#'       info_source <- NULL
#'       if (!is.null(sample_info_line$SOURCE))
#'         info_source <- paste0(", Source=", sample_info_line$SOURCE)
#'
#'       gene = paste0("Gene=", sample_info_line$Gene)
#'
#'       geno=NULL
#'       if (!is.null(sample_info_line$Genotype))
#'         geno = paste0(", Genotype=", sample_info_line$Genotype)
#'
#'       variant=NULL
#'       if (!is.null(sample_info_line$Variant))
#'         variant = paste0(", Variant=", sample_info_line$Variant)
#'
#'       pheno=NULL
#'       if (!is.null(sample_info_line$Phenotype))
#'         pheno = paste0(", Phenotype=", sample_info_line$Phenotype)
#'
#'
#'       core_info <- paste0("*Generale*: ", gene, pheno, variant, geno, info_source)
#'
#'       additional_info <- c()
#'       if (!is.null(sample_info_line$ITA))
#'         additional_info <- c(additional_info, paste("*Indicazioni*:", sample_info_line$ITA))
#'
#'       if (!is.null(sample_info_line$Clinical_relevance))
#'         additional_info <- c(additional_info, paste("*Clinical Relevance*:", sample_info_line$Clinical_relevance))
#'
#'       if (!is.null(sample_info_line$Reccomendations))
#'         additional_info <- c(additional_info, paste("*Reccomendations*:", sample_info_line$Reccomendations))
#'
#'       data.frame(text=paste(c(core_info, additional_info), collapse="\n\n"), category=sample_info_line$Category)
#'     })
#'     do.call(rbind, by_gene)
#'   })
#'   out <- do.call(rbind, by_drug)
#'   out$category <- factor(as.character(out$category), levels=c("red","yellow", "green", ""))
#'   out[order(out$category), ]
#' }
#'
#'
#' #' Extract drug infos
#' #'
#' #'
#' .extract_drug_guidelines <- function(drug, sample_genos, guidelines_CYP, guidelines_SNP) {
#'   drug_guide_CYP <- guidelines_CYP[guidelines_CYP$Drug == drug, ,drop=F]
#'   drug_guide_SNP <- guidelines_SNP[guidelines_SNP$Drug == drug, ,drop=F]
#'
#'   CYP <- c("CYP2D6", "CYP2C19")
#'   # extract_gene_guide("CYP2C19", drug_guide_CYP, sample_genos, variable="Gene")
#'   cyp_guide <- lapply(CYP, extract_gene_guide, drug_guide=drug_guide_CYP,
#'                       sample_genos=sample_genos, fvar="Gene", pg="Phenotype")
#'
#'   genes <- c("SLC6A4")
#'   slc_guide <- lapply(genes, extract_gene_guide, drug_guide=drug_guide_SNP,
#'                                   sample_genos=sample_genos, fvar="Gene", pg="Genotype")
#'
#'
#'   variants <- c("rs489693", "rs4713916", "rs7997012", "rs6295")
#'   variants_guide <- lapply(variants, extract_gene_guide, drug_guide=drug_guide_SNP,
#'                            sample_genos=sample_genos, fvar="Variant", pg="Genotype")
#'
#'   all_drug_guide <- c(cyp_guide, slc_guide, variants_guide)
#'   names(all_drug_guide) <- c(CYP, genes, variants)
#'   all_drug_guide
#' }
#'
#'
#'
#' #' Extract gene specific drug guide
#' #'
#' #' @param fvar feature Variable name to select column accordingly
#' #' @param pg Phenotype or Genotype
#' #'
#' .extract_gene_guide <- function(feature, drug_guide, sample_genos,
#'                                fvar=c("Gene", "Variant"),
#'                                pg=c("Genotype", "Phenotype")) {
#'
#'   genotype <- feature
#'   phenotype <- paste0(feature, "_pheno")
#'
#'   phenoOrGenotype <- genotype
#'
#'   if (pg[1] == "Phenotype") {
#'     phenoOrGenotype <- phenotype
#'   }
#'
#'   gene_drug_guide <- drug_guide[drug_guide[,fvar[1]] == feature, ,drop=F]
#'   gdg <- gene_drug_guide[gene_drug_guide[pg[1]] == sample_genos[,phenoOrGenotype], ]
#'   if (pg[1] == "Phenotype" & nrow(gdg)>0) {
#'     gdg$Genotype <- sample_genos[,feature]
#'   }
#'   gdg
#' }
#'
