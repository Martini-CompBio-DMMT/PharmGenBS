#' Extract drug infos
#'
#' @export
#'
extract_drug_guidelines <- function(drug, sample_genotype) {
  data(fcinetica)
  data(fdinamica)

  drug_guide_c <- fcinetica[fcinetica$Drug == drug, ,drop=F]
  drug_guide_d <- fdinamica[fdinamica$Drug == drug, ,drop=F]

  CYP <- c("CYP2D6", "CYP2C19")
  # extract_gene_guide("CYP2C19", drug_guide_CYP, sample_genotype, variable="Gene")
  cyp_guide <- lapply(CYP, PharmGenBS:::extract_gene_guide, drug_guide=drug_guide_c,
                      sample_genotype=sample_genotype, fvar="Gene", pg="Phenotype")

  genes <- c("SLC6A4")
  slc_guide <- lapply(genes, PharmGenBS:::extract_gene_guide, drug_guide=drug_guide_d,
                      sample_genotype=sample_genotype, fvar="Gene", pg="Genotype")


  variants <- c("rs489693", "rs4713916", "rs7997012", "rs6295")
  variants_guide <- lapply(variants, PharmGenBS:::extract_gene_guide, drug_guide=drug_guide_d,
                           sample_genotype=sample_genotype, fvar="Variant", pg="Genotype")

  all_drug_guide <- c(cyp_guide, slc_guide, variants_guide)
  names(all_drug_guide) <- c(CYP, genes, variants)
  all_drug_guide
}



#' Extract gene specific drug guide
#'
#' @param fvar feature Variable name to select column accordingly
#' @param pg Phenotype or Genotype
#'
extract_gene_guide <- function(feature, drug_guide, sample_genotype,
                               fvar=c("Gene", "Variant"),
                               pg=c("Genotype", "Phenotype")) {

  if (!nrow(drug_guide)) {
    return(drug_guide)
  }

  genotype <- feature
  phenotype <- paste0(feature, "_pheno")

  phenoOrGenotype <- genotype

  if (pg[1] == "Phenotype") {
    phenoOrGenotype <- phenotype
  }

  if (is.na(sample_genotype[,phenoOrGenotype]))
    return(data.frame(Drug=character(), Gene=character(), Phenotype=character(),
                      Category=character(), Indicazione_Clinica=character(), Genotype=character()))

  gene_drug_guide <- drug_guide[drug_guide[,fvar[1]] == feature, ,drop=F]

  gdg <- gene_drug_guide[gene_drug_guide[pg[1]] == sample_genotype[,phenoOrGenotype], ]
  if (pg[1] == "Phenotype" & nrow(gdg)>0) {
    gdg$Genotype <- sample_genotype[,feature]
  }
  gdg
}

