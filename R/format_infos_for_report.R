#' Extract minimal report
#'
#' @importFrom utils stack
extract_suggestion_table <- function(drugs_info) {
  drug_light <- sapply(drugs_info, function(drug_rep) {
    suggestions <- lapply(drug_rep, "[[", "Category")
    suggestions <- do.call(c, suggestions)
    if ("ROSSO" %in% suggestions) {
      "ROSSO"
    } else if ("GIALLO" %in% suggestions) {
      "GIALLO"
    } else {
      "VERDE"
    }
  })
  sugg <- stack(tapply(names(drug_light), drug_light, identity))
  sugg$ind <- factor(as.character(sugg$ind), levels=c("ROSSO", "GIALLO", "VERDE"))
  sugg <- sugg[order(sugg$ind), ]
  colnames(sugg) <- c("Drug", "TI")
  row.names(sugg) <- NULL
  sugg
}


#' Extract full report
#'
extract_detailed_suggestions <- function(drugs_report) {
  drugs_info <- lapply(drugs_report, function(drug_rep) {
    info_num <- lapply(drug_rep, nrow)
    drug_rep[info_num!=0]
  })
  drugs_info_num <- lapply(drugs_info, length)
  drug_sugg <- drugs_info[drugs_info_num!=0]
  ## info=drug_sugg[[1]]
  reformat_drug_sugg <- lapply(drug_sugg, reformat_info)
}

#' Reformat the info
#'
reformat_info <- function(info) {
  by_drug <- lapply(info, function(gene_info) {
    by_gene <- lapply(seq_len(nrow(gene_info)), function(idx){
      sample_info_line <- gene_info[idx, ,drop=F]

      gene = sample_info_line$Gene
      label="FD"
      if (gene %in% c("CYP2D6", "CYP2C19"))
        label="FC"

      info_source <- NULL
      if (!is.null(sample_info_line$SOURCE))
        info_source <- paste0(", Source=", sample_info_line$SOURCE)

      gene = paste0("Gene=", sample_info_line$Gene)

      geno=NULL
      if (!is.null(sample_info_line$Genotype))
        geno = paste0(", Genotype=", sample_info_line$Genotype)

      variant=NULL
      if (!is.null(sample_info_line$Variant))
        variant = paste0(", Variant=", sample_info_line$Variant)

      pheno=NULL
      if (!is.null(sample_info_line$Phenotype))
        pheno = paste0(", Phenotype=", sample_info_line$Phenotype)


      core_info <- paste0("*Generale*: ", gene, pheno, variant, geno, info_source)

      data.frame(label=label,
                 text=paste0(sample_info_line$Indicazione_Clinica, "\n\n"),
                 richText=paste(c(core_info, sample_info_line$Indicazione_Clinica), collapse="\n\n"),
                 category=sample_info_line$Category)
    })
    do.call(rbind, by_gene)
  })

  out <- do.call(rbind, by_drug)
  out$category <- factor(as.character(out$category), levels=c("ROSSO","GIALLO", "VERDE", ""))
  out[order(out$category), ]
}
