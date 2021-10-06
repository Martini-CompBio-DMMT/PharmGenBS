#' Read and format sample info and sample genotypes
#'
#' @param sample_info_file Get patients' file info form web_app generated through Prepare_data_Mac_v2.py
#' @param sample_genotypes_file Get patients' genotype info file form web_app generated through Prepare_data_Mac_v2.py
#' @export
#'
generate_sample_info_table <- function(sample_info_file, sample_genotypes_file) {
  # samples_info <- read_sample_info("../web_app/samples_info.tsv")
  # samples_genotypes <- read_sample_genos("../web_app/samples_genos.tsv")
  # sample_info_table <- generate_sample_info_table("../web_app/samples_info.tsv", "../web_app/samples_genos.tsv")
  samples_info <- read_sample_info(sample_info_file)
  samples_genotypes <- read_sample_genos(sample_genotypes_file)

  sample_with_no_genotype <- setdiff(row.names(samples_info), row.names(samples_genotypes))
  if (length(sample_with_no_genotype)){
    warning(paste0("the following samples have no genotypes: \n",
                   paste(sample_with_no_genotype, collapse = ", ")))
  }

  sample_with_no_info <- setdiff(row.names(samples_genotypes), row.names(samples_info))
  if (length(sample_with_no_info)){
    stop(paste0("the following samples have no info: \n",
                   paste(sample_with_no_info, collapse = ", ")))
  }

  if (length(sample_with_no_genotype)) {
    missing_genotypes <- data.frame(matrix(NA, 1, 10, dimnames=list(sample_with_no_genotype, colnames(samples_genotypes))))
    missing_genotypes$Sample <- sample_with_no_genotype

    samples_genotypes <- rbind(samples_genotypes, missing_genotypes)
  }

  if (!identical(sort(row.names(samples_genotypes)), sort(row.names(samples_info))))
    stop("Sample names confusion: please check sample info and genotypes")

  samples_genotypes <- samples_genotypes[row.names(samples_info), , drop=F]
  if (!identical(row.names(samples_genotypes), row.names(samples_info)))
    stop("Sample names confusion: please check sample info and genotypes")
  samples_genotypes$Sample <- NULL

  cbind(samples_info, samples_genotypes)

}


#' Read in sample clinical info
#'
#' @param file file to read in
#' @importFrom utils read.table
#' @export
#'
read_sample_info <- function(file) {
  sinfo <- read.table(file, header=T, as.is=T)
  sinfo <- unique(sinfo)
  if (any(duplicated(sinfo$Sample)))
    stop(paste0("Sample in file \"", file,"\" is duplicated"))
  row.names(sinfo) <- sinfo$Sample
  sinfo
}


#' Read in sample genotypes
#'
#' @param file file to read in
#' @importFrom utils read.table
#' @export
#'
read_sample_genos <- function(file) {
  samples_genos <- read.table(file, header=T, as.is=T)
  samples_genos <- unique(samples_genos)
  if (any(duplicated(samples_genos$Sample)))
    stop(paste0("Sample in file \"", file,"\" is duplicated"))
  row.names(samples_genos) <- samples_genos$Sample
  samples_genos
}
