## code to prepare `fcinetica` dataset goes here
## link from /Users/paolo/bioinfotree/prj/unibs-pharmacogenomics-farmacogenomica

fcinetica <- read.csv("data-raw/GuideLines_table_CYP_30-08-2021-farmacocinetica.csv",
                      sep=";", header=T, check.names = F,stringsAsFactors = F,
                      fileEncoding = "UTF-8")

colnames(fcinetica) <- c("Drug", "Gene", "Phenotype", "Category", "Indicazione_Clinica")

fdinamica <- read.csv("data-raw/GuideLines_table_FarmDinamica_02-09-2021_allele.csv",
                      sep=";", header=T, check.names = F,stringsAsFactors = F,
                      fileEncoding = "UTF-8")

colnames(fdinamica) <- c("Drug", "Gene", "Variant", "Genotype", "Category", "Indicazione_Clinica")
all_drugs <- unique(c(fcinetica$Drug, fdinamica$Drug))

usethis::use_data(fcinetica, fdinamica, all_drugs, overwrite = TRUE)



# # Encoding issue
#
# # The thing that worked for me was to declare the encoding as "latin1", and then use iconv to convert to UTF-8.
# #
# # Encoding(levels(english_monarchs$name)) <- "latin1"
# # levels(english_monarchs$name) <- iconv(
# #   levels(english_monarchs$name),
# #   "latin1",
# #   "UTF-8"
# # )
# #
