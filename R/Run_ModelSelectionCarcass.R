### #
### # Running Model Selection for carcass calves and adults
### # 2020-04-27 (skn)
### # ---------------------------------------------


#' @title Running model selection for carcass
#'
#' @param psInputFile
#' @param psOutputPath
#' @param psSampleID
#' @export running_ms_carcass
running_ms_carcass <- function(psInputFile,
                               psOutputPath,
                               psSampleID){

  suppressPackageStartupMessages(if(! require("dplyr")) {
    install.packages("dplyr", repos="https://stat.ethz.ch/CRAN/")
    require("dplyr")
  })

  ### # check parameters
  if(is.null(psInputFile)) {
    stop("--missingDataFile has not been specified")
  }

  ### # Read dataset
  tbl_data <- readr::read_delim(file = psInputFile, delim = " ")

  ### # Remove and rename some columns
  tbl_data <- tbl_data %>% select(-c(herdyear,herd,calfcull,controlcull,ageVK,slaughterCategory,branding,ccvn,ccvo,cwvn,cwvo,cfvn,cfvo,cavn,cavo))
  names(tbl_data)[30] <- "het"
  names(tbl_data)[31] <- "rec"

  ### # Convert all fixed effects into factors
  tbl_data$sex <- as.factor(tbl_data$sex)
  tbl_data$abattoir <- as.factor(tbl_data$abattoir)
  tbl_data$classifier <- as.factor(tbl_data$classifier)
  tbl_data$yearsaison <- as.factor(tbl_data$yearsaison)
  tbl_data$breedcomb <- as.factor(tbl_data$breedcomb)
  tbl_data$prodsystem <- as.factor(tbl_data$prodsystem)
  tbl_data$het <- as.factor(tbl_data$het)
  tbl_data$rec <- as.factor(tbl_data$rec)

  tbl_inputFile <- tbl_data

  ### # All trait without covariable age and ageQuadrat
  Merkmal_oCov <- c("cccn","ccco","ccan","ccas","ccao",
                    "cwcn","cwco","cwan","cwas","cwao",
                    "cfcn","cfco","cfan","cfas","cfao",
                    "cacn","caco","caan","caas","caao")

  for(mrkml in Merkmal_oCov){
    cat("mrkml: ", mrkml,"\n")
    ModelSelection::generate_output_backAIC(psInputFile = tbl_inputFile,
                                            psTrait = mrkml,
                                            psOutputPath = psOutputPath,
                                            psSampleID = psSampleID,
                                            psTraitComment = "oCov")
  }

  ### # All trait with covariable age and ageQuadrat
  Merkmal_mCov <- c("cccn","ccco","ccan","ccas","ccao",
                    "cwcn","cwco","cwan","cwas","cwao",
                    "cfcn","cfco","cfan","cfas","cfao")


  for(mrkml in Merkmal_mCov){
    ModelSelection::generate_output_backAIC(psInputFile = tbl_inputFile,
                                            psTrait = mrkml,
                                            psOutputPath = psOutputPath,
                                            psSampleID = psSampleID,
                                            psTraitComment = "mCov")
  }

  ### # Output with all result of a sample
  ModelSelection::convergingResults_perSample_backAIC(psDirectory = psOutputPath,
                                                      psOutputFile = "Overview_ResultPerSample_backAIC.csv")


}






