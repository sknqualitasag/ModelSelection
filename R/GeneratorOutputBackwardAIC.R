### #
### # Generate an output with result of step backward aic for each model
### # 2020-04-24 (skn)
### #-------------------------------------------------------------------


#' @title Generate an output for result of step backward for carcass
#'
#' @param psInputFile
#' @param psTrait
#' @param psOutputPath
#' @param psSampleID
#' @param psTraitComment
#' @export generate_output_backAIC
generate_output_backAIC <- function(psInputFile,
                                    psTrait,
                                    psOutputPath,
                                    psSampleID,
                                    psTraitComment){

  suppressPackageStartupMessages(if(! require("dplyr")) {
    install.packages("dplyr", repos="https://stat.ethz.ch/CRAN/")
    require("dplyr")
  })

  ### # check parameters
  if(is.null(psInputFile)) {
    stop("--missingDataFile has not been specified")
  }

  # Build a model
  # With age as covariable
  if(psTraitComment == "mCov"){
    lm_model <- lm(as.formula(paste(psTrait, "~ sex + abattoir + classifier + yearsaison + breedcomb + het + rec + age + ageQuadrat")), data = psInputFile)
  }else{
    #Without age as covariable
    lm_model <- lm(as.formula(paste(psTrait, "~ sex + abattoir + classifier + yearsaison + breedcomb + het + rec")), data = psInputFile)
  }

  # Check the model with the function 'ols_step_backward_aic' of the Rpackage olsrr
  k <- olsrr::ols_step_backward_aic(lm_model)
  model <- paste(format(lm_model$terms)[1],format(lm_model$terms)[2],sep = "")
  optimumaic <- k$aics[length(k$predictors)+1]

  # Build a tibble as output
  # With age as covariable
  if(psTraitComment == "mCov"){
    tibble_output <- tibble::tibble(psSampleID, psTrait, psTraitComment, model, AIC = optimumaic, sex = 1, abattoir = 1, classifier = 1,
                                    age = 1, ageQuadrat = 1, yearsaison = 1, breedcomb = 1, het = 1, rec = 1)
  }else{
    #Without age as covariable
    tibble_output <- tibble::tibble(psSampleID, psTrait, psTraitComment, model, AIC = optimumaic, sex = 1, abattoir = 1, classifier = 1,
                                    age = NA, ageQuadrat = NA, yearsaison = 1, breedcomb = 1, het = 1, rec = 1)
  }

  # Update tibble with variables which are removed
  if(!is.null(k$predictors)){
    i <- 1
    for(i in 1:length(k$predictors)){
      variable2remove <- k$predictors[i]
      tibble_output[,variable2remove] <- 0
      i <- i+1
    }
  }

  # Write tibble-Output in a file
  readr::write_csv(x = tibble_output, path = paste(psOutputPath,paste(psSampleID,psTrait,psTraitComment,sep = "_"),".csv",sep = ""))

}
