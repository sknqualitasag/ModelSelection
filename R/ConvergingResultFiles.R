### #
### # Converging result-files of step backward aic in one result-file
### # 2020-04-27 (skn)
### #-------------------------------------------------------------------


#' Converging result-files of step backward aic in one result-file per sample
#'
#' @param psDirectory
#' @param psOutputFile
#' @export convergingResults_perSample_backAIC
convergingResults_perSample_backAIC <- function(psDirectory,
                                                psOutputFile){

  ### # check parameters
  if(is.null(psDirectory)) {
    stop("--missingDirectory has not been specified")
  }

  ### # Read and combine multiple data files
  convergedFiles <- readbulk::read_bulk(directory = psDirectory,
                                        extension = ".csv",
                                        fun = readr::read_csv)

  ### # Write output file
  readr::write_csv(x = convergedFiles, path = paste(psDirectory,psOutputFile,sep = ""))

}



#' Converging result-files of step backward aic in one overall result-file
#'
#' @param psDirectory
#' @param psSubdirectories
#' @param psOutputFile
#' @export convergingResults_backAIC
convergingResults_backAIC <- function(psDirectory,
                                      psSubdirectories,
                                      psOutputFile){
  ### # check parameters
  if(is.null(psDirectory)) {
    stop("--missingDirectory has not been specified")
  }

  ### # Read and combine multiple data files
  convergedFilePerTrait <- readbulk::read_bulk(directory = psDirectory,
                                               subdirectories = psSubdirectories,
                                               extension = "Overview_ResultPerSample_backAIC.csv",
                                               fun = readr::read_csv)

  ### # Write output file
  readr::write_csv(x = convergedFilePerTrait, path = paste(psDirectory,psOutputFile,sep = ""))

}
