#' Check consistency for sweetpotato experimental data
#'
#' Set of rules to check for consistency of sweetpotato experimental data.
#' Data labels must be defined as specified in the PROCEDURES FOR THE EVALUATION
#' AND ANALYSIS OF SWEETPOTATO TRIALS document.
#' @param plot.size Plot size in square meters.
#' @param data The name of the data frame.
#' @return It returns a file with name checks.txt.
#' @author Raul Eyzaguirre.
#' @examples
#'  # The data
#'  head(pjpz09)
#'  str(pjpz09)
#'
#'  # Check the data
#'  spconsis(4.5, pjpz09)
#' @export

spconsis <- function(plot.size, data){

  options(width=240)

  sink("checks.txt")

  ## NOPS > NOPE > NOPH > NOPR

  if (exists("NOPE", where=data)==1 & exists("NOPS", where=data)==1)
    if (dim(subset(data, NOPE>NOPS))[1]>0){
      cat("\n","Number of plants established (NOPE) greater than number of plants sowed (NOPS):","\n")
      print(subset(data, NOPE>NOPS))
    }

  if (exists("NOPH", where=data)==1 & exists("NOPE", where=data)==1)
    if (dim(subset(data, NOPH>NOPE))[1]>0){
      cat("\n","Number of plants harvested (NOPH) greater than number of plants established (NOPE):","\n")
      print(subset(data, NOPH>NOPE))
    }

  sink()
}

