#' getChemicalClass
#'
#' function for getting the chemical class of a molecule from its name
#'
#' @param chemical.names vector of molecule names
#'
#' @examples getChemicalClass(c("ethanol", "acetic acid", "isopentyl acetate"))
getChemicalClass <- function(chemical.names) {
  class <- rep(NA, length(chemical.names))

  class[grep('.*ate\\>|.*at\\>', chemical.names)]                <- 'ester'
  class[grep('.*one\\>|.*on\\>', chemical.names)]                <- 'ketone'
  class[grep('.*al\\>|.*aldehyde\\>', chemical.names)]           <- 'aldehyde'
  class[grep('.*ol\\>', chemical.names)]                         <- 'alcohol'
  class[grep('.*acid\\>', chemical.names)]                       <- 'acid'
  class[grep('.*amid\\>|.*amide\\>', chemical.names)]            <- 'amide'
  class[grep('.*amin\\>|.*amine\\>', chemical.names)]            <- 'amine'
  class[grep('.*ane\\>|.*an\\>', chemical.names)]                <- 'alcane'
  class[grep('benzoate|benzene|phenyl|phenol>', chemical.names)] <- 'arom'

  return(class)
}
