rescaleStudies <- function(receptor) {
  da      <- get(receptor)
  studies <- names(da)[(default.val(DoOR_default = "num.charColumns") + 1):length(da)]

  da.rescaled <- da[,1:default.val(DoOR_default = "num.charColumns")]  
  for(i in studies) {
    ors.tested <- rownames(weight.globNorm)[which(!is.na(weight.globNorm[i]))]
    for(j in 1:length(ors.tested)) {
      if(j == 1) {
        rescale.x <- data.frame(get(ors.tested[j])[,i])
      } else {
        rescale.x <- cbind(rescale.x, get(ors.tested[j])[,i])
      }
    }
    names(rescale.x) <- ors.tested
    rescale.x <- DoORnorm(rescale.x)
    da.rescaled <- cbind(da.rescaled, rescale.x[,receptor])
  }
  
  colnames(da.rescaled)[(default.val(DoOR_default = "num.charColumns") + 1):length(da)] <- studies
  return(da.rescaled)
}
