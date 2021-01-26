ensureDHARMa <- function(simulationOutput,
                         convert = F){
  
  if(inherits(simulationOutput, "DHARMa")){
    return(simulationOutput)
  } else {
    
    if(convert == FALSE) stop("wrong argument to function, simulationOutput must be a DHARMa object!")
    else {
      
      if (class(simulationOutput)[1] %in% getPossibleModels()){
        if (convert == "Model" | convert == T) return(simulateResiduals(simulationOutput))
      } else if(is.vector(simulationOutput, mode = "numeric") & convert == T) {
        out = list()
        out$scaledResiduals = simulationOutput
        out$nObs = length(out$scaledResiduals)
        class(out) = "DHARMa"
        return(out)
      }
    }
  }
  stop("wrong argument to function, simulationOutput must be a DHARMa object or a numeric vector of quantile residuals!")
}

newplotQQunif <- function(simulationOutput, testUniformity = T, testOutliers = T, testDispersion = T, ...){
  
  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")
  
  gap::qqunif(simulationOutput$scaledResiduals,pch=2,bty="n", logscale = F, col = "black", cex = 0.6, main = "QQ plot residuals", cex.main = 1, ...)
  
  if(testUniformity == TRUE){
    temp = testUniformity(simulationOutput, plot = F)
    legend("topleft", c(paste("KS test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "blue" ), bty="n")
  }
  
  if(testOutliers == TRUE){
    temp = testOutliers(simulationOutput, plot = F)
    legend("bottomright", c(paste("Outlier test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "blue" ), bty="n")
  }
  
  if(testDispersion == TRUE){
    temp = testDispersion(simulationOutput, plot = F)
    legend("center", c(paste("Dispersion test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "blue" ), bty="n")
  }
  
}
