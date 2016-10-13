convertVectorNameToExpression <- function(vec = vector()){
      sapply(as.vector(strsplit(
            paste(c('pleasure','arousal','dominance','colourfullness','meanBrightness','stdBrightness','meanSaturation','stdSaturation'),
                  collapse=",")
            ,",")), eval)
}