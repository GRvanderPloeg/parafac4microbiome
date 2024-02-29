#' Title
#'
#' @param TCC_cube Output of [[assessNumComponents]]$metrics$TCC
#'
#' @return Plot
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(ggpubr)
#'
#' X = Fujita2023$data
#' assessment = assessNumComponents(X,
#'  minNumComponents = 1,
#'  maxNumComponents = 5,
#'  numRepetitions = 10,
#'  ctol=1e-6,
#'  maxit=2500)
#'
#'  plotOverallTCCs(assessment$metrics$TCC[[2]])
plotOverallTCCs = function(TCC_cube){
  I = dim(TCC_cube)[1]
  J = dim(TCC_cube)[2]
  K = dim(TCC_cube)[3]
  L = dim(TCC_cube)[4]

  plotlist = list()
  plotIterator = 1

  for(i in 1:I){
    for(j in 1:J){

      if(i < j){
        overallTCC = rep(1, K)
        for(l in 1:L){
            overallTCC = overallTCC * TCC_cube[i,j,,l]
        }

        plotlist[[plotIterator]] = overallTCC %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(index=dplyr::row_number()) %>%
          ggplot2::ggplot(ggplot2::aes(x=index,y=value)) +
          ggplot2::geom_bar(stat="identity") +
          ggplot2::xlab("Model number") +
          ggplot2::ylab("Tucker congruence coefficient") +
          ggplot2::ggtitle(paste0("Overall TCC ", "Components ", i, " vs. ", j))
        plotIterator = plotIterator + 1
      }
    }
  }

  plot = ggpubr::ggarrange(plotlist=plotlist)
  return(plot)
}

# Ugly solution to namespace issues caused by dplyr
index <- NULL
value <- NULL
