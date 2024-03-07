plotModelMetric = function(metrics){
  metricNames = c("numIterations", "SSE", "CORCONDIA", "varExp")
  output = list()
  overviewPlotlist = list()
  numModels = nrow(metrics["numIterations"][[1]])
  numComponents = ncol(metrics["numIterations"][[1]])

  # Create and store a bar and box plot of every metric
  for(i in 1:length(metricNames)){
    metricName = metricNames[i]

    overviewBox = metrics[metricName][[1]] %>%
      dplyr::as_tibble() %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      ggplot2::ggplot(ggplot2::aes(x=as.factor(name),y=value)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("Number of components")

    overviewBar = metrics[metricName][[1]] %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(index=1:numModels) %>%
      tidyr::pivot_longer(-index) %>%
      ggplot2::ggplot(ggplot2::aes(x=index,y=value)) +
      ggplot2::facet_wrap(ggplot2::vars(name)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::xlab("Model index")

    if(metricName == "numIterations"){
      overviewBox = overviewBox + ggplot2::ylab("Number of iterations")
      overviewBar = overviewBar + ggplot2::ylab("Number of iterations")
      output$numIterations = overviewBar
    } else if(metricName == "SSE"){
      overviewBox = overviewBox + ggplot2::ylab("Sum of squared errors")
      overviewBar = overviewBar + ggplot2::ylab("Sum of squared errors")
      output$SSE = overviewBar
    } else if(metricName == "CORCONDIA"){
      overviewBox = overviewBox + ggplot2::ylab("CORCONDIA")
      overviewBar = overviewBar + ggplot2::ylab("CORCONDIA")
      output$CORCONDIA = overviewBar
    } else if (metricName == "varExp"){
      overviewBox = overviewBox + ggplot2::ylab("Variance explained (%)") + ggplot2::ylim(0,100)
      overviewBar = overviewBar + ggplot2::ylab("Variance explained (%)") + ggplot2::ylim(0,100)
      output$varExp = overviewBar
    }

    overviewPlotlist[[i]] = overviewBox
  }

  # Plot TCC separately
  output$TCC = list()
  output$TCCoverall = list()
  for(i in 2:numComponents){
    output$TCC[[i]] = plotModelTCCs(metrics$TCC[[i]])
    output$TCCoverall[[i]] = plotOverallTCCs(metrics$TCC[[i]])
  }

  # Add overview boxplot
  output$overview = ggpubr::ggarrange(plotlist=overviewPlotlist)
  return(output)
}

# Ugly solution to namespace issues caused by dplyr
index <- NULL
value <- NULL
name <- NULL
