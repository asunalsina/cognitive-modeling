## Library
{
  library(d3heatmap)
}

## Read tables
{
  neural_responses <- read.table("NeuralResponses")
  behaviour_rdm <- read.table("BehaviourRDM")
  category_labels <- read.table("CategoryLabels")
  category_vectors <- read.table("CategoryVectors")
  hmax_rdm <- read.table("HmaxRDM")
  neuro_rdm <- read.table("NeuroRDM")
}

## Generate noise
{
  # mean = 0, standard_deviation = 1
  noise_responses <- list()
  for(j in 1:12){
    noise <- data.frame()
  
    for (i in 1:92){
      noise <- rbind(noise, rnorm(100, mean = 0, sd = 1))
    }

    noise_responses[[j]] <- neural_responses + noise
  }
}

## RDMs
{
  RDM <- list()
  RDM[[1]] <- 1 - cor(neural_responses)
  for(i in 1:12){
    RDM[[i+1]] <- 1 - cor(noise_responses[[i]])
  }
}
