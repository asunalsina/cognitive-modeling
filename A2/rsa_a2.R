## Library
{
  library(d3heatmap)
  library(ggplot2)
  library(RColorBrewer)
  library(gplots)
  library(reshape2)
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
  RDM[[1]] <- 1 - cor(t(neural_responses))
  for(i in 1:12){
    RDM[[i+1]] <- 1 - cor(t(noise_responses[[i]]))
  }
  
  heatmap_neural_data <- melt(RDM[[1]])
  heatmap_participant <- melt(RDM[[2]])
  
  ggplot(heatmap_neural_data, aes(heatmap_neural_data$Var1, heatmap_neural_data$Var2, fill= heatmap_neural_data$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("")
  
  ggplot(heatmap_participant, aes(heatmap_participant$Var1, heatmap_participant$Var2, fill= heatmap_participant$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("")
  
  RDM_average <- list()
  for(i in 1:12){
    RDM_average[[i]] <- 1 - cor(t(noise_responses[[i]]))
  }
  
  average_matrix <- Reduce("+", RDM_average) / 12
  
  heatmap_average <- melt(average_matrix)
  
  ggplot(heatmap_average, aes(heatmap_average$Var1, heatmap_average$Var2, fill= heatmap_average$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("")
}

## First column
{
  first_column <- category_vectors[,1]
  first_column_matrix <- data.frame()
  for (i in first_column){
    column_compared <- i == first_column
    for(i in 1:length(column_compared)){
      if(column_compared[i]){
        column_compared[i] <- 1
      }else{
        column_compared[i] <- 0
        }
    }
    first_column_matrix <- rbind(first_column_matrix, column_compared)
  }
  matrix_without_diagonal <- first_column_matrix[lower.tri(first_column_matrix, diag = FALSE)]
  matrix_neural_responses <- neural_responses[lower.tri(neural_responses, diag = FALSE)]
  # animate = 1
  neural_animate <- matrix_neural_responses[matrix_without_diagonal == 1]
  # inanimate = 0
  neural_inanimate <- matrix_neural_responses[matrix_without_diagonal == 0]
  animation_t_test <- t.test(neural_animate, neural_inanimate, paired = FALSE)
}


