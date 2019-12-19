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

## First column t.test
{
  first_column <- category_vectors[,1]
  first_column_matrix <- data.frame()
  for (i in first_column){
    first_column_matrix <- rbind(first_column_matrix, i == first_column)
  }
  first_without_diagonal <- first_column_matrix[lower.tri(first_column_matrix, diag = FALSE)]
  
  # diagonal matrix
  first_neural_responses <- neural_responses[lower.tri(neural_responses, diag = FALSE)]
  # animate = 1
  first_neural_animate <- first_neural_responses[first_without_diagonal == 1]
  # inanimate = 0
  first_neural_inanimate <- first_neural_responses[first_without_diagonal == 0]
  first_neural_responses_t_test <- t.test(first_neural_animate, first_neural_inanimate, paired = FALSE)
  
  # diagonal matrix
  first_individual <- noise_responses[[1]][lower.tri(noise_responses[[1]], diag = FALSE)]
  # animate = 1
  first_individual_animate <- first_individual[first_without_diagonal == 1]
  # inanimate = 0
  first_individual_inanimate <- first_individual[first_without_diagonal == 0]
  first_individual_t_test <- t.test(first_individual_animate, first_individual_inanimate, paired = FALSE)
  
  # diagonal matrix
  first_average <- average_matrix[lower.tri(average_matrix, diag = FALSE)]
  # animate = 1
  first_average_animate <- first_average[first_without_diagonal == 1]
  # inanimate = 0
  first_average_inanimate <- first_average[first_without_diagonal == 0]
  first_average_individual_t_test <- t.test(first_average_animate, first_average_inanimate, paired = FALSE)
}

## Sixth column t.test
{
  sixth_column <- category_vectors[,6]
  sixth_column_matrix <- data.frame()
  for (i in sixth_column){
    sixth_column_matrix <- rbind(sixth_column_matrix, i == sixth_column)
  }
  sixth_without_diagonal <- sixth_column_matrix[lower.tri(sixth_column_matrix, diag = FALSE)]
  
  # diagonal matrix
  sixth_neural_responses <- neural_responses[lower.tri(neural_responses, diag = FALSE)]
  # animate = 1
  sixth_neural_animate <- sixth_neural_responses[sixth_without_diagonal == 1]
  # inanimate = 0
  sixth_neural_inanimate <- sixth_neural_responses[sixth_without_diagonal == 0]
  sixth_neural_responses_t_test <- t.test(sixth_neural_animate, sixth_neural_inanimate, paired = FALSE)
  
  # diagonal matrix
  sixth_individual <- noise_responses[[1]][lower.tri(noise_responses[[1]], diag = FALSE)]
  # animate = 1
  sixth_individual_animate <- sixth_individual[sixth_without_diagonal == 1]
  # inanimate = 0
  sixth_individual_inanimate <- sixth_individual[sixth_without_diagonal == 0]
  sixth_individual_t_test <- t.test(sixth_individual_animate, sixth_individual_inanimate, paired = FALSE)
  
  # diagonal matrix
  sixth_average <- average_matrix[lower.tri(average_matrix, diag = FALSE)]
  # animate = 1
  sixth_average_animate <- sixth_average[sixth_without_diagonal == 1]
  # inanimate = 0
  sixth_average_inanimate <- sixth_average[sixth_without_diagonal == 0]
  sixth_average_individual_t_test <- t.test(sixth_average_animate, sixth_average_inanimate, paired = FALSE)
}

