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

## First column t.test (animate - inanimate)
{
  first_column <- category_vectors[,1]
  first_column_matrix <- data.frame()
  for (i in first_column){
    first_column_matrix <- rbind(first_column_matrix, i == first_column)
  }
  first_without_diagonal <- first_column_matrix[lower.tri(first_column_matrix, diag = FALSE)]
  
  # diagonal matrix
  first_neural_responses <- neural_responses[lower.tri(neural_responses, diag = FALSE)]
  # animate -> TRUE
  first_neural_animate <- first_neural_responses[first_without_diagonal == TRUE]
  # inanimate -> FALSE
  first_neural_inanimate <- first_neural_responses[first_without_diagonal == FALSE]
  first_neural_responses_t_test <- t.test(first_neural_animate, first_neural_inanimate, paired = FALSE)
  
  # diagonal matrix
  first_individual <- noise_responses[[1]][lower.tri(noise_responses[[1]], diag = FALSE)]
  # animate -> TRUE
  first_individual_animate <- first_individual[first_without_diagonal == TRUE]
  # inanimate -> FALSE
  first_individual_inanimate <- first_individual[first_without_diagonal == FALSE]
  first_individual_t_test <- t.test(first_individual_animate, first_individual_inanimate, paired = FALSE)
  
  # diagonal matrix
  first_average <- average_matrix[lower.tri(average_matrix, diag = FALSE)]
  # animate -> TRUE
  first_average_animate <- first_average[first_without_diagonal == TRUE]
  # inanimate -> FALSE
  first_average_inanimate <- first_average[first_without_diagonal == FALSE]
  first_average_individual_t_test <- t.test(first_average_animate, first_average_inanimate, paired = FALSE)
}

## Sixth column t.test (face - no face)
{
  sixth_column <- category_vectors[,6]
  sixth_column_matrix <- data.frame()
  for (i in sixth_column){
    sixth_column_matrix <- rbind(sixth_column_matrix, i == sixth_column)
  }
  sixth_without_diagonal <- sixth_column_matrix[lower.tri(sixth_column_matrix, diag = FALSE)]
  
  # diagonal matrix
  sixth_neural_responses <- neural_responses[lower.tri(neural_responses, diag = FALSE)]
  # face -> TRUE
  sixth_neural_face <- sixth_neural_responses[sixth_without_diagonal == TRUE]
  # no face -> FALSE
  sixth_neural_noface <- sixth_neural_responses[sixth_without_diagonal == FALSE]
  sixth_neural_responses_t_test <- t.test(sixth_neural_face, sixth_neural_noface, paired = FALSE)
  
  # faceness among animate objects
  face_animate <- sixth_neural_responses[sixth_without_diagonal == TRUE & first_without_diagonal == TRUE]
  face_animate_t_test <- t.test(face_animate, first_neural_animate, paired = FALSE)
}

## Third column t.test (human - no human)
{
  third_column <- category_vectors[,3]
  third_column_matrix <- data.frame()
  for (i in third_column){
    third_column_matrix <- rbind(third_column_matrix, i == third_column)
  }
  third_without_diagonal <- third_column_matrix[lower.tri(third_column_matrix, diag = FALSE)]
  
  # diagonal matrix
  third_neural_responses <- neural_responses[lower.tri(neural_responses, diag = FALSE)]
  # human -> TRUE
  third_neural_human <- third_neural_responses[third_without_diagonal == TRUE]
  # no human -> FALSE
  third_neural_nohuman <- third_neural_responses[third_without_diagonal == FALSE]
  third_neural_responses_t_test <- t.test(third_neural_human, third_neural_nohuman, paired = FALSE)

  # faceness among animate objects
  human_animate <- third_neural_responses[third_without_diagonal == TRUE & first_without_diagonal == TRUE]
  human_animate_t_test <- t.test(human_animate, first_neural_animate, paired = FALSE)
}

