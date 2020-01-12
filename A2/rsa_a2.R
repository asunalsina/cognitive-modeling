## Libraries
{
  library(d3heatmap)
  library(ggplot2)
  library(RColorBrewer)
  library(gplots)
  library(reshape2)
  library(lsr)
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
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("") + labs(fill = "Dissimilarity")
  
  ggplot(heatmap_participant, aes(heatmap_participant$Var1, heatmap_participant$Var2, fill= heatmap_participant$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("") + labs(fill = "Dissimilarity")
  
  RDM_average <- list()
  for(i in 1:12){
    RDM_average[[i]] <- 1 - cor(t(noise_responses[[i]]))
  }
  
  average_matrix <- Reduce("+", RDM_average) / 12
  
  heatmap_average <- melt(average_matrix)
  
  ggplot(heatmap_average, aes(heatmap_average$Var1, heatmap_average$Var2, fill= heatmap_average$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("") + labs(fill = "Dissimilarity")
}

## First column t.test (same animacy - different animacy)
{
  first_column <- category_vectors[,1]
  first_column_matrix <- data.frame()
  for (i in first_column){
    first_column_matrix <- rbind(first_column_matrix, i == first_column)
  }
  first_without_diagonal <- first_column_matrix[lower.tri(first_column_matrix, diag = FALSE)]
  
  # triangular matrix
  first_neural_responses <- RDM[[1]][lower.tri(RDM[[1]], diag = FALSE)]
  # same animacy -> TRUE
  first_neural_same <- first_neural_responses[first_without_diagonal == TRUE]
  # different animacy -> FALSE
  first_neural_diff <- first_neural_responses[first_without_diagonal == FALSE]
  first_neural_responses_t_test <- t.test(first_neural_same, first_neural_diff, paired = FALSE)
  
  # triangular matrix
  first_individual <- noise_responses[[1]][lower.tri(noise_responses[[1]], diag = FALSE)]
  # same animacy -> TRUE
  first_individual_same <- first_individual[first_without_diagonal == TRUE]
  # different animacy -> FALSE
  first_individual_diff <- first_individual[first_without_diagonal == FALSE]
  first_individual_t_test <- t.test(first_individual_same, first_individual_diff, paired = FALSE)
  
  # triangular matrix
  first_average <- average_matrix[lower.tri(average_matrix, diag = FALSE)]
  # same animacy -> TRUE
  first_average_same <- first_average[first_without_diagonal == TRUE]
  # different animacy -> FALSE
  first_average_diff <- first_average[first_without_diagonal == FALSE]
  first_average_individual_t_test <- t.test(first_average_same, first_average_diff, paired = FALSE)
}

## Sixth column t.test (face - no face)
{
  {
    sixth_column <- category_vectors[,6]
    sixth_column_matrix <- data.frame()
    for (i in sixth_column){
      sixth_column_matrix <- rbind(sixth_column_matrix, i == sixth_column)
    }
    sixth_without_diagonal <- sixth_column_matrix[lower.tri(sixth_column_matrix, diag = FALSE)]
  
    # triangular matrix
    sixth_neural_responses <- RDM[[1]][lower.tri(RDM[[1]], diag = FALSE)]
    # face -> TRUE
    sixth_neural_face <- sixth_neural_responses[sixth_without_diagonal == TRUE]
    # no face -> FALSE
    sixth_neural_noface <- sixth_neural_responses[sixth_without_diagonal == FALSE]
    sixth_neural_responses_t_test <- t.test(sixth_neural_face, sixth_neural_noface, paired = FALSE)
  }
  
  {
    # faceness among animate objects
    neural_responses_triangular <- RDM[[1]][lower.tri(RDM[[1]], diag = FALSE)]

    first_column_animate_matrix <- data.frame()
    for (i in first_column){
      if(i == 1){
        first_column_animate_matrix <- rbind(first_column_animate_matrix, i == first_column)
      }else{
        first_column_animate_matrix <- rbind(first_column_animate_matrix, rep(FALSE, length(first_column)))
      }
    }
    first_animate_without_diagonal <- first_column_animate_matrix[lower.tri(first_column_animate_matrix, diag = FALSE)]
  
    animate_face_matrix <- data.frame()
    for (i in 1:length(first_animate_without_diagonal)){
      if(first_animate_without_diagonal[i] == 1 & sixth_without_diagonal[i] == 1){
        animate_face_matrix <- rbind(animate_face_matrix, TRUE)
      }else{
        animate_face_matrix <- rbind(animate_face_matrix, FALSE)
      }
    }  
  
    # face -> TRUE
    first_neural_face_animate <- neural_responses_triangular[animate_face_matrix == TRUE]
    # no face -> FALSE
    first_neural_noface_animate <- neural_responses_triangular[animate_face_matrix == FALSE]
    face_t_test <- t.test(first_neural_face_animate, first_neural_noface_animate, paired = FALSE)
    face_t_test
  }
}

## Third column t.test (human - no human)
{
  {
    third_column <- category_vectors[,3]
    third_column_matrix <- data.frame()
    for (i in third_column){
      third_column_matrix <- rbind(third_column_matrix, i == third_column)
    }
    third_without_diagonal <- third_column_matrix[lower.tri(third_column_matrix, diag = FALSE)]
  
    # triangular matrix
    third_neural_responses <- RDM[[1]][lower.tri(RDM[[1]], diag = FALSE)]
    # human -> TRUE
    third_neural_human <- third_neural_responses[third_without_diagonal == TRUE]
    # no human -> FALSE
    third_neural_nohuman <- third_neural_responses[third_without_diagonal == FALSE]
    third_neural_responses_t_test <- t.test(third_neural_human, third_neural_nohuman, paired = FALSE)
  }
  
  {  
    # human among animate objects
    animate_human_matrix <- data.frame()
    for (i in 1:length(first_animate_without_diagonal)){
      if(first_animate_without_diagonal[i] == 1 & third_without_diagonal[i] == 1){
        animate_human_matrix <- rbind(animate_human_matrix, TRUE)
      }else{
        animate_human_matrix <- rbind(animate_human_matrix, FALSE)
      }
    }  
    
    # human -> TRUE
    first_neural_human_animate <- neural_responses_triangular[animate_human_matrix == TRUE]
    # no human -> FALSE
    first_neural_nohuman_animate <- neural_responses_triangular[animate_human_matrix == FALSE]
    human_t_test <- t.test(first_neural_human_animate, first_neural_nohuman_animate, paired = FALSE)
    human_t_test
  }
}

## ANOVA or linear model
{
  # sixth_column_animate_matrix
  # first_column_matrix
  neural_matrix <- RDM[[1]][lower.tri(RDM[[1]], diag = FALSE)]
  face_animated_matrix <- as.matrix(animate_face_matrix)
  animacy_matrix <- first_without_diagonal
  anova_test <- aov(neural_matrix ~ face_animated_matrix + animacy_matrix)
  summary(anova_test)
  etaSquared(anova_test, anova = FALSE)
  
  anova_test2 <- aov(neural_matrix ~ animacy_matrix + face_animated_matrix)
  summary(anova_test2)
  etaSquared(anova_test2, anova = FALSE)
}

## Macaque monkey
{
  # correlation
  macaque_matrix <- neuro_rdm[lower.tri(neuro_rdm, diag = FALSE)]
  average_participant_matrix <- average_matrix[lower.tri(average_matrix, diag = FALSE)]
  correlation_macaque_neuron <- cor.test(macaque_matrix, average_participant_matrix)
  correlation_macaque_neuron
  
  # scatter plot
  plot(average_participant_matrix, macaque_matrix, xlab = "Average participant", ylab = "Macaque neuron")
  
  # animated part
  neuro_animacy <- neuro_rdm[1:48, 1:48]
  macaque_animacy_matrix <- neuro_animacy[lower.tri(neuro_animacy, diag = FALSE)]
  average_animacy <- average_matrix[1:48, 1:48]
  average_animacy_matrix <- average_animacy[lower.tri(average_animacy, diag = FALSE)]
  correlation_animacy_macaque <- cor.test(macaque_animacy_matrix, average_animacy_matrix)
  correlation_animacy_macaque
  
  # inanimated part
  neuro_inanimated <- neuro_rdm[49:92, 49:92]
  macaque_inanimated_matrix <- neuro_inanimated[lower.tri(neuro_inanimated, diag = FALSE)]
  average_inanimated <- average_matrix[49:92, 49:92]
  average_inanimated_matrix <- average_inanimated[lower.tri(average_inanimated, diag = FALSE)]
  correlation_inanimated_macaque <- cor.test(macaque_inanimated_matrix, average_inanimated_matrix)
  correlation_inanimated_macaque
}

## Behaviour
{
  # image
  heatmap_behaviour <- melt(as.matrix(behaviour_rdm))
  
  ggplot(heatmap_behaviour, aes(heatmap_behaviour$Var1, heatmap_behaviour$Var2, fill= heatmap_behaviour$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("") + labs(fill = "Dissimilarity")
  
  # average subject
  behaviour_matrix <- behaviour_rdm[lower.tri(behaviour_rdm, diag = FALSE)]
  correlation_behaviour <- cor.test(behaviour_matrix, average_participant_matrix)
  correlation_behaviour
  
  # animated part
  behaviour_animacy <- behaviour_rdm[1:48, 1:48]
  behaviour_animacy_matrix <- behaviour_animacy[lower.tri(behaviour_animacy, diag = FALSE)]
  correlation_animacy_behaviour <- cor.test(behaviour_animacy_matrix, average_animacy_matrix)
  correlation_animacy_behaviour
  
  # inanimated part
  behaviour_inanimated <- behaviour_rdm[49:92, 49:92]
  behaviour_inanimated_matrix <- behaviour_inanimated[lower.tri(behaviour_inanimated, diag = FALSE)]
  correlation_inanimated_behaviour <- cor.test(behaviour_inanimated_matrix, average_inanimated_matrix)
  correlation_inanimated_behaviour
}

## HMAX
{
  # image
  heatmap_hmax <- melt(as.matrix(hmax_rdm))
  
  ggplot(heatmap_hmax, aes(heatmap_hmax$Var1, heatmap_hmax$Var2, fill= heatmap_hmax$value)) + 
    geom_tile() + scale_fill_viridis_c(option = "B") + xlab("") + ylab("") + labs(fill = "Dissimilarity")
  
  # average subject
  hmax_matrix <- hmax_rdm[lower.tri(hmax_rdm, diag = FALSE)]
  correlation_hmax <- cor.test(hmax_matrix, average_participant_matrix)
  correlation_hmax
  
  # animated part
  hmax_animacy <- hmax_rdm[1:48, 1:48]
  hmax_animacy_matrix <- hmax_animacy[lower.tri(hmax_animacy, diag = FALSE)]
  correlation_animacy_hmax <- cor.test(hmax_animacy_matrix, average_animacy_matrix)
  correlation_animacy_hmax
  
  # inanimated part
  hmax_inanimated <- hmax_rdm[49:92, 49:92]
  hmax_inanimated_matrix <- hmax_inanimated[lower.tri(hmax_inanimated, diag = FALSE)]
  correlation_inanimated_hmax <- cor.test(hmax_inanimated_matrix, average_inanimated_matrix)
  correlation_inanimated_hmax
}

