# Cognitive Task analysis
library(tidyverse)
library(vegan)

# Ezgi has triplet data in the form we need to write out to Python
d <- read_csv("Salmon_responses_06_28_21.csv")

# However, we need to first separate them by individual

# # for each identity
#  for(id in unique(d$puid)){
#    temp <- d %>% filter(puid %in% id)
#    name <- paste0("individual_triplets/", id, ".csv")
#    write_csv(temp, name)
#  }
# 
# # for graduate/graduated-already
# for(id in unique(d$`Are you currently a graduate student or have you completed a graduate degree in a field that involves cognitive science/the use of cognitive science tasks (e.g., psychology, cognitive science, neuroscience, education, etc)?`)){
#   temp <- d %>% filter(`Are you currently a graduate student or have you completed a graduate degree in a field that involves cognitive science/the use of cognitive science tasks (e.g., psychology, cognitive science, neuroscience, education, etc)?` %in% id)
#   name <- paste0("grad_triplets/", id, ".csv")
#   write_csv(temp, name)
# }


task_list <- read_csv("Task List.csv")

# Reading in completed embeddings
student <- read_csv("embeddings/grad_group/2d_Yes, I am a graduate student.csv") 
student$label <- task_list$cog_task_name
names(student) <- c("row", "x", "y", "label")

ggplot() +
  geom_point(data = student, aes(x, y)) +  # Plot points
  geom_text(data = student, aes(x = x, y = y, label = label), vjust = -0.5)  # Plot text above the points


graduated <- read_csv("embeddings/grad_group/2d_Yes, I have completed a graduate degree.csv")
graduated$label <- task_list$cog_task_name
names(graduated) <- c("row", "x", "y", "label")

ggplot() +
  geom_point(data = graduated, aes(x, y)) +  # Plot points
  geom_text(data = graduated, aes(x = x, y = y, label = label), vjust = -0.5)  # Plot text above the points


#na_group <- read_csv("embeddings/grad_group/2d_NA.csv")

student$group <- rep("student",81)
graduated$group <- rep("graduated", 81)


student_dims <- student %>% select(x, y) %>% as.matrix()

sims <- student_dims %*% t(student_dims)
sims %>% heatmap(symm = FALSE)

# getting distance for tree and MDS
distance <- sims %>% as.dist()
clusters <- hclust(distance, method = "ward.D2")

library(plotly)
library(reshape2)
library(ape)

## Unrooted tree, colored by cluster from hierarchical clustering 
plot(as.phylo(clusters), type = "unrooted", cex = 0.6,
     no.margin = TRUE, tip.color = cutree(clusters, k = 20))






combined_df <- rbind(student, graduated)

dims_only <- combined_df %>% dplyr::select(x,y,group)
nas <- dims_only[which(is.na(dims_only$x)),]
dims_only <- dims_only %>% filter(!group %in% nas$group)


sims <-  matrix(0, ncol = length(unique(dims_only$group)), nrow = 0)
ids <- unique(dims_only$group) %>% sort()

for(id in ids){
  temp_list <- list()
  temp_id <- dims_only %>% filter(group == id)
  i <- 1
  for(id_2 in ids){
    aligned <- procrustes(temp_id[,1:2], dims_only[dims_only$group == id_2, 1:2])
    temp_list[i] <- aligned$ss
    i <- i + 1
  }
  sims <- rbind(sims, (unlist(temp_list)))
}
sims %>% heatmap(Rowv = NA, Colv = NA, symm = TRUE, verbose = T )


