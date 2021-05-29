# clear 
rm(list = ls())
gc()

## load the library ##
library(FactoMineR)
library(ggplot2)
library(factoextra)

# load data
data <- read.csv("nearby_significant_cluster.csv")

# convert cluster to factor
cluster <- as.factor(data$cluster)

# select columns for MCA analysis
data <- data[,1:(ncol(data)-1)]

aggregate(data, by=list(cluster),
                    FUN=mean, na.rm=TRUE)


# place columns convert to place_y/n
yeses <- which(data == 1,arr.ind=TRUE)
nos <- which(data == 0,arr.ind=TRUE)

data[yeses] <- names(data)[yeses[,"col"]]
data[nos] <- paste0(names(data)[nos[,"col"]],"_no")

# bind cluster with place_y/n
data <- cbind(data,cluster)

# convert characters to factors for MCA
for (i in 1:ncol(data)){
  data[,i] <- as.factor(data[,i])
}
  
# check number of categories per variable
cats = apply(data, 2, function(x) nlevels(as.factor(x)))
cats 

# run mca
mca1 <- MCA(data, graph = FALSE)

# plot the graph
# data frame with variable coordinates - establishments
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates - store
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")
ggsave("mca-obs-vars.png", width = 9, height = 5)
#############

# plot mca on store clusters only
mca1_obs_df <- cbind(mca1_obs_df, cluster)
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2, color = cluster)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  #geom_density2d(colour = "gray80") +
  ggtitle("Multiple Correspondence Analysis - Clustered Stores") +
  scale_color_manual(values = c("red","darkgreen",'blue')) +
  xlab("Dimension 1 (17.00%)") +
  ylab("Dimension 2 (16.46%)")
ggsave("mca-obs-cluster.png", width = 7, height = 5)


# plot mca on variables only
mca1_vars_df <- mca1_vars_df[-seq(2,32, by = 2),] # remove place_nos
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(vjust = -0.5) +
  geom_point() +
  ggtitle("Multiple Correspondence Analysis - Establishments") +
  xlab("Dimension 1 (17.00%)") +
  ylab("Dimension 2 (16.46%)")
ggsave("mca-vars.png", width = 6, height = 5)


# plot mca on store - variables clusters only
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2, color = cluster)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("red","darkgreen","blue", "black")) +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), color = "black")) +
  ggtitle("Multiple Correspondence Analysis - Store & Establishments") +
  guides(color = FALSE) +
  xlab("Dimension 1 (17.00%)") +
  ylab("Dimension 2 (16.46%)")
ggsave("mca-obs-vars-new.png", width = 6, height = 5)

