library(dplyr)
library(stringr)
library(zoo)
library(tidyr)
library(lubridate)
library(reshape2)
library(data.table)
library(tidyverse)
library(cluster)
library(factoextra)
library(network)
detach(package:network)
rm(routes_network)
library(igraph)
library(tidygraph)
library(ggraph)

# generate basic tables we need 
setwd("C:/Users/320024909/OneDrive - Philips/YiranJia/Self_Projs/News_Analysis")
all <- read.csv("Philips_Hospital_HS_News_with_Articles.csv", stringsAsFactors = FALSE) 
colnames(all) <- c("date", "type", "title", "content", "link", "first_name", "first_id", "second_id", "third_id", "forth_id")
idn.rob <- read.csv("Rob_IDN.csv", stringsAsFactors = FALSE)
idn <- inner_join(all, idn.rob, by = "first_id") %>% select(-first_name.y) %>% mutate(first_name = first_name.x) %>% select(-first_name.x)
idn$date <- factor(as.yearqtr(idn$date, format = "%Y-%m-%d"))

# store the health system account name & its id
idn.names <- idn %>% select(-date, -type, -title, -content, -link, -second_id, -third_id, -forth_id) %>% unique()
# only left the info we are interested
idn.pure <- idn %>% select(-link, -second_id, -third_id, -forth_id)

# Topic One: Cluster Analysis (K-Mean)
# first - simple clustering analysis - idn_cluster
# the unstack function works when every row of wide table has unqiue idn name. it doesn't work if your expected result is an account with two 
# articles that have the same type in different dates, because that would be two rows with the same idn name.
idn.cluster <- idn.pure %>% mutate(num = 1) %>% select(-title, -content, date, type, num) %>% select(-date, -first_id)

# add the numbers of a style of an account all together. we have unique rows by the first 3 cols now.
idn.cluster <- aggregate(idn.cluster['num'], by = idn.cluster[c('first_name', 'type')], sum) 
idn.cluster.wide <- spread(idn.cluster, type, num)
idn.cluster.wide <- idn.cluster.wide[!duplicated(idn.cluster.wide$first_name),]
row.names(idn.cluster.wide) <- idn.cluster.wide[, 'first_name']
idn.cluster.wide <- idn.cluster.wide %>% select(-first_name)
idn.cluster.wide[is.na(idn.cluster.wide)] <- 0 
idn.cluster.wide <- idn.cluster.wide[-1,]

k2 <- kmeans(idn.cluster.wide, centers = 3, nstart = 25)
str(k2) # cluster 
cluster.result <- data.frame(k2$cluster)
cluster.result <- cluster.result %>% mutate(first_name = rownames(cluster.result)) %>% select(first_name, cluster = k2.cluster)
fviz_cluster(k2, data = idn.cluster.wide)

# how many clusters is the best
# method: elbow 
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(idn.cluster.wide, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(idn.cluster.wide, kmeans, method = "wss") # write above function in one line

# method: average silhouette method 
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(idn.cluster.wide, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
fviz_nbclust(idn.cluster.wide, kmeans, method = "silhouette") # write above function in one line

# method: gap statisitcs 
set.seed(123)
gap_stat <- clusGap(idn.cluster.wide, FUN = kmeans, nstart = 25,
                    K.max = 13, B = 50)
# print the result
print(gap_stat, method = "firstmax")
# visualize the results
fviz_gap_stat(gap_stat)

# export the report into Tableau to explore the relationships
idn_tableau <- left_join(cluster.result,idn.pure)
idn_tableau <- left_join(idn_tableau,idn.cluster)
write.csv(idn_tableau, "IDN_Tableau.csv")
# cluster 2 has the most number of articles, then 3, 1 has the least.
# 1,2,3 al has same trend - increasing number of articles over the year 
# 2 is increasing especially in Affilation/Partnerhsip, Finances/Funding, New Capabilities, Merger/Acquisition
# 3 is increasing especially in People on the Move it has the highest, Awards/Recognitions, light partnership, light finance, inc light new capabilities
# both 2&3 have inc Information Technology, Legal/Regulatory
# 1 is the least, but it did has the highest number of Labor issues articles. People on Move is the laste contribution
# 1 has been very consistent over time. it has few articles and the inc rate of each type is similar
# 1 has couple bankruptcy
# 2016-2018 boom period 

# Topic two - Network analysis
# https://www.jessesadler.com/post/network-analysis-with-r/
# I need two tables: edge list and node list
# the first one - node list 
sources <- all %>% select(first_name, first_id, second_id, third_id, forth_id)
node1 <- sources %>% select(first_id, first_name) %>% unique()
colnames(node1) <- c("id","label")
node <- function(node){
  temp <- sources %>% select(node) %>% filter(!is.na(node)) %>% unique()
  colnames(temp)[1] <-"id"
  temp <- left_join(temp, node1, by = "id") %>% filter(!is.na(label))
  return(temp)
}
node2 <- node("second_id")
node3 <- node("third_id")
node4 <- node("forth_id")
rest_node <- rbind(node2, node3, node4) # list of accounts that cannot solved by node1
dfhc_hospitals <- read.csv("Hospitals.csv", stringsAsFactors = FALSE)
dfhc_idns <- read.csv("IDNs.csv", stringsAsFactors = FALSE)
dfhc_acct <- rbind(dfhc_hospitals, dfhc_idns)
rest_node <- left_join(rest_node,dfhc_acct, by ="id") %>% filter(!is.na(label.y)) %>% select(-label.x)
colnames(rest_node)[2] <- "label"
nodes <- rbind(node1, rest_node) %>% unique()

repeat_id <- nodes %>% group_by(id) %>% summarise(count = n()) %>% filter(count > 1)
repeat_id <- left_join(repeat_id, nodes, by = "id")
to_delete <- seq(1, nrow(repeat_id), 2) # lucky, the max&min of repeated id is 2. they only repeat no more than twice. so I delete the odd row, or the ones old names
repeat_id <- repeat_id[-to_delete, c(1,3)]
nodes <- nodes[!(nodes$id %in% repeat_id$id),]
nodes <- rbind(nodes, repeat_id)

# the second one - edge list 
cnts <- sources %>% select(-first_name)
edge1 <- cnts %>% select(first_id, second_id) %>% filter(!is.na(second_id))
colnames(edge1) <- c("from","to")
edge2 <- cnts %>% select(first_id, third_id) %>% filter(!is.na(third_id))
colnames(edge2) <- c("from","to")
edge3 <- cnts %>% select(first_id, third_id) %>% filter(!is.na(third_id))
colnames(edge3) <- c("from","to")
edge_list <- rbind(edge1, edge2, edge3) %>% group_by(from, to) %>% summarise(weight = n()) %>% ungroup()

test <- edge_list
colnames(test)[1] <- "id"
test <- left_join(test, nodes, by = "id")
test <- test[!is.na(test$label),]
test <- test %>% select(-label)
test <- test %>% unique()
colnames(test) <- c("from", "id", "weight")
test <- left_join(test, nodes, by = "id")
test <- test[!is.na(test$label),]
test <- test %>% select(-label)
test <- test %>% unique()
colnames(test) <- c("from", "to", "weight")
edges <- test %>% unique()

edges <- top_n(edges,10,weight)

# network
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
plot(routes_network, vertex.cex = 3)

#igraph
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot.igraph(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex_size = 3, vertex_label = NA)

# tidy graph
# routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)







