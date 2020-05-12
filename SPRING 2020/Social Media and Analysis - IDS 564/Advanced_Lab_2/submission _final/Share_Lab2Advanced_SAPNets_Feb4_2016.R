# Ali Tafti
# Optional Advanced Lab 2: SAP Community Network
#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"~/Downloads/SPRING 2020/Social Media and Analysis - IDS 564/Advanced_Lab_2/"
setwd(dir_path)

# clear everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame
str(el)

# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_SAPSub)
## Vertices
vcount(g_SAPSub)


## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---

# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

# Edges
ecount(g_SAPSub_simpl)
## Vertices
vcount(g_SAPSub_simpl)

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <-inv_weight

# You can see the neighbors of some selected nodes
neighbors(g_SAPSub_simpl, v=c('900'))
neighbors(g_SAPSub_simpl, v=c('592540'))

# plot the simple graph:
plot(g_SAPSub_simpl, vertex.label= NA,
     edge.arrow.size=0.7,
     main="plot for simplified graph")
plot(g_SAPSub_simpl,vertex.label=V(g_SAPSub_simpl)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)

#tkplot - also used to run different layout on graph 
#tkplot(g_SAPSub_simpl)

# above isan interactive GUI, for purpose of report writing R code to generate different layouts:

# 1. Fructermn layout

#plot(g_SAPSub_simpl, layout=layout.fruchterman.reingold, main="fruchterman.reingold") - Not visibily interpreatable
l <- layout.fruchterman.reingold(g_SAPSub_simpl, dim =3)
plot(g_SAPSub_simpl, layout=l,vertex.label=NA,main="fruchterman.reingold Algorithm")

plot(g_SAPSub_simpl,vertex.label= NA,vertex.size=3,layout = l,edge.width = 1, edge.arrow.size = 0.6,vertex.size2 = 5,margin = -0.2)

# 2 : Trying in circle layout - Not visibily interpreatable
l <- layout.circle(g_SAPSub_simpl)
plot(g_SAPSub_simpl, layout=l,vertex.label=NA,main="Circle Layout")

# fast greedy - only for undirected graphs
# spring glass - does not work with unconnected graph

# walktrap 
sap_comm_walk <- walktrap.community(g_SAPSub_simpl, weights=E(g_SAPSub_simpl)$weight)
plot(sap_comm_walk,g_SAPSub_simpl, vertex.label= NA, vertex.size=3, 
     edge.width =1, edge.arrow.size = 0.6,) + title(main = "Walktrap Algorithm")

E(g_SAPSub_simpl)$weight <- inv_weight


reciprocity(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")

# Diameter with both kinds of weights

# Clustering
transitivity(g_SAPSub_simpl, weights = inv_weight)
# Avg. path length and diameter
average.path.length(g_SAPSub_simpl, directed=TRUE)

diameter(g_SAPSub_simpl)
diameter(g_SAPSub_simpl, weights= num_weight)
diameter(g_SAPSub_simpl, weights= inv_weight)

# Summarize the graph structure
summary(g_SAPSub_simpl)

# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))
#A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)

# cliques of size 2,3,4,5 
cliques_all <- maximal.cliques(g_SAPSub_simpl)

largest_cliq <- largest.cliques(g_SAPSub_simpl)

cliques <- c(largest_cliq[[1]],largest_cliq[[2]],largest_cliq[[3]],largest_cliq[[4]],largest_cliq[[5]])
#View(cliques)

g2 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(cliques))
plot(g2,main="Cliques with size 5")


fourcliques <- max_cliques(g_SAPSub_simpl, min = 4, max = 4, subset = NULL,file = NULL)
fourcliquesall<- c()

for (i in c(1:39)) {
        fourcliquesall <- c(fourcliquesall,fourcliques[[i]])
}

fourcliquesall

g2 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(fourcliquesall))
plot(g2,main="cliques of size 4")
plot(g2,vertex.label=V(g2)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)


fourcliques <- max_cliques(g_SAPSub_simpl, min = 4, max = 4, subset = NULL,file = NULL)
fourcliquesall<- c()

for (i in c(1:39)) {
        fourcliquesall <- c(fourcliquesall,fourcliques[[i]])
}

fourcliquesall

g2 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(fourcliquesall))
plot(g2,vertex.label=V(g2)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,
     vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2, main="cliques of size 4")

# three cliques
threecliques <- max_cliques(g_SAPSub_simpl, min = 3, max = 3, subset = NULL,file = NULL)
threecliquesall<- c()

for (i in c(1:335)) {
        threecliquesall <- c(threecliquesall,threecliques[[i]])
}


threecliquesall

g3 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(threecliquesall))

plot(g3,vertex.label=V(g3)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2, main="cliques of size 3")


# two cliques
# three cliques
twocliques <- max_cliques(g_SAPSub_simpl, min = 2, max = 2, subset = NULL,file = NULL)
twocliquesall<- c()

for (i in c(1:3320)) {
        twocliquesall <- c(twocliquesall,twocliques[[i]])
}


g4 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(twocliquesall))

plot(g4,vertex.label=V(g4)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2, main="cliques of size 2")


# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight

# Embeddedness/ n (see Burt 2004)
constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
constraints_SAP
df<-data.frame(constraints_SAP)

df<-data.frame(rownames(df),constraints_SAP)
colnames(df)<-c('Vertex','constraints_SAP')

top20_holes<-df[order(df$constraints_SAP),][1:20,]

vertex1<-as.character((top20_holes$Vertex))
top20_holes_subgraph<-induced_subgraph(g_SAPSub_simpl,vids=vertex1)

plot(top20_holes_subgraph,vertex.size=15,edge.color='black',
     edge.arrow.width=0.3,vertex.label.cex=0.7) + title("Top 20 Nodes with Highest Structural Hole")


# Degree centrality
degree_sap <- degree(g_SAPSub_simpl)
# Node betweenness
betweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
# Local clustering coefficients
clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight, inv_weight)
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
node_frame<-data.frame(betweens_SAP, constraints_SAP, clustering_SAP, degree_sap)
a_node<-aggregate(betweens_SAP ~ clustering_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")


# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(betweens_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")
a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
a_node<-aggregate(clustering_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
a_node<-aggregate(constraints_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")


#degree centrality

degree_sap <- degree(g_SAPSub_simpl)
degree_df<-data.frame(degree_sap)
degree_df<-data.frame(rownames(degree_df),degree_sap)
colnames(degree_df)<-c('Vertex','Degree')

top20_degree<-degree_df[order(degree_df$Degree,decreasing = T),][1:20,]
d1<-as.character(top20_degree$Vertex)
top20_degree_subgraph<-induced_subgraph(g_SAPSub_simpl,vids=d1)

plot(top20_degree_subgraph,vertex.size=degree(top20_degree_subgraph)*3,edge.color='black',
     edge.arrow.width=0.3,vertex.label.cex=0.7) 

# Log-log degree distributino
par(mfrow=c(1, 2))
d.net <-degree(g_SAPSub_simpl)
dd.net <- degree.distribution(g_SAPSub_simpl)
d <- 1:max(d.net)-1
ind <- (dd.net != 0)
plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

# CHUNK 8# Average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(g_SAPSub_simpl,V(g_SAPSub_simpl))$knn
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))
