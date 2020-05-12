# Clearing everything out of memory
rm(list=ls()) 

# Input file
infile_nasdaq <- "~/Downloads/SPRING 2020/Social Media and Analysis - IDS 564/Advanced_Lab_5/wide_twitter_daily_lab5.csv"

#load iGraph package
library(igraph)

nasdaq = read.csv(infile_nasdaq, header = TRUE, sep = ",")
class(nasdaq)

# Describe the data frame
str(nasdaq)

# Looking at the data
head(nasdaq) 

# Converting 'datestart' column to rownames
rownames(nasdaq) <- nasdaq[,1]
nasdaq[,1] <- NULL

# Using regular lab #5 code snippets to analyze
heatmap(scale(nasdaq), Rowv=NA)

#generating corelations - to predict the links 
mycorr <- cor(nasdaq)

# plotting intial simple analysis based on correlation
library(dplyr)
library(corrr)
sum(is.na(nasdaq))

df_twitter_corr <- nasdaq %>% correlate() %>% stretch()
df_twitter_corr
names(df_twitter_corr)[3]<-"corr_coeff"

nw_corr_graph <- df_twitter_corr %>% filter(abs(corr_coeff) > 0.30) %>% graph_from_data_frame(directed = FALSE)
V(nw_corr_graph) 
E(nw_corr_graph)

#plot the same 
library(ggraph)

ggraph(nw_corr_graph, layout = 'kk') +
  geom_edge_link(aes(edge_alpha = abs(corr_coeff), edge_width = abs(corr_coeff), color = corr_coeff)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-0.3, 0.85), colors = c("steelblue1", "steelblue3")) +
  geom_node_point(color = "dodgerblue4", size = 6) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Network Correlation Between Nasdaq 100 Companies")

# CHUNK 9
# Fisher's transformation
z <- 0.5 * log((1 + mycorr) / (1 - mycorr))
z.vec <- z[upper.tri(z)]
n <- dim(nasdaq)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0, 
                        sqrt(1 / (n-3)), lower.tail=FALSE)

# total length of fisher's correlation p-vals
length(corr.pvals) ## [1] 4186

# Number of edges predicted: using statistical significance at the p < 0.05 threshold
length(corr.pvals[corr.pvals < 0.05]) ## [1] 483

# Benjamini-Hochberg adjustment to control for the false discovery rate
corr.pvals.adj <- p.adjust(corr.pvals, "BH")

# CHUNK 13
# Number of edges predicted: using  the overall correlation and statistical significance 
# at the p < 0.05 threshold
corr.edges <- (corr.pvals.adj < 0.05)
length(corr.pvals.adj[corr.edges])

summary(corr.pvals)
summary(corr.pvals.adj)

# Creating the graph predicted by the statistically significant overall correlations
corr.A <- matrix(0, 92, 92)
corr.A[lower.tri(corr.A)] <- as.numeric(corr.edges)
g.corr <- graph.adjacency(corr.A, "undirected")

library(fdrtool)

# CHUNK 15
mycorr.vec <- mycorr[upper.tri(mycorr)]
fdr <- fdrtool(mycorr.vec, statistic="correlation")

# CHUNK 16
# Below code for partial correlations to predict edges
pcorr.pvals <- matrix(0, dim(mycorr)[1], 
                      dim(mycorr)[2])
for(i in seq(1, 92)){
  for(j in seq(1, 92)){
    rowi <- mycorr[i, -c(i, j)]
    rowj <- mycorr[j, -c(i, j)]
    tmp <- (mycorr[i, j] - 
              rowi*rowj)/sqrt((1-rowi^2) * (1-rowj^2))
    tmp.zvals <- (0.5) * log((1+tmp) / (1-tmp))
    tmp.s.zvals <- sqrt(n-4) * tmp.zvals
    tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 
                           0, 1, lower.tail=FALSE)
    pcorr.pvals[i, j] <- max(tmp.pvals)
  }
}

pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]

#Benjamini-Hochberg adjustment to control for the false discovery rate
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# CHUNK 18
# Number of edges predicted: using  the partial correlation and statistical significance 
# at the p < 0.05 threshold
pcorr.edges <- (pcorr.pvals.adj < 0.05)
length(pcorr.pvals.adj[pcorr.edges]) ## [1] 48

# CHUNK 19
# Creating the graph predicted by the statistically significant partial correlations (p < 0.05)
pcorr.A <- matrix(0, 92, 92)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

# Number of edges predicted: using  the partial correlation and statistical significance 
# at the p < 0.01 threshold
pcorr.edges.01 <- (pcorr.pvals.adj < 0.01)
length(pcorr.pvals.adj[pcorr.edges.01]) ## [1] 32

# Creating the graph predicted by the statistically significant partial correlations (p < 0.01)
pcorr.A.01 <- matrix(0, 92, 92)
pcorr.A.01[lower.tri(pcorr.A.01)] <- as.numeric(pcorr.edges.01)
g.pcorr.01 <- graph.adjacency(pcorr.A.01, "undirected")

# Find overlap between the two graphs
graph.intersection(g.pcorr, g.pcorr.01, byname=FALSE)

# CHUNK 21
# FDR tool can also be used to adjust for false discovery rate and predict new edges 
# based on partial correlations
fdr <- fdrtool(pcorr.pvals.vec, statistic="pvalue", 
               plot=FALSE)
pcorr.edges.2 <- (fdr$qval < 0.05)
length(fdr$qval[pcorr.edges.2]) ## [1] 47

# Creating the graph predicted by the statistically significant partial correlations of FDR tool
pcorr.A.2 <- matrix(0, 92, 92)
pcorr.A.2[lower.tri(pcorr.A.2)] <- as.numeric(pcorr.edges.2)
g.pcorr.2 <- graph.adjacency(pcorr.A.2, "undirected")

# Find overlap between the graph predicted by partial correlations of 
# FDR tool with the partial correlations of BH
graph.intersection(g.pcorr, g.pcorr.2, byname=FALSE)


# HUGE (High-dimensional undirected graph estimation library) procedure for predicted links
# CHUNK 22

library(huge)
set.seed(42)

# Converting the data frame into a matrix for HUGE
nasdaq_mat <- data.matrix(nasdaq,rownames.force = NA)
huge.out <- huge(nasdaq_mat)

# CHUNK 23
huge.opt <- huge.select(huge.out, criterion="ric")
summary(huge.opt$refit)

# CHUNK 24
huge.opt <- huge.select(huge.out, criterion="stars")
g.huge <- graph.adjacency(huge.opt$refit, "undirected")
plot(g.huge, vertex.size=3, vertex.label=NA)
summary(g.huge)

# CHUNK 25
# Find overlap between the graph produced by the partial adjusted correlations (BH) 
# with graph produced by HUGE library
graph.intersection(g.pcorr, g.huge)

# CHUNK 26
# Find overlap between the graph produced by the partial adjusted correlations (FDR tools) 
# with graph produced by HUGE library
graph.intersection(g.pcorr.2, g.huge, byname=FALSE)

#######################
### Network Plot: 1 ###
#######################
# Benjamini-Hochberg adjustment used to control for the false discovery rate;
# and a threshold of p < 0.05 used to identify statistically significant overall
# correlations. 
graph.corrL <- set.vertex.attribute(g.corr, "name", value=names(nasdaq))
graph.corr = delete.vertices(graph.corrL, which(degree(graph.corrL) < 1))

degree(graph.corr)
table(degree(graph.corr))
neighbors(graph.corr, v=c('EXPE'))

# setting seed so as to make the layout reproducible
set.seed(9143)

# Creating a layout object
layout1 <- layout.fruchterman.reingold(graph.corr, niter=500) 

V(graph.corr)[degree(graph.corr) <= 2]$color <- "aquamarine3"
V(graph.corr)[degree(graph.corr) == 3]$color <- "coral2"
V(graph.corr)[degree(graph.corr) == 4]$color <- "cyan3"
V(graph.corr)[degree(graph.corr) == 5]$color <- "lightskyblue2"
V(graph.corr)[degree(graph.corr) >= 6]$color <- "olivedrab3"

V(graph.corr)$label.color <- 'black'

# Edge width and Color
E(graph.corr)$width <- 4
E(graph.corr)$color <- "black"

#Plotting 
plot(graph.corr, layout=layout1, vertex.size=10,
     vertex.label.cex = 0.9, vertex.label.degree = 2, vertex.label=names(nasdaq))

legend("left", inset=1.0, title="Node Color Scheme",
       c("Deg <= 2","Deg = 3","Deg = 4","Deg = 5","Deg >= 6"), 
       fill=c('aquamarine3','coral2','cyan3','lightskyblue2', 'olivedrab3'), horiz=FALSE, box.lty=0, cex=0.7)

# title("Nasdaq100 Network Graph: Fruchterman Reingold Layout")

# viola plot for pcorr graph
nv <- vcount(g.corr)
ncn <- numeric()
A<-get.adjacency(g.corr)

#Find the number of common neighbors for each pair of nodes in the g.huge network
for(i in (1:(nv-1))) {
  ni <- neighborhood(g.corr, 1, i)
  nj <- neighborhood(g.corr, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"), col='darkcyan')
title(ylab="Number of Common Neighbors")

#######################
### Network Plot: 2 ###
#######################
# Benjamini-Hochberg adjustment used to control for the false discovery rate;
# and a threshold of p < 0.05 used to identify statistically significant partial
# correlations. 

graph.pcorrL <- set.vertex.attribute(g.pcorr, "name", value=names(nasdaq))
graph.pcorr = delete.vertices(graph.pcorrL, which(degree(graph.pcorrL) < 1))

degree(graph.pcorr)
table(degree(graph.pcorr))
neighbors(graph.pcorr, v=c('AMZN'))

# setting seed so as to make the layout reproducible
set.seed(9143)  

# Creating a layout object
layout1 <- layout.fruchterman.reingold(graph.pcorr, niter=500) 

V(graph.pcorr)[degree(graph.pcorr) == 1]$color <- "aquamarine3"
V(graph.pcorr)[degree(graph.pcorr) == 2]$color <- "coral2"
V(graph.pcorr)[degree(graph.pcorr) == 3]$color <- "cyan3"
V(graph.pcorr)[degree(graph.pcorr) == 4]$color <- "lightskyblue2"
V(graph.pcorr)[degree(graph.pcorr) == 6]$color <- "olivedrab3"


V(graph.pcorr)$label.color <- 'black'

# Edge width and Color
E(graph.pcorr)$width <- 4
E(graph.pcorr)$color <- "black"

#Plotting 
plot(graph.pcorr, layout=layout1, vertex.size=10,
     vertex.label.cex = 0.5, vertex.label.degree = 2)

legend("left", inset=1.0, title="Node Color Scheme",
       c("Deg = 1","Deg = 2","Deg = 3","Deg = 4","Deg = 6"), 
       fill=c('aquamarine3','coral2','cyan3','lightskyblue2', 'olivedrab3'), horiz=FALSE, box.lty=0, cex=0.7)

E(graph.pcorr)
V(graph.pcorr)
## Viola Plot for p_corr 0.01 graph 
nv <- vcount(g.pcorr)
ncn <- numeric()
A<-get.adjacency(g.pcorr)

#Find the number of common neighbors for each pair of nodes in the g.huge network
for(i in (1:(nv-1))) {
  ni <- neighborhood(g.pcorr, 1, i)
  nj <- neighborhood(g.pcorr, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"), col='darkcyan')
title(ylab="Number of Common Neighbors")

#######################
### Network Plot: 3 ###
#######################
# Benjamini-Hochberg adjustment used to control for the false discovery rate;
# and a threshold of p < 0.01 used to identify statistically significant partial
# correlations. 

graph.pcorr.01L <- set.vertex.attribute(g.pcorr.01, "name", value=names(nasdaq))
graph.pcorr.01 = delete.vertices(graph.pcorr.01L, which(degree(graph.pcorr.01L) < 1))

E(graph.pcorr.01)
V(graph.pcorr.01)

degree(graph.pcorr.01)
table(degree(graph.pcorr.01))
neighbors(graph.pcorr.01, v=c('AMZN'))

# setting seed so as to make the layout reproducible
set.seed(9143)  

# Creating a layout object
layout1 <- layout.fruchterman.reingold(graph.pcorr.01, niter=500) 

V(graph.pcorr.01)[degree(graph.pcorr.01) == 1]$color <- "aquamarine3"
V(graph.pcorr.01)[degree(graph.pcorr.01) == 2]$color <- "coral2"
V(graph.pcorr.01)[degree(graph.pcorr.01) == 3]$color <- "lightskyblue2"
V(graph.pcorr.01)[degree(graph.pcorr.01) == 4]$color <- "olivedrab3"

V(graph.pcorr.01)$label.color <- 'black'

# Edge width and Color
E(graph.pcorr.01)$width <- 4
E(graph.pcorr.01)$color <- "black"

#Plotting 
plot(graph.pcorr.01, layout=layout1, vertex.size=11,
     vertex.label.cex = 0.7, vertex.label.degree = 2)
legend("left", inset=1.0, title="Node Color Scheme",
       c("Deg = 1","Deg = 2","Deg = 3","Deg = 4"), 
       fill=c('aquamarine3','coral2','lightskyblue2', 'olivedrab3'), horiz=FALSE, box.lty=0, cex=0.7)

#viola plot

nv <- vcount(g.pcorr.01)
ncn <- numeric()
A<-get.adjacency(g.pcorr.01)

#Find the number of common neighbors for each pair of nodes in the  network
for(i in (1:(nv-1))) {
  ni <- neighborhood(g.pcorr.01, 1, i)
  nj <- neighborhood(g.pcorr.01, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"), col='darkcyan')
title(ylab="Number of Common Neighbors")



#######################
### Network Plot: 4 ###
#######################
# FDR tool library used to adjust for false discovery rate; and threshold of 
# p < 0.05 used to identify statistically significant partial correlations.

graph.pcorr.2L <- set.vertex.attribute(g.pcorr.2, "name", value=names(nasdaq))
graph.pcorr.2 = delete.vertices(graph.pcorr.2L, which(degree(graph.pcorr.2L) < 1))

degree(graph.pcorr.2)
table(degree(graph.pcorr.2))
neighbors(graph.pcorr.2, v=c('AMZN'))

# setting seed so as to make the layout reproducible
set.seed(9143)  

# Creating a layout object
layout1 <- layout.fruchterman.reingold(graph.pcorr.2, niter=500) 

V(graph.pcorr.2)[degree(graph.pcorr.2) == 1]$color <- "aquamarine3"
V(graph.pcorr.2)[degree(graph.pcorr.2) == 2]$color <- "coral2"
V(graph.pcorr.2)[degree(graph.pcorr.2) == 3]$color <- "cyan3"
V(graph.pcorr.2)[degree(graph.pcorr.2) == 4]$color <- "lightskyblue2"
V(graph.pcorr.2)[degree(graph.pcorr.2) == 5]$color <- "olivedrab3"


V(graph.pcorr.2)$label.color <- "black"

# Edge width and Color
E(graph.pcorr.2)$width <- 4
E(graph.pcorr.2)$color <- "black"

#Plotting 
plot(graph.pcorr.2, layout=layout1, vertex.size=10,
     vertex.label.cex = 0.5, vertex.label.degree = 2)

legend("left", inset=1.0, title="Node Color Scheme",
       c("Deg = 1","Deg = 2","Deg = 3","Deg = 4","Deg = 5"), 
       fill=c('aquamarine3','coral2','cyan3','lightskyblue2', 'olivedrab3'), horiz=FALSE, box.lty=0, cex=0.7)

V(graph.pcorr.2)
E(graph.pcorr.2)
#viola
nv <- vcount(g.pcorr.2)
ncn <- numeric()
A<-get.adjacency(g.pcorr.2)

#Find the number of common neighbors for each pair of nodes in the g.huge network
for(i in (1:(nv-1))) {
  ni <- neighborhood(g.pcorr.2, 1, i)
  nj <- neighborhood(g.pcorr.2, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"), col='darkcyan')
title(ylab="Number of Common Neighbors")

#######################
### Network Plot: 5 ###
#######################
# HUGE procedure used for predicted links.

graph.hugeL <- set.vertex.attribute(g.huge, "name", value=names(nasdaq))
graph.huge = delete.vertices(graph.hugeL, which(degree(graph.hugeL) < 1))

degree(graph.huge)
table(degree(graph.huge))
neighbors(graph.huge, v=c('AMZN'))

# setting seed so as to make the layout reproducible
set.seed(9143)  

# Creating a layout object
layout1 <- layout.fruchterman.reingold(graph.huge, niter=500)

V(graph.huge)[degree(graph.huge) == 1]$color <- "aquamarine3"
V(graph.huge)[degree(graph.huge) == 2]$color <- "coral2"
V(graph.huge)[degree(graph.huge) == 3]$color <- "cyan3"
V(graph.huge)[degree(graph.huge) == 4]$color <- "lightskyblue2"
V(graph.huge)[degree(graph.huge) >= 5]$color <- "olivedrab3"

V(graph.huge)$label.color <- 'black'

# Edge width and Color
E(graph.huge)$width <- 4
E(graph.huge)$color <- "black"

#Plotting 
plot(graph.huge, layout=layout1, vertex.size=10,
     vertex.label.cex = 0.5, vertex.label.degree = 2)

legend("left", inset=1.0, title="Node Color Scheme",
       c("Deg = 1","Deg = 2","Deg = 3","Deg = 4","Deg >= 5"), 
       fill=c('aquamarine3','coral2','cyan3','lightskyblue2', 'olivedrab3'), horiz=FALSE, box.lty=0, cex=0.7)

#Viola Plot : graph created by huge
nv <- vcount(g.huge)
ncn <- numeric()
A <- huge.opt$refit

#Find the number of common neighbors for each pair of nodes in the g.huge network
for(i in (1:(nv-1))) {
  ni <- neighborhood(g.huge, 1, i)
  nj <- neighborhood(g.huge, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"), col='darkcyan')
title(ylab="Number of Common Neighbors")


