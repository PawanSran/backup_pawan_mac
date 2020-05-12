# SAND with R, chapter7.tex

# CHUNK 1
library(sand)
fblog<-upgrade_graph(fblog)

plot(fblog)

nv <- vcount(fblog)
ncn <- numeric()
A <- get.adjacency(fblog)

# Find the number of common neighbors for each pair of nodes in the fblog network
for(i in (1:(nv-1))) {
  ni <- neighborhood(fblog, 1, i)
  nj <- neighborhood(fblog, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

# CHUNK 2
#install.packages(vioplot)
library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"), col="Violet")
title(ylab="Number of Common Neighbors")

# CHUNK 3
library(ROCR)
pred <- prediction(ncn, Avec)
perf <- performance(pred, "auc")
slot(perf, "y.values")
# ---
## [[1]]
## [1] 0.9275179
# ---

# CHUNK 4
rm(list=ls())
data(Ecoli.data)
ls()
# ---
## [1] "Ecoli.expr" "regDB.adj"
# ---

# CHUNK 5
heatmap(scale(Ecoli.expr), Rowv=NA)

# CHUNK 6
library(igraph)
g.regDB <- graph.adjacency(regDB.adj, "undirected")
summary(g.regDB)
# ---
## IGRAPH UN-- 153 209 -- 
## attr: name (v/c)
# ---

# CHUNK 7
plot(g.regDB, vertex.size=3, vertex.label=NA)


# Note the code of CHUNK 8 THROUGH CHUNK 13 uses the full correlation matrix to predict edges in the network
# CHUNK 8
mycorr <- cor(Ecoli.expr)

# CHUNK 9
# Fisher's transformation
z <- 0.5 * log((1 + mycorr) / (1 - mycorr))

# CHUNK 10
z.vec <- z[upper.tri(z)]
n <- dim(Ecoli.expr)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0, 
                        sqrt(1 / (n-3)), lower.tail=FALSE)

# CHUNK 11
length(corr.pvals)
# ---
## [1] 11628
# ---

# CHUNK 12
# Benjamini-Hochberg adjustment to control for the false discovery rate
hist(mycorr)
hist(z.vec)
corr.pvals.adj <- p.adjust(corr.pvals, "BH")

# CHUNK 13
# Number of edges predicted: using statistical significance at the p < 0.05 threshold
length(corr.pvals.adj[corr.pvals.adj < 0.05])
pmean <- mean(corr.pvals.adj)
pmean
## [1] 5227
# ---


# CHUNK 14
library(fdrtool)

# CHUNK 15
mycorr.vec <- mycorr[upper.tri(mycorr)]
fdr <- fdrtool(mycorr.vec, statistic="correlation")

# Note the code of CHUNK 16 through CHUNK 19 uses partial correlations to predict edges
# CHUNK 16
pcorr.pvals <- matrix(0, dim(mycorr)[1], 
                      dim(mycorr)[2])
for(i in seq(1, 153)){
  for(j in seq(1, 153)){
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

# CHUNK 17
pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pco_ed<-(pcorr.pvals.vec<0.05)
length(pcorr.pvals.vec[pco_ed])

# Benjamini-Hochberg adjustment to control for the false discovery rate
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")
plot(pcorr.pvals.adj)

# CHUNK 18
pcorr.edges <- (pcorr.pvals.adj < 0.05)
length(pcorr.pvals.adj[pcorr.edges])
pmean1 <- mean(pcorr.pvals.adj)
pmean1
# ---
## [1] 25
# ---

# CHUNK 19
# Create the graph predicted by the statistically significant partial correlations
pcorr.A <- matrix(0, 153, 153)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

# CHUNK 20
# Find overlap between the graph predicted by partial correlations with the known gene linkages uncovered in prior research (from g.regDB)
str(graph.intersection(g.regDB, g.pcorr, byname=FALSE))
# ---
## IGRAPH UN-- 153 4 -- 
## + attr: name (v/c)
## + edges (vertex names):
## [1] yhiW_b3515_at--yhiX_b3516_at
## [2] rhaR_b3906_at--rhaS_b3905_at
## [3] marA_b1531_at--marR_b1530_at
## [4] gutM_b2706_at--srlR_b2707_at
# ---

# CHUNK 21
# FDR tool can also be used to adjust for false discovery rate and predict new edges based on partial correlations
fdr <- fdrtool(pcorr.pvals.vec, statistic="pvalue", 
               plot=FALSE)
pcorr.edges.2 <- (fdr$qval < 0.05)
length(fdr$qval[pcorr.edges.2])
# ---
## [1] 25
# ---

library(huge)

set.seed(42)
huge.out <- huge(Ecoli.expr)

# CHUNK 23
huge.opt <- huge.select(huge.out, criterion="ric")
summary(huge.opt$refit)
# ---
## 153 x 153 sparse Matrix of class "dsCMatrix", with 0 entries 
## [1] i j x
## <0 rows> (or 0-length row.names)
# ---

# CHUNK 24
huge.opt <- huge.select(huge.out, criterion="stars")
g.huge <- graph.adjacency(huge.opt$refit, "undirected")
summary(g.huge)

# HUGE (High-dimensional undirected graph estimation library) procedure for predicted links
# CHUNK 22
#install.packages("huge")

nv <- vcount(g.huge)
ncn <- numeric()
A <- get.adjacency(g.huge)

# Find the number of common neighbors for each pair of nodes in the g.huge network

for(i in (1:(nv-1))){
  ni <- neighborhood(g.huge, 1, i)
  nj <- neighborhood(g.huge, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

# CHUNK 2
#install.packages("vioplot")
library(vioplot)
?vioplot
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"))
title(ylab="Number of Common Neighbors")


# ---
## IGRAPH U--- 153 759 --
# ---

# CHUNK 25
# Find overlap between the graph produced by the partial correlations with graph produced by HUGE library
str(graph.intersection(g.pcorr, g.huge))
# ---
## IGRAPH U--- 153 25 -- 
## + edges:
##  [1] 145--146 144--146 112--125 112--113 109--138
##  [6] 108--135  97--111  96--119  92--107  87--104
## [11]  86-- 87  84--129  81--137  72--141  70-- 75
## [16]  60--127  46-- 77  42-- 43  38--153  37-- 98
## [21]  27--123  21-- 33  12--135   9-- 90   3-- 60
# ---

# CHUNK 26
# Find overlap between the graph predicted by HUGE library with the known gene linkages uncovered in prior research.
str(graph.intersection(g.regDB, g.huge, byname=FALSE))
# ---
## IGRAPH UN-- 153 22 -- 
## + attr: name (v/c)
## + edges (vertex names):
##  [1] yhiW_b3515_at--yhiX_b3516_at
##  [2] tdcA_b3118_at--tdcR_b3119_at
##  [3] rpoE_b2573_at--rpoH_b3461_at
##  [4] rpoD_b3067_at--tyrR_b1323_at
##  [5] rhaR_b3906_at--rhaS_b3905_at
##  [6] nac_b1988_at --putA_b1014_at
##  [7] marA_b1531_at--marR_b1530_at
##  [8] malT_b3418_at--rpoD_b3067_at
##  [9] hns_b1237_at --rcsB_b2217_at
## [10] hipA_b1507_at--hipB_b1508_at
## [11] himA_b1712_at--himD_b0912_at
## [12] gutM_b2706_at--srlR_b2707_at
## [13] fruR_b0080_at--mtlR_b3601_at
## [14] flhD_b1892_at--lrhA_b2289_at
## [15] crp_b3357_at --srlR_b2707_at
## [16] crp_b3357_at --pdhR_b0113_at
## [17] crp_b3357_at --oxyR_b3961_at
## [18] crp_b3357_at --malT_b3418_at
## [19] crp_b3357_at --galS_b2151_at
## [20] caiF_b0034_at--rpoD_b3067_at
## [21] caiF_b0034_at--hns_b1237_at 
## [22] arcA_b4401_at--lldR_b3604_at
# ---

fblog <- huge(fblog)
A <- get.adjacency(fblog)
nv <- vcount(fblog)
ncn <- numeric()

# Find the number of common neighbors for each pair of nodes in the fblog network
for(i in (1:(nv-1))){
  ni <- neighborhood(fblog, 1, i)
  nj <- neighborhood(fblog, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

# CHUNK 2
library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"))
title(ylab="Number of Common Neighbors")
