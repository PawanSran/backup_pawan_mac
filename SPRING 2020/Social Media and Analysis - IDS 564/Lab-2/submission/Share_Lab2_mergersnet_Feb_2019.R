#Read in the hs0 data over the internet using the read.table() function.
#getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
#dir_path <-"~/YourWorkingDirectoryFilePath"
#setwd(dir_path)
# clear everything out of memory
#rm(list=ls())  
infile<-"~/Downloads/SPRING 2020/Social Media and Analysis - IDS 564/Lab-2/MergerNet_Jan21_2016_forR.csv"
## Load package
library(igraph)
el=read.csv(infile, header = TRUE, sep = ",")
g_acq=graph.data.frame(el, directed = TRUE, vertices= NULL)

### List of all the years represented in the set
el[,"year"]
df <-data.frame(el)
class(df$weight)
# ---
#[1] "integer"
# ---

class(df$source)
# ---
# [1] "factor"
# ---

class(el)
# ---
# [1] "data.frame"
# ---

#Ques 1
# Edges
ecount(g_acq)
## Vertices
vcount(g_acq)

#Ques 2
t <-components(g_acq, mode= c("weak", "strong"))
count_components(g_acq, mode= c("strong"))
# 4 strong
count_components(g_acq, mode= c("weak"))
# 1 weak 

#Is it a simple graph? No!
## Check whether Self_loops exist, as do multiple edges
is.simple(g_acq)
# ---
#[1] FALSE
# ---
E(g_acq)$weight 
g_acq_simpl<-simplify(g_acq)
### The above should default to the option below, to sum the existing edge weights ### when combining them
##g_acq_simpl<-simplify(g_acq,edge.attr.comb="sum" )

E(g_acq_simpl)$weight 
# Will use the inverse of log weight for shortest path calculations
inv_weight<-1/log(E(g_acq_simpl)$weight  + 1)
num_weight<-E(g_acq_simpl)$weight 
length(inv_weight)

#Ques 3
diameter(g_acq_simpl , weights = inv_weight , directed = T)
diameter(g_acq_simpl , weights = inv_weight , directed = F)  

#Ques 4
df <- as.data.frame(betweenness(g_acq_simpl, v= V(g_acq_simpl) , directed= TRUE, weights = inv_weight, normalized = TRUE, nobigint = TRUE))
View(df)

# Remove disconnected components to create a strongly connected component. 
#We will use this component to calculate closeness and shortest path distances.
g_acq_scc <-g_acq_simpl - vertices('814', '925', '928')
inv_weight_scc<-1/log(E(g_acq_scc)$weight  + 1)

#Ques 5 6 
?closeness
closeness(g_acq_scc, vids = V(g_acq_scc), mode = "in", weights = inv_weight_scc)
closeness(g_acq_scc, vids = V(g_acq_scc), mode = "out", weights = inv_weight_scc)

#Ques7 8
?transitivity
loc <- transitivity(g_acq_simpl, type = "local", weights = inv_weight)
?mean
mean_loc <- mean(loc, na.rm = TRUE)
?sd
sd_loc <- sd(loc, na.rm = TRUE)
mean_loc
sd_loc
transitivity(g_acq_simpl, type = "global", weights = inv_weight)

#Ques 9
shortest.paths(g_acq_simpl,  to= V(g_acq_simpl)[68], weights = inv_weight, mode = "in")
shortest.paths(g_acq_simpl,  to= V(g_acq_simpl)[68], weights = inv_weight, mode = "out")


# induced subgraph
sub_net<-induced.subgraph(g_acq_simpl, v=c('511', '541',
                                           '518', '519', '517', '325', '423', '446', '512', '523',
                                           '561', '621', '115', '482', '485', '487', '491', '492',
                                           '521', '712' ))

inv_weight_1<-1/log(E(sub_net)$weight  + 1)

diameter(sub_net , weights = inv_weight_1 , directed = T)
diameter(sub_net , weights = inv_weight_1 , directed = F)  

?plot
V(sub_net)[c('511', '541', '518', '519')]$color<-c("red", "lightblue", "yellow", "aquamarine")
E(sub_net)$weight <- inv_weight_1

plot(sub_net, vertex.size= degree(sub_net)*0.9,
     layout=layout.fruchterman.reingold)

title("HW_2 Graph")






  