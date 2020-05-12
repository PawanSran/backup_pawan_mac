getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-'/Users/pawanjeetkaur/Downloads/SPRING 2020/Social Media and Analysis - IDS 564/Lab-3'
setwd(dir_path)

# clear everything out of memory
rm(list=ls())  

# Load primary school data, contact data
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

g_primschool=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

# Edges
ecount(g_primschool)
## Vertices
vcount(g_primschool)
is.weighted(g_primschool)

V(g_primschool)$name
E(g_primschool)$weight
V(g_primschool)$gender
V(g_primschool)[V(g_primschool)$classname=="1B"]

is.simple(g_primschool)
is.connected(g_primschool)

# http://igraph.wikidot.com/community-detection-in-r
# "The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Let us call the edges within a 
# community "internal" and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges 
# incident to a vertex of the community. More internal than external edges show that the community is significant; less 
# internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by 
# this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community."
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")

# Does edge weight make any difference here?

# Community detection using the Fast Greedy Algorithm
school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)
c.m <- membership(school_comm_fast)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m, stud.class, useNA = c("no"))

# Here, we are testing community significance in fast greedy. 
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
v_comp3 <- V(g_primschool)[c.m==3]
v_comp4 <- V(g_primschool)[c.m==4]
v_comp5 <- V(g_primschool)[c.m==5]
v_comp6 <- V(g_primschool)[c.m==6]
v_comp7 <- V(g_primschool)[c.m==7]
community.significance.test(g_primschool, v_comp1)
#significant
community.significance.test(g_primschool, v_comp2)
#not-significant
community.significance.test(g_primschool, v_comp3)
#significant
community.significance.test(g_primschool, v_comp4)
#not-significant
community.significance.test(g_primschool, v_comp5)
#significant
community.significance.test(g_primschool, v_comp6)
#not-significant
community.significance.test(g_primschool, v_comp7)
#not-significant

#plot for fastgreedy algorithm
plot(school_comm_fast,g_primschool, vertex.label= NA, vertex.size=3) + title(main = "Fast Greedy Algorithm") 

# walktrap :
school_comm_walk <- walktrap.community(g_primschool, weights=E(g_primschool)$weight)
c.m_1 <- membership(school_comm_walk)
table(c.m_1, stud.class, useNA = c("no"))

v_walk_comp1 <- V(g_primschool)[c.m_1==1]
v_walk_comp2 <- V(g_primschool)[c.m_1==2]
v_walk_comp3 <- V(g_primschool)[c.m_1==3]
v_walk_comp4 <- V(g_primschool)[c.m_1==4]
v_walk_comp5 <- V(g_primschool)[c.m_1==5]
v_walk_comp6 <- V(g_primschool)[c.m_1==6]
v_walk_comp7 <- V(g_primschool)[c.m_1==7]
v_walk_comp8 <- V(g_primschool)[c.m_1==8]
v_walk_comp9 <- V(g_primschool)[c.m_1==9]
community.significance.test(g_primschool, v_walk_comp1)
#significant
community.significance.test(g_primschool, v_walk_comp2)
#significant
community.significance.test(g_primschool, v_walk_comp3)
#not-significant
community.significance.test(g_primschool, v_walk_comp4)
#not-significant
community.significance.test(g_primschool, v_walk_comp5)
#not-significant
community.significance.test(g_primschool, v_walk_comp6)
#not-significant
community.significance.test(g_primschool, v_walk_comp7)
#not-significant
community.significance.test(g_primschool, v_walk_comp8)
#not-significant
community.significance.test(g_primschool, v_walk_comp9)
#not-significant

plot(school_comm_walk,g_primschool, vertex.label= NA, vertex.size=3) + title(main = "Walktrap Algorithm")


#springlass 
school_comm_sping <- spinglass.community(g_primschool, weights=E(g_primschool)$weight)
c.m_2 <- membership(school_comm_sping)

table(c.m_2, stud.class, useNA = c("no"))

plot(school_comm_sping, g_primschool, vertex.label= NA, vertex.size=1) + title(main = "Spinglass Algorithm")

#label prop
school_comm_label = cluster_label_prop(g_primschool, weights = E(g_primschool)$weight)
c.m_3 <- membership(school_comm_label)

table(c.m_3, stud.class, useNA = c("no"))

plot(school_comm_label, g_primschool, vertex.label= NA, vertex.size=1) + title(main = "Label-Propagation Algorithm")

# Here, we are testing community significance for just two of the communities. Students will complete tests for the remainder of communities for each algorithm. 
v_comp1 <- V(g_primschool)[c.m==1]
v_comp2 <- V(g_primschool)[c.m==2]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)

# Students will produce similar plots for the walktrap, spinglass, and label propagation algorithms for community detection
plot(school_comm_fast,g_primschool, vertex.label= NA, vertex.size=2)

# Girvan Newman
school_comm_edge <- edge.betweenness.community(g_primschool, weights=E(g_primschool)$weight)
Girvan_size <- sizes(school_comm_edge)
c.m.edge <- membership(school_comm_edge)

table(c.m.edge, stud.class, useNA = c("no"))
table(c.m.edge, stud.gender, useNA = c("no"))

plot(school_comm_edge, g_primschool, vertex.edge= NA, vertex.size=1)+ title(main = "Girvan Newman Algorithm")

#question 8
g <- graph.formula(A-B,B-C,C-D, D-E, E-A)
E(g)$sign<-c(+1,-1, -1, +1, 1)
plot(g)

#question 9
g <- graph.formula(A-B,A-C,A-D, B-C, B-D, C-D )
E(g)$sign<-c(+1,1, -1, 1, -1, 1)
plot(g)


#question 10:

# In the igraph help, online documentation or KC book, students will find the function calls for the walktrap, spinglass, and label propagation algorithms 
# Why are the benefits and drawbacks of the Girvan-Newman algorithm for community detection? Hint: try it in igraph

# Consider students in first grade and 5th grade. To what extent does community structure indicate that students segregate by gender in these two grades?
# Use the Fast Greedy algorithm for analysis.
v_grade1students<-V(g_primschool)[V(g_primschool)$classname=="1B" | V(g_primschool)$classname=="1A"]
v_grade5students<-V(g_primschool)[V(g_primschool)$classname=="5B" | V(g_primschool)$classname=="5A"]

subgraph_grade1<-induced_subgraph(g_primschool, v_grade1students)
stud.gender1<- get.vertex.attribute(subg_grade1, "gender")

subgraph_grade5<-induced_subgraph(g_primschool, v_grade5students)
stud.gender2<- get.vertex.attribute(subg_grade5, "gender")


school_comm_grade_1 <- fastgreedy.community(subgraph_grade1, weights=E(subgraph_grade1)$weight)
c.m_4 <- membership(school_comm_grade_1)


school_comm_grade_2 <- fastgreedy.community(subgraph_grade5, weights=E(g_primschool)$weight)
c.m_5 <- membership(school_comm_grade_2)

plot(school_comm_grade_1, subg_grade1, vertex.label= NA, vertex.size=2)

table(c.m_4, stud.gender1, stud.class_1, useNA = c("no"))

table(c.m_5, stud.gender2,stud.class_1, useNA = c("no"))


