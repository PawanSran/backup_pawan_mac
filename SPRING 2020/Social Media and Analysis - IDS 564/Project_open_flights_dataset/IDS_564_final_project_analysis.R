# Clearing everything out of memory
rm(list=ls()) 

# load routes file
routes_orig <- read.csv("~/Downloads/Masters/SPRING 2020/Social Media and Analysis - IDS 564/Project_open_flights_dataset/routes.dat", header= FALSE)

#setting the heading for data 

# from: source airport , to: destination airport , codeshare: "Y" if this flight is a codeshare (that is, not operated by Airline, but another carrier), empty otherwise.
# from_airport_id and to_airport_id : mapping to airport data
# equipment: 3-letter codes for plane type(s) generally used on this flight, separated by spaces 
col_headings <- c('airline', 'airline_ID', 'from' , 'from_airport_id','to', 'to_airport_ID', 'Codeshare',
                  'stops', 'Equipment')

names(routes_orig) <- col_headings

#save a copy to work on
temp_routes <- routes_orig

#number of rows/edges (routes)
nrow(temp_routes)

#number of unique routes 
nrow(unique(temp_routes[,c("from", "to")]))

# number of unique source airports in the route list 
length(unique(temp_routes$from))

#number of unique destination airports in route list
length(unique(temp_routes$to))


# there are duplicate routes in data , aggregating by frequency 
routes <- aggregate(numeric(nrow(temp_routes)), temp_routes[c("from", "to")], length)

col_head <- c("from", "to", "weight")
names(routes) <- col_head

# we will create graph to show the routes with given frequency
library(igraph)

net_routes <- graph.data.frame(routes)

#Number of egdges and vertices
vertices_routes = V(net_routes)
length(vertices_routes)

edges_routes = E(net_routes)       
length(edges_routes)

colours <- c("lightblue")

V(net_routes)["ALG"]$color

#V(net_routes)$color <- ifelse(V(net_routes)$name == c('ALG'), "RED", colours)
V(net_routes)$color<-"lightblue"

V(net_routes)["ALG"]$color<-"red"

V(net_routes)$color
#plotting the network graph of routes 
plot.igraph(net_routes,vertex.label=NA, vertex.color=V(net_routes)$color,
            vertex.size=6, edge.arrow.size=.3, edge.color="blue",
            edge.width=edges_routes$weight/10,
            edge.arrow.width=0.5, edge.curved=.1)

# Other analysis on Routes used by different airline
hist(routes$weight)
mean(routes$weight)
median(routes$weight)
sd(routes$weight)

# set a cut-off
mean_cut_off <- mean(routes$weight)

#drop edges above that cut off
new_network_sp <- delete.edges(net_routes, E(net_routes)[weight>mean_cut_off])

#plot new network
plot.igraph(new_network_sp, vertex.label=NA, 
            vertex.color="light blue", vertex.size=5, 
            edge.arrow.size=.4, edge.color="black",
            edge.width=E(new_network_sp)$weight/10,edge.arrow.width=0.5,
            edge.curved=.1, main="Network Flight Routes, with cut-off")


# There are many nodes with degree zero in above graph - we can filter on degree at least greater than 1 

#some analysis done in advanced lab-1 (on basic network features , centraility of nodes etc)
# Now we will calculate the edge density
ecount(net_routes)/(vcount(net_routes)*(vcount(net_routes)-1))

#reciprocity
reciprocity(net_routes)

#transitivity
triad_census(net_routes)


#Diameter of the network
diameter(net_routes, directed=T, weights=NA)

get_diameter(net_routes, directed=T)


# calculating node degrees
deg <- degree(net_routes, mode="all")

#top 15 nodes with high degree
sort(deg, decreasing=TRUE)[1:15]

#in degree - top 15
sort(degree(net_routes, mode="in"), decreasing=TRUE)[1:15]

# out degree - top 15
sort(degree(net_routes, mode="out"), decreasing=TRUE)[1:15]

# weighted degree - top 15
graph_temp <- graph.data.frame(routes)
sort(degree(graph_temp, mode="all"), decreasing=TRUE)[1:15]

#weighted in - top 15 
sort(degree(graph_temp, mode="in"), decreasing=TRUE)[1:15]

#weighted out - top 15 
sort(degree(graph_temp, mode="out"), decreasing=TRUE)[1:15]


### ### ### ### ### ### ### ### ### ### ### 
# Histogram plot for node degree
### ### ### ### ### ### ### ### ### ### ### 

hist(deg, breaks=1:vcount(net_routes)-1, main="Histogram of node degree")

# Degree distribution
deg_dist <- degree_distribution(net_routes, cumulative=T, mode="all")

plot( x=0:max(deg), y=deg_dist, pch=19, cex=1.2, col="aquamarine3",
      xlab="Degree", ylab="Cumulative Frequency", main="Degree Distribution")

plot( x=0:max(deg), y=deg_dist, pch=19, cex=1.2, col="aquamarine3", 
      log="y", xlab="Degree", ylab="Cumulative Frequency (logarithmic)", main="Degree Distribution (log)")


### ### ### ### ### ### ### ### ### ### ### 
# Calculate Centralities 
### ### ### ### ### ### ### ### ### ### ### 

degree(net_routes, mode="in")

centr_degree(net_routes, mode="in", normalized=T)

### ### ### ### ### ### ### ### ### ### ### 
#$centralization
#[1] 0.06630355

#$theoretical_max
#[1] 11727200
### ### ### ### ### ### ### ### ### ### ### 

# Closeness centraility
close_center <- closeness(net_routes, mode="all", weights=NA)

sort(close_center, decreasing=TRUE)[1:15]

centr_clo(net_routes, mode="all", normalized=T)

### ### ### ### ### ### ### ### ### ### ### 
#$centralization
#[1] 0.003748188

#$theoretical_max
#[1] 1711.75
### ### ### ### ### ### ### ### ### ### ### 

#top 15 - degree
degree(net_routes, v=c("FRA","CDG","LHR","AMS", "DXB", 
                "LAX" , "JFK" , "YYZ", "IST", "ORD",
                "PEK", "MUC", "FCO", "NRT", "EWR"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#FRA CDG LHR AMS DXB LAX JFK YYZ IST ORD PEK MUC FCO NRT EWR 
#477 470 342 463 370 297 322 293 457 409 412 380 316 206 305 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Eigen centraility

eigen_centrality(net_routes, directed=T, weights=NA)

sort(eigen_centrality(net_routes, directed=T, weights=NA)$vector, decreasing=TRUE)[1:15]

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#AMS       FRA       CDG       MUC       LHR       FCO       IST       BCN 
#1.0000000 0.9989855 0.9596674 0.8978758 0.8259622 0.8189349 0.7814552 0.7802331 
#ZRH       MAD       BRU       DUB       DUS       LGW       VIE 
#0.7606253 0.7428368 0.7375816 0.7070025 0.6990770 0.6930084 0.6894696 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

eigen_centrality(net_routes, directed=T, weights=NA)$value
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#[1] 69.28393
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

centr_eigen(net_routes, directed=T, normalized=T)$value
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#[1] 69.28393
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# check degree level of top 15 eigen-vector
degree(net_routes, v=c("AMS","FRA","CDG","MUC", "LHR",
                "FCO" , "IST" , "BCN", "ZRH", "MAD",
                "BRU", "DUB", "DUS", "LGW", "VIE"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#AMS FRA CDG MUC LHR FCO IST BCN ZRH MAD BRU DUB DUS LGW VIE 
#463 477 470 380 342 316 457 326 273 314 293 288 294 330 275 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


# Betweeness centrality
net_route_between <- betweenness(net_routes, directed=T, weights=NA)

edge_bet_net_route <- edge_betweenness(net_routes, directed=T)

center_bet_net_route <- centr_betw(net_routes, directed=T, normalized=T)

center_bet_net_route

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#$centralization
#[1] 0.06933109

#$theoretical_max
#[1] 40130485248
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# check degree level of top 15 betweenness
degree(net_routes, v=c("ANC","LAX","CDG","DXB", "FRA",
                "PEK" , "ORD" , "SEA", "AMS", "YYZ",
                "IST", "GRU", "LHR", "NRT", "SYD"))
#ANC LAX CDG DXB FRA PEK ORD SEA AMS YYZ IST GRU LHR NRT SYD 
#68 297 470 370 477 412 409 184 463 293 457 182 342 206 168 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#clustering
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Hub and Authority score
hs_routes <- hub_score(net_routes)$vector
as_routes <- authority_score(net_routes)$vector

# Hubs and Authorities
#par(mfrow=c(1,2))

plot(net_routes, vertex.size=hs_routes*50, 
     vertex.color="light blue",vertex.label=NA, 
     edge.arrow.size=.4, edge.color="black",
     edge.width=E(net_routes)$weight/10,
     edge.arrow.width=0.5, edge.curved=.1, main="Hubs")

plot(net_routes, vertex.size=as_routes*30, vertex.color="light blue",
     vertex.label=NA, edge.arrow.size=.4, edge.color="black",
     edge.width=E(net_routes)$weight/10,edge.arrow.width=0.5, edge.curved=.1, 
     main="Authorities")


# Distance and Paths 

# Mean Distance 
mean_distance(net_routes, directed=T)
## [1] 4.146205

distances(net_routes)

## Sub-group and communities
net_route_sub <- as.undirected(net_routes, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))

# components ( clusters )
scc_cl <- clusters(net_routes, mode="strong")

V(net_routes)$color <- rainbow(scc_cl$no)[scc_cl$membership]

plot.igraph(net_routes, mark.groups = split(1:vcount(net_routes), scc_cl$membership))

sort(scc_cl$membership, decreasing=TRUE)[1:15]

#KYK KLN KZB AOS KPR SYB KKB FMI KOO CZJ PVE TUA UII BSS ORX 
#44  43  42  41  40  39  38  37  36  35  34  33  32  31  30 

# Identifying biggest component
gs_route <- induced.subgraph(net_routes, scc_cl$membership==order(-scc_cl$csize)[1])

plot(gs_route, vertex.size=as_routes*30, vertex.color="light blue",vertex.label=NA, edge.arrow.size=.4, 
     edge.color="black",edge.width=E(net_routes)$weight/10,edge.arrow.width=0.5, edge.curved=.1)

# articulation points

net2_route <- net_routes


V(net2_route)$color = "black"

articulation.points(net2_route)
V(net2_route)[articulation.points(net2_route)]$color = "red"

plot.igraph(net2_route, vertex.size=3, vertex.label=NA,
            edge.arrow.width=0.3, edge.color="aquamarine4", 
            edge.width = 0.1, edge.curved=.1, 
            main="Articulation Points")


# For strongest component

gs2_route <- gs_route
V(gs2_route)$color = "black"
articulation.points(gs2_route)   # 358 nodes
V(gs2)[articulation.points(gs2)]$color = "red"

plot.igraph(gs2, vertex.size=3, vertex.label=NA,edge.arrow.width=0.3, 
            edge.color="light blue", edge.width = 0.1, edge.curved=.1, 
            main="Articulation Points")


# Assortativity and Homophily
routes_new <- routes
x <- ifelse(routes_new$weight < 5, "periferic", ifelse((routes_new$weight>=5) & (routes_new$weight<10),"small", "large"))
routes_new$type <- x
net2_route_new <- graph.data.frame(routes_new)
net2_route_new

assortativity_degree(net_routes, directed=T)
#[1] -0.01044319



########################################
#DIFFERENT LAYOUTS
########################################
library(igraph)

# Create the directed graph object
g_Sub=graph.data.frame(routes_orig, directed = TRUE, vertices= NULL)

is.simple(g_Sub)

# Create edge weights
E(g_Sub)$weight <-1

E(g_Sub)$weight 

g_Sub_simpl<-simplify(g_Sub)
is.simple(g_Sub_simpl)


# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_Sub_simpl)$weight  + 1)
num_weight<-E(g_Sub_simpl)$weight 
length(inv_weight)
E(g_Sub_simpl)$weight <-inv_weight


# plot the simple graph:
plot(g_Sub_simpl, vertex.label= NA,
     edge.arrow.size=0.7,
     main="plot for simplified graph")

plot(g_Sub_simpl,vertex.label=V(g_Sub_simpl)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,
     vertex.label.cex = .75,asp = 0.5,margin = -0.2)


#fruchterman layout

l <- layout.fruchterman.reingold(g_Sub_simpl, dim =3)
plot(g_Sub_simpl, layout=l,vertex.label=NA,main="fruchterman.reingold Algorithm")

plot(g_Sub_simpl,vertex.label= NA,vertex.size=3,layout = l,edge.width = 5, edge.arrow.size = 0.6,vertex.size2 = 6,margin = -0.2)

# 2 : Trying in circle layout - Not visibily interpreatable
l <- layout.circle(g_Sub_simpl)
plot(g_Sub_simpl, layout=l,vertex.label=NA,main="Circle Layout")


# walktrap 
comm_walk <- walktrap.community(g_Sub_simpl, weights=E(g_Sub_simpl)$weight)
plot(comm_walk,g_Sub_simpl, vertex.label= NA, vertex.size=3, 
     edge.width =1, edge.arrow.size = 0.6,) + title(main = "Walktrap Algorithm")

E(g_Sub_simpl)$weight <- inv_weight

#connected analysis
is.connected(g_Sub_simpl)
is.connected(g_Sub_simpl, mode="strong")
is.connected(g_Sub_simpl, mode="weak")
#all false



