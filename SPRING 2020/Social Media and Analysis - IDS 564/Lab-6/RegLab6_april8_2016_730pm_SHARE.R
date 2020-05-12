###############################################################
# Ali Tafti
# This script uses and modifies code from http://www.tb.ethz.ch/education/learningmaterials/modelingcourse/level-2-modules/network.html


# clear everything out of memory
rm(list=ls())  

#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Set your working directory
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"~/YourWorkingDirectoryFilePath"
setwd(dir_path)
library(igraph)
################
# Function definition #
# Students: Please modify the function below according to the lab exercise instructions
################


# This one implements stats for susceptible as well as remove
# Also, make use of the susceptible again feature
# Please note that the only required argument for this function is the graph object. The other arguments can be overriden or skipped by the calling function; if skipped,
# the default values are used.
# Arguments: 1) network.i: graph object (required), 2) simlength: number of iterations (rounds) for the simulation, 3) p.t: probability of infecting a connected susceptible neighbor, if a node is infected
# 4) display_net: show the evolving network plots for each round of the simulation (press Enter to continue, q to quit). Set it to FALSE if you have a lot of 
# rounds and just want to collect the summary timestats. 5) removeafter: Number of rounds that an infected node can infect neigbors (infectious), 
# after which it moves to a Removed state where it is immune and not infectious. 5) susceptibleafter: Number of rounds after which a node in the Removed state becomes susceptible to infection again

simulate_sir <- function(network.i, simlength=15, p.t=0.2, display_net=TRUE, removeafter=2, susceptibleafter=10000) {
  
  links <-get.edgelist(network.i)
  N<- vcount(network.i)
  time_stats<-list()
  
  # Initialize time stats. 
  # Number of nodes in S, I, or R status in each round of time
  time_stats$infected_t<-rep(1,simlength)
  time_stats$removed_t<-rep(0,simlength)
  #susceptible is total that are not removed or infected
  time_stats$susceptible_t<-rep(N-1,simlength)
  
  # For advanced lab, you will also need to keep track of susceptible and removed status of each node   
  infected <- logical(N) # initialize infection status
  #   susceptible <- rep(TRUE, N) # initialize susceptible status
  #   removed<-logical(N)
  patientzero <- sample(N,1) # select 'patient zero'
  
  # Initialize a vector that keeps track of the time of infection for each node. For advanced lab, you will need to use this appropriately
  infected_time<-rep(0, N)
  # For advanced lab, you will also need to keep track of the number of time steps that nodes are in the removed state, so that you can make them susceptible again in due time 
  # removed_time<-rep(0, N)
  
  #patient zero  
  infected[patientzero] <- TRUE
  # For advanced lab, you will need to use this to keep track of which nodes are susceptible
  # susceptible[patientzero] <-FALSE
  
  # Used to count towards a removal; after a certain number of periods, the node will be immune (i.e. removed)
  infected_time[patientzero] <- 1
  
  if (N > 50) {
    V(network.i)$size <- 2
    V(network.i)$label <- ""
  }
  if (display_net) {
    
    fixlayout <- layout.kamada.kawai(network.i)  # store a fixed layout for the graph
    node.colour <- rep("SkyBlue2",N) # initialize node colours (SkyBlue2 is also the default node colour in igraph)
    node.colour[patientzero] <- "red" # infected nodes will be coloured red
    plot(network.i,layout=fixlayout, main="Time = 0", vertex.color=node.colour)
  }
  for (i in 1:simlength) {
    
    # Original spreading mechanism, that did not account for removed nodes
    # Advanced lab: Need to update this to consider removed (immune) or newly susceptible nodes
    discordant.links <- which(xor(infected[as.integer(links[,1])],infected[as.integer(links[,2])])) # find the indeces of links that connect an infected individual to an uninfected
    
    transmit <- rbinom(length(discordant.links),1,p.t) # determine randomly which of the discordant links transmit the disease
    
    # let me update the infection vector in three steps to make it easier to read:
    transmitter.links <- discordant.links[transmit==1]
    nodes.of.transmitter.links <- unique(as.vector(as.integer(links[transmitter.links,1:2]))) # gets both nodes of the transmitter links into a single vector; unique just filters out repetitions
    infected[nodes.of.transmitter.links] <- TRUE # here I simply set both nodes to TRUE (although the transmitter already had 'TRUE'). In more complex models, you might want to do a further check here and overwrite only the newly infected nodes.
    
    # At some point in this loop, you need to update the number infected, and for advanced lab, number removed and susceptible
    # For advanced lab, you need to update the time that nodes have been in infected and removed states about here in the code.    
    # Increment length of time infected, for those that are infected.

    # Also, keep track of when to make nodes susceptible again after a certain number of periods until the susceptibleafter setting

    if (display_net) {
      node.colour[infected] <- "red"
      # Make the removed points as yellow, susceptible points as skyblue. Uncomment these for advanced lab.
      #       node.colour[removed] <- "yellow"
      #       node.colour[susceptible] <-"SkyBlue2"
      input<-readline() # waits for the user to press <ENTER> before proceeding; you need to switch to the console to do this
      
      # TIP: Hit q to break the simulation between renderings of the network
      if (input=="q") {break}
      plot(network.i,layout=fixlayout, main=paste("Time =", i, " out of ", simlength), vertex.color=node.colour)
    }
    
    
    # Advanced lab: Around this point, you need to remove nodes that have been infected after time determined by removeafter variable.

    # Advanced lab: Around this point, you need to make certain nodes susceptible again if they have been in the removed state more than the time determined by susceptibleafter variable

    #Advanced lab: Around here, update the susceptible vector so that any node that is not infected and not removed is susceptible

    #Advanced lab: Around here, reset the infection times for nodes that have been infected for more than the time determined by the removeafter variable
    #Advanced lab: Around here, also reset the removed times for nodes that have been in the removed state for more than the time determined by susceptibleafter variable

    # Also, will need to make sure that infection status is FALSE (off) for nodes that are in the removed state 
  }  
  # time_stats is a list of three vectors that keeps track of number of infected, removed, and susceptible nodes over each round of time.
  # In the regular lab, you need only worry about the number of infected. 
  return(time_stats)
}



################
# Main Program #
################


# Primary school network 
infile_edges<-"Edges_sp_data_school_day_2.csv"
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
g_primschool_orig<-graph.data.frame(edge_frame, directed = FALSE)
# delete the weak edges
g_primschool<-delete.edges(g_primschool_orig, which(E(g_primschool_orig)$weight < 65))
# Make sure to convert names to integers for this graph before running the simulation
V(g_primschool)$name<-as.integer(1:vcount(g_primschool))

par(mfrow=c(1,1))
# When you view the simulations of evolving network plots, you can press Enter to continue each step, or press q to quit the simulation o
# See the list of arguments above in the function definition
infected_school_tm<-simulate_sir(g_primschool, simlength= 10,  p.t=0.05)
# Parameter display_net determines whether you view the evolving network plot. You want to set it to FALSE if you just want the statistics returned by the function 
infected_school_tm<-simulate_sir(g_primschool, simlength= 100,  p.t=0.05, display_net=FALSE)

# Run three plots together for the advanced lab. Regular lab 6 requires only the infected numbers (top plot).
par(mfrow=c(3,1))
plot(infected_school_tm$infected_t, type="l", col="red", ylab="Infected over time", xlab = "time index")
plot(infected_school_tm$removed_t, type="l", col="black", ylab="Removed over time", xlab = "time index")
plot(infected_school_tm$susceptible_t, type="l", col="blue", ylab="Susceptible over time", xlab = "time index")

# Three kinds of networks. Please explore by changing the parameters-- numbers of nodes, edges, and probabilities of 
gl <- list()
gl$ws <- watts.strogatz.game(1, 50, 2, 0.1)
gl$er <- erdos.renyi.game(75, 75, type=c("gnm"))
# gl$ba <- barabasi.game(60, m=5, directed=FALSE)

# Run the simulation to view the spread over each round
gl$ba <- barabasi.game(45, power=0.98, m=1,  directed=FALSE)
infected_time_1<-simulate_sir(gl$ba , removeafter=1000, simlength= 20, p.t=0.3, display_net=TRUE)

#Settings for Quiz 6: Question 3, on Barabasi preferential network
gl$ba_1 <- barabasi.game(90, power=0.98, m=1,  directed=FALSE)
gl$ba_2 <- barabasi.game(90, power=0.98, m=2,  directed=FALSE)

#Settings Quiz 6: Question 4, small-worlds networks
gl$ws_01 <- watts.strogatz.game(1, 50, 2, 0.01)
gl$ws_02 <- watts.strogatz.game(1, 50, 2, 0.1)
gl$ws_03 <- watts.strogatz.game(1, 50, 2, 0.2)


# Run to compile statistics. You will need to change these settings to answer quiz questions. 
infected_time_ba<-simulate_sir(gl$ba, removeafter=5000, simlength= 100, p.t=0.1, display_net=FALSE)
infected_time_ws<-simulate_sir(gl$ws, removeafter=5000, simlength= 100, p.t=0.1, display_net=FALSE)
infected_time_er<-simulate_sir(gl$er, removeafter=5000, simlength= 100, p.t=0.1, display_net=FALSE)

# Run three plots together for the advanced lab. Regular lab 6 requires only the infected numbers (top plot).
par(mfrow=c(3,1))
plot(infected_time_ba$infected_t, type="l", col="red", ylab="Infected over time", xlab = "time index")
plot(infected_time_ba$removed_t, type="l", col="black", ylab="Removed over time", xlab = "time index")
plot(infected_time_ba$susceptible_t, type="l", col="blue", ylab="Susceptible over time", xlab = "time index")

# Run three plots together for the advanced lab. Regular lab 6 requires only the infected numbers (top plot).
par(mfrow=c(3,1))
plot(infected_time_ws$infected_t, type="l", col="red", ylab="Infected over time", xlab = "time index")
plot(infected_time_ws$removed_t, type="l", col="black", ylab="Removed over time", xlab = "time index")
plot(infected_time_ws$susceptible_t, type="l", col="blue", ylab="Susceptible over time", xlab = "time index")



