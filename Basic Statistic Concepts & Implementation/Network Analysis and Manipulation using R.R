#  Analyze Social network graphs
## Social Network Analysis is a set of methods used to visualize networks, 
## describe specific characteristics of overall network structure, 
## and build mathematical and statistical models of network structures and dynamics (Luke 2015)

# For introduction matter,
# this tutorial will take advantage of the popular game “Six Degrees of Kevin Bacon”. 
# Six Degrees of Kevin Bacon is a parlour game based on the “six degrees of separation” concept, 
# which posits that any two people on Earth are six or fewer acquaintance links apart.
# The game requires a group of players to try to connect any such individual to Kevin Bacon as quickly as possible and in as few links as possible

# 1. Load Data
library(igraph)
library(readr)
library(tidyverse)
actors <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Actors.csv")
movies <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Movies.csv")

# 2. EDA

##  actor’s name, gender, and BestActorActress
head(actors)
##  connections between actors based on movies
head(movies)
# The d variable takes the edges
# (There are social networks where the relationship is directional, for example a professor may have a directed relationship where he teaches students. As this is a list of actors who were in various movies together, 
# this is an undirected network so the directed variable takes an argument of FALSE.)
actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)
actorNetwork

plot(actorNetwork)

# Find connections
E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", 
                                ifelse(E(actorNetwork)$Movie == "Apollo 13", "black",
                                       "orange"))
V(actorNetwork)$color <- ifelse(V(actorNetwork)$BestActorActress == "Winner", "gold",
                                ifelse(V(actorNetwork)$BestActorActress == "Nominated","grey",
                                       "lightblue"))

plot(actorNetwork, vertex.frame.color="white")

legend("bottomright", c("Winner","Nominee", "Not Nominated"), pch=21,
       col="#777777", pt.bg=c("gold","grey","lightblue"), pt.cex=2, cex=.8)


legend("topleft", c("Forest Gump","Apollo 13", "The Rock"), 
       col=c("green","black","orange"), lty=1, cex=.8)

# 3. Describing the Network

# (1) Degree - it measures the number of connections between a node and all other nodes
degree(actorNetwork, mode="all")
## Nicolas Cage is connected to Sean Connery and Ed Harris, so he should have a degree centrality score of 2

# (2) Closeness - closeness of a node to all other nodes in a network
closeness(actorNetwork, mode="all", weights=NA, normalized=T)

# (3) Betweenness
betweenness(actorNetwork, directed=F, weights=NA, normalized = T)
