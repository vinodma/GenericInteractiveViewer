library(plyr)
library(igraph)


build_initial_graph <-function (file){
  # Uses igraph to parse the initial, full graph
  table <- read.csv(file, header = TRUE, sep = ",")
  edges <- table[c('MetadataFrom', 'MetadataTo')]
  data1 <- table[c('MetadataFrom', 'type1')]
  data2 <- table[c('MetadataTo', 'type2')]
  colnames(data1) <- c('entity', 'type')
  colnames(data2) <- c('entity', 'type')
  vertex_data <- unique(rbind(data1, data2))

  g <- graph_from_data_frame(edges, directed = FALSE)# vertices = vertex_data)
  g1<-set.vertex.attribute(g, "entity", index=V(g), "entity1")
  return(g1)
}


get_communities <- function(graph){
  # Runs louvain community detection
  return(multilevel.community(graph))
}

get_community_graph <- function(graph, communities){
  # Builds a graph of the communities
  V(graph)$comm <-communities$membership
  contracted <- contract.vertices(graph, communities$membership, "random")
  community_graph <- simplify(contracted, "random")
  V(community_graph)$name <- V(community_graph)$comm
  
  # Set the size of each node to be proportional to the community size
  counts <- count(communities$membership)
  V(community_graph)$size <- counts$freq
  
  
  return(community_graph)
}

subgraph_of_one_community <- function(graph, communities, community_id){
  # Builds a subgraph of one community from the original graph
  idx <- which(communities$membership == community_id)
  subgraph <- induced.subgraph(graph, idx)
  return(subgraph)
}