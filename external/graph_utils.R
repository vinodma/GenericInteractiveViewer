library(plyr)
library(igraph)


build_initial_graph <-function (conf){
  # Uses igraph to parse the initial, full graph
  vert_atr<-TRUE
  file <- conf$FilePath
  ent1<-conf$Entity1
  typ1<-conf$type1
  ent2 <- conf$Entity2
  typ2 <- conf$type2
  if(is.null(conf$type1))
  {
    vert_atr<-FALSE
    typ1<- "type1"
    typ2<-"type2"
    table[,typ1] <- "Entity1"
    table[,typ2] <- "Entity2"
  }
  
  
  table <- read.csv(file, header = TRUE, sep = ",")
  
 

  #print(typ1)

  edges <- table[c(ent1, ent2)]
  data1 <- table[c(ent1, typ1)]
  data2 <- table[c(ent2, typ2)]
  colnames(data1) <- c('entity', 'type')
  colnames(data2) <- c('entity', 'type')
  vertex_data <- unique(rbind(data1, data2))
  #print(vertex_data)
  if(vert_atr==TRUE){
    g <- graph_from_data_frame(edges, directed = FALSE, vertices = vertex_data)
    #g1<-set.vertex.attribute(g, "entity", index=V(g), "entity1")
    return(g)
  }
  else
  {
    g <- graph_from_data_frame(edges, directed = FALSE)
    g1<-set.vertex.attribute(g, "entity", index=V(g), "entity1")
    return(g1)
  }
  
}


get_communities <- function(graph,alg="lv"){
  # Runs louvain community detection
  if(alg=="lv"){
  return(cluster_louvain(graph))
  }
  else if(alg=="wk"){
    return(cluster_walktrap(graph))
  }
  else if(alg=="fg"){
    return(cluster_fast_greedy(graph))
  }
  else if(alg=="imap"){
    return(cluster_infomap(graph))
  }
  else if(alg=="ebetweens"){
    return(cluster_edge_betweenness(graph))
  }
  else if(alg=="lp"){
    return(cluster_label_prop(graph))
  }
  else if(alg=="sg"){
    return(cluster_spinglass(graph))
  }
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