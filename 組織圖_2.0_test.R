library(readxl)
library(dplyr)
library(tidyverse)
library(igraph)
library(statnet)
library(visNetwork)
library(networkD3)
library(RColorBrewer)
library(networkD3)
library(tibble)
library(magrittr) 

x <- "//KM/資料交換區/xx"
setwd(gsub("\\\\", "/", x))

office="xx1"
office="xx2"
office="xx3"
office="xx4"


edge_path= paste0("EDGE_", office, ".xlsx")
node_path= paste0("NODE_", office, ".xlsx")
edge <- read_excel(edge_path)
node <- read_excel(node_path)

nodes <- node %>% rowid_to_column("id")
colnames(nodes)[2]<-c("label")

edges <- edge %>% 
  left_join(nodes, by = c("SOURCE" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("TARGET" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to)

nodes$label = paste(nodes$NAME, substr(nodes$START_DATE, 1, 4))

# 如果離職的話
# nodes$RISK<- ifelse(node$exit==1, -1, nodes$RISK)
# nodes$color =cut(as.numeric(nodes$RISK), breaks=c(-1, 0, 0.5, 0.8, 1.0),
#                  labels=c("silver", "green", "orange", "red"), include.lowest=T)

nodes$color =cut(as.numeric(nodes$RISK), breaks=c(0, 0.5, 0.8, 1.0),
                 labels=c("green", "orange", "red"), include.lowest=T)

nodes$shape <- factor(nodes$Contract_level_desc, 
  levels=c("通訊處經理", "通訊處總監", "績優區經理", "區經理", "業務襄理", "業務主任","業務代表", "業務專員"), 
  labels=c("star", "star", "diamond", "diamond", "triangle", "dot", "dot", "dot"))


class <- function(x) {
  if(x=="通訊處經理") {
    100
  } else if (x=="通訊處總監"){
    100
  } else if (x=="績優區經理"){
    50
  }else if (x=="區經理"){
    50
  }else if (x=="業務襄理"){
    25
  }else{
    40
  }
}
nodes$value <- sapply(nodes$Contract_level_desc, class)

visNetwork(nodes, edges) %>% 
  visEdges(arrows = "from", color = "black") %>%
  visIgraphLayout(physics=FALSE)


visNetwork(nodes, edges) %>% 
  visNodes(shape = "square", size = 10) %>%  
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() %>%
  visIgraphLayout(physics=FALSE)

visNetwork(nodes, edges) %>% 
  visNodes(shape = "square", size = 10) %>%  
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout(direction = "LR") %>%
  visIgraphLayout(physics=FALSE)

visNetwork(nodes, edges) %>% 
  visNodes(shape = "square", size = 10) %>%
  visEdges(arrows = "from") %>% 
  visIgraphLayout(physics=FALSE)
