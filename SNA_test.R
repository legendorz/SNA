library(readxl)
library(dplyr)
library(tidyverse)
library(igraph)
library(statnet)
library(visNetwork)
library(RColorBrewer)
library(networkD3)
library(tibble)
library(magrittr) 

x <- "/Users/glenn/Desktop/xxxx"
setwd(x)

ag_cd = "xxxx1"

edge_path= paste0("EDGE_", ag_cd, ".xlsx")
node_path= paste0("NODE_", ag_cd, ".xlsx")
edge <- read_excel(edge_path)
node <- read_excel(node_path)

nodes <- node %>% rowid_to_column("id")
colnames(nodes)[2:3]<-c("label", "group")
colnames(edge)[3] <- "group"
nodes$color <- factor(nodes$group, levels=c("POL", "ADDR","G_AG", "B_AG", "CELL", "M_C", "G_C"), 
                      labels=c("green", "red", "lightskyblue", "lightskyblue","yellow", "blue","black"))

edges <- edge %>% 
  left_join(nodes, by = c("SOURCE" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("TARGET" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, group)

#動態網路圖
visNetwork(nodes, edges) %>%  visEdges(value = 10) %>%
  visGroups(groupname  = "POL", shape = "icon", icon = list(code = "f15c", color="green")) %>%
  visGroups(groupname  = "CELL", shape = "icon", icon = list(code = "f10b", color="yellow")) %>%
  visGroups(groupname  = "CUST", shape = "icon", icon = list(code = "f007", color="black")) %>%
  visGroups(groupname  = "ADDR", shape = "icon", icon = list(code = "f015", color="red")) %>%
  addFontAwesome() %>% 
  visIgraphLayout(physics=FALSE)

#動態網路圖2
visNetwork(nodes, edges) %>%  visEdges(value = 10) %>%
  visGroups(groupname  = "POL", shape = "icon", icon = list(code = "f15c", color="green")) %>%
  visGroups(groupname  = "CELL", shape = "icon", icon = list(code = "f10b", color="yellow")) %>%
  visGroups(groupname  = "CUST", shape = "icon", icon = list(code = "f007", color="silver")) %>%
  visGroups(groupname  = "ADDR", shape = "icon", icon = list(code = "f015", color="red")) %>%
  visClusteringByGroup(groups = c("POL", "CUST", "CELL", "ADDR")) %>%
  visLegend() %>% 
  addFontAwesome() %>% 
  visIgraphLayout(physics=FALSE)

#動態網路圖
# visNetwork(nodes, edges) %>%  visEdges(value = 10) %>%
#   visGroups(groupname  = "POL", shape = "icon", icon = list(code = "f15c", color="green")) %>%
#   visGroups(groupname  = "CELL", shape = "icon", icon = list(code = "f10b", color="yellow")) %>%
#   visGroups(groupname  = "M_C", shape = "icon", icon = list(code = "f007", color="blue")) %>%
#   visGroups(groupname  = "G_C", shape = "icon", icon = list(code = "f007", color="black")) %>%
#   visGroups(groupname  = "ADDR", shape = "icon", icon = list(code = "f015", color="red")) %>%
#   addFontAwesome() %>% 
#   visIgraphLayout(physics=FALSE) %>%
#   visSave(file = "network.html", selfcontained =TRUE)

#動態網路圖2
# visNetwork(nodes, edges) %>%  visEdges(value = 10) %>%
#   visGroups(groupname  = "POL", shape = "icon", icon = list(code = "f15c", color="green")) %>%
#   visGroups(groupname  = "CELL", shape = "icon", icon = list(code = "f10b", color="yellow")) %>%
#   visGroups(groupname  = "CUST", shape = "icon", icon = list(code = "f007", color="silver")) %>%
#   visGroups(groupname  = "ADDR", shape = "icon", icon = list(code = "f015", color="red")) %>%
#   visClusteringByGroup(groups = c("POL", "CUST", "CELL", "ADDR")) %>%
#   visLegend() %>% 
#   addFontAwesome() %>% 
#   visIgraphLayout(physics=FALSE)

#一般網路圖
colnames(node)[2] <- "type"
net = graph_from_data_frame(edge,vertices = node,directed = T)

colrs.v = c(AG = "blue", CELL = "yellow", CUST = "paleturquoise1", ADDR="red", POL="green") #node colours
V(net)$color = colrs.v[V(net)$type]

plot(net, edge.arrow.size=.3, vertex.size=10, vertex.label=NA) 
