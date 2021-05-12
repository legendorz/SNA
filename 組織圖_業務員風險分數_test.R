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

x <- "//xxxx/xx"
setwd(gsub("\\\\", "/", x))

office="xx1"


edge_path= paste0("EDGE_", office, ".xlsx")
node_path= paste0("NODE_", office, ".xlsx")

node_path= paste0("NODE_", office, "_Y2.xlsx")
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

nodes$title <- if (grepl("Y1", node_path, fixed=TRUE)) {
  paste0("<b>score:", nodes$SCORE, 
         "</b><br> X1:", nodes$X1, "; ", node$X1_DETAIL,
         "</b><br> X2:", nodes$X2, "; ", round(as.numeric(sub("%", "", node$X2_DETAIL)), 2), 
         "</b><br> X3:", nodes$X3, "; ", node$X3_DETAIL, 
         "</b><br> X4:", nodes$X4, "; ", node$X4_DETAIL,
         "</b><br> X5:", nodes$X5, "; ", round(as.numeric(sub("%", "", node$X5_DETAIL)), 2),
         "</b><br> X6:", nodes$X6, "; ", round(as.numeric(sub("%", "", node$X6_DETAIL)), 2))
}  else if (grepl("Y2", node_path, fixed=TRUE)){
  paste0("<b>score:", nodes$SCORE, 
         "</b><br> X1:", nodes$X1, "; ", node$X1_DETAIL,
         "</b><br> X2:", nodes$X2, "; ", round(as.numeric(sub("%", "", node$X2_DETAIL)), 2), 
         "</b><br> X3:", nodes$X3, "; ", node$X3_DETAIL, 
         "</b><br> X4:", nodes$X4, "; ", node$X4_DETAIL,
         "</b><br> X5:", nodes$X5, "; ", round(as.numeric(sub("%", "", node$X5_DETAIL)), 2))
} else {
  paste0("<b>score:", nodes$SCORE, 
         "</b><br> X1:", nodes$X1, "; ", node$X1_DETAIL,
         "</b><br> X2:", nodes$X2, "; ", node$X2_DETAIL, 
         "</b><br> X3:", nodes$X3, "; ", round(as.numeric(sub("%", "", node$X3_DETAIL)), 2), 
         "</b><br> X4:", nodes$X4, "; ", node$X4_DETAIL,
         "</b><br> X5:", nodes$X5, "; ", node$X5_DETAIL)
}
#被申訴過的另外標顏色; NA押成0.1
nodes$SCORE <- ifelse(is.na(nodes$Y)==TRUE, 0.1, ifelse(nodes$Y==1, -1, nodes$SCORE))

nodes$color <- cut(as.numeric(nodes$SCORE), breaks=c(-1, 0, 10, 50, 150),
      labels=c("#FFFF00", "#ADADAD", "#FF7575", "#AE0000"), 
      include.lowest=T)

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
    40
  }else{
    40
  }
}
nodes$value <- sapply(nodes$Contract_level_desc, class)

visNetwork(nodes, edges) %>% 
  visEdges(arrows = "from", color = "black") %>%
  visIgraphLayout(physics=FALSE)
