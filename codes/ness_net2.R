
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(widyr)
library(gridExtra)
library(data.table)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

fread("tidy.csv") %>% as_tibble() -> tidy
fread("eco.csv") %>% as_tibble() -> eco

tidy %>%
  distinct(year,word) %>%
  count(year) %>%
  mutate(class="No. of distinct keywords")-> tidy.count

eco %>%
  count(year) %>%
  mutate(class="No. of articles")-> eco.count

#windowsFonts(Times=windowsFont("TT Times New Roman"))

#growth of paper no. and keyword richness
bind_rows(tidy.count,eco.count) %>%
  ggplot(aes(year,n,shape=class)) +
   geom_point(size=2) +
   geom_line(size=1) +
   ylab("") + xlab("\nYear")+theme_bw() +
   #theme(axis.title.x=element_text(family = "Times",face="bold")) +
   #theme(axis.text.x=element_text(family = "Times",face="bold")) +
   #theme(axis.text.y=element_text(family = "Times",face="bold")) +
   scale_shape_manual(values=c(1,2)) +
   theme(legend.background = element_rect(fill="white",colour = "black"))+
   theme(legend.position = c(0,1),legend.justification = c(0,1)) +
   theme(legend.title=element_blank()) 

tiff("G:\\生态学知识图谱构建可行性分析\\文集\\PLOS_1\\第一次返修\\Fig_1.tif", 
     width = 2000,height = 1600,res = 300)
bind_rows(tidy.count,eco.count) %>%
  ggplot(aes(year,n,shape=class)) +
  geom_point(size=2) +
  geom_line(size=1) +
  ylab("") + xlab("\nYear")+theme_bw() +
  #theme(axis.title.x=element_text(family = "Times",face="bold")) +
  #theme(axis.text.x=element_text(family = "Times",face="bold")) +
  #theme(axis.text.y=element_text(family = "Times",face="bold")) +
  scale_shape_manual(values=c(1,2)) +
  theme(legend.background = element_rect(fill="white",colour = "black"))+
  theme(legend.position = c(0,1),legend.justification = c(0,1)) +
  theme(legend.title=element_blank()) 
# Make plot
dev.off()


#drop of network density  
make_graph = function(df){
  df %>%
    pairwise_count(word,doi,upper=F) %>%
    graph_from_data_frame(directed = F) %>%
    as_tbl_graph(directed=F)
}

tidy %>%
  group_by(year) %>%
  nest() %>%
  mutate(graph=map(data,make_graph)) -> graph.table

graph.table %>%
  mutate(edge_den = map_dbl(graph,edge_density)) %>%
  ggplot(aes(year,edge_den))+
  geom_point(shape=1,size=2) +
  geom_smooth(linetype="dashed",se=F,show.legend = T,colour="black") +
  theme_bw() + 
  #theme(axis.title.y=element_text(family = "Times",face="bold")) +
  #theme(axis.text.x=element_text(family = "Times",face="bold")) +
  #theme(axis.text.y=element_text(family = "Times",face="bold")) +
  #theme(axis.title.x=element_text(family = "Times",face="bold")) +
  ylab("\nKnowledge network density\n") + xlab("\nYear") +
  ggtitle("(A)") -> plotA

#network cluster no.
make_graph = function(df){
  df %>%
    pairwise_count(word,doi,upper=F) %>%
    graph_from_data_frame(directed = F) %>%
    as_tbl_graph(directed=F)
}

tidy %>%
  group_by(year) %>%
  nest() %>%
  mutate(graph=map(data,make_graph)) -> graph.table

graph.table %>%
  mutate(cluster_no = map_dbl(graph,count_components)) %>%
  ggplot(aes(year,cluster_no))+
  geom_point(shape=1,size=2) +
  geom_smooth(linetype="dashed",se=F,show.legend = T,colour="black") +
  theme_bw() + 
  #theme(axis.title.y=element_text(family = "Times",face="bold")) +
  #theme(axis.text.x=element_text(family = "Times",face="bold")) +
  #theme(axis.text.y=element_text(family = "Times",face="bold")) +
  #theme(axis.title.x=element_text(family = "Times",face="bold")) +
  ylab("\nNo. of clusters\n") + xlab("\nYear") +
  ggtitle("(B)") -> plotB


#construct network
tidy %>%
  pairwise_count(word,doi,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  mutate(degree = centrality_degree()) -> tidy.net

#degree distribution
tidy.net %>%
  as_tibble() %>%
  count(degree) %>%
  ggplot(aes(x=degree,y=n)) +
  geom_point(size=2, shape = 1) +
  geom_smooth(linetype="dashed",se=F,show.legend = T,colour="black") +
  #geom_smooth(linetype="dashed",se=F,show.legend = T,colour="black") +
  #geom_segment(aes(xend=degree), yend=0) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  scale_x_log10() + scale_y_log10() +
  ylab("\nCount\n") + xlab("\nDegree") + ggtitle("(C)") -> plotC


#display all plots
tiff("G:\\生态学知识图谱构建可行性分析\\文集\\PLOS_1\\第一次返修\\Fig_4.tif", 
     width = 2000,height = 1600,res = 300)
grid.arrange(plotA,plotB,plotC,ncol=2)  # Make plot
dev.off()

grid.arrange(plotA,plotB,plotC,ncol=2) 

#示例网络图
eco %>%
  slice(300) %>%
  pull(doi) -> sample.doi

tidy %>%
  filter(doi==sample.doi) %>%
  pairwise_count(word,doi,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  ggraph(layout = "kk")+
  geom_edge_link(aes(size=10)) +
  geom_node_point(aes(size=7)) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 fontface="bold",size=5,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none")

#
tidy.net %>% as_tibble %>% arrange(desc(degree))
tidy %>% count(word) %>% arrange(desc(n))
tidy %>% count(word) %>% pull(n) %>% table

