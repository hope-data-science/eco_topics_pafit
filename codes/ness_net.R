
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
  geom_point(size=3, colour="grey50") +
  #geom_segment(aes(xend=degree), yend=0) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  ylab("\nCount\n") + xlab("\nDegree") + ggtitle("(B)") -> plotB



#a small proportion of top ranking keywords could cover large proportion of relationships
tidy.net %>% 
  activate(nodes) %>%
  as_tibble() %>%
  arrange(desc(degree)) %>%
  mutate(rank=row_number(desc(degree))) %>%
  mutate(prop=degree/sum(degree)) %>%
  mutate(cum_prop=cumsum(prop)) -> a

a %>%
  filter(cum_prop>=0.5) %>%
  head(1) %>%
  pull(rank) -> no.50

a %>%
  ggplot(aes(rank,cum_prop))+
  geom_line()+
  geom_point(aes(no.50,0.5,size=1),colour="#FF2400")+
  geom_hline(aes(yintercept = 0.5),linetype="dashed",colour="blue",size=0.8)+
  geom_linerange(aes(x=no.50,ymin=0,ymax=0.5),linetype="dashed",colour="blue",size=0.8) +
  annotate("text",x=no.50+8000,y=0.5-0.05,label=str_c("(",no.50,",0.5)")) +
  theme_bw() +
  ylab("Cumulative propotion of total degree") +
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(50000,100000)) -> no_need_to_show

a %>%
  mutate(rank_prop=rank/max(rank)) -> b

b%>%
  filter(cum_prop>=0.5) %>%
  dplyr::select(cum_prop,rank_prop) %>%
  head(1)-> no_50

b %>%
  ggplot(aes(rank_prop,cum_prop))+
  geom_line()+
  geom_point(aes(no_50$rank_prop,no_50$cum_prop,size=1),colour="#FF2400")+
  geom_hline(aes(yintercept = 0.5),linetype="dashed",colour="blue",size=0.8)+
  geom_linerange(aes(x=no_50$rank_prop,ymin=0,ymax=0.5),linetype="dashed",colour="blue",size=0.8) +
  annotate("text",x=no_50$rank_prop+0.15,y=0.5-0.05,
           label=str_c("(",round(no_50$rank_prop*100,2),"%,50.0%)")) +
  theme_bw()  +
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  ylab("Cumulative propotion of total degree") +
  xlab("Propotion of top ranking keywords") +
  ggtitle("(C)") -> plotC

#display all plots
grid.arrange(plotA,plotB,ncol=1)

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


