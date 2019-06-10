

library(tidyverse)
library(data.table)
library(tidygraph)
library(ggraph)
library(widyr)
library(igraph)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")


fread("tidy.csv") %>% as_tibble -> tidy
fread("full_table.csv") %>% as_tibble() -> full.table

top=100

full.table %>% top_n(top,P) -> p.table
full.table %>% top_n(top,freq) -> f.table
#full.table %>% top_n(top,degree) -> d.table

anti_join(p.table,f.table,by="word")
#anti_join(p.table,d.table,by="word")
#anti_join(d.table,f.table,by="word")


tidy %>%
  pairwise_count(word,doi,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) -> tidy.net

full.table %>%
  select(word,fitness,P,freq) -> sub.table

tidy.net %>%
  activate(nodes) %>%
  left_join(sub.table) -> tidy.full

tidy.full %>%
  as_tibble %>%
  pull(word) -> a

##
which(a=="remote sensing") -> b #241
  
tidy.full %>% 
  make_ego_graph(1,b) %>%
  .[[1]] %>%
  as_tbl_graph()-> re.net

re.net %>% 
  mutate(word=str_replace(word,"plant鈥損lant interactions","plant-plant interactions")) %>%
  mutate(degree = centrality_degree())-> re.net

re.net %>%
  as_tibble() %>%
  gather(P,fitness,key="class",value="value") %>%
  group_by(class) %>%
  top_n(30) %>%
  pull(word) %>%
  unique -> sel.key

re.net %>%
  mutate(P.rank=row_number(desc(P)),
         fit.rank=row_number(desc(fitness)),
         degree.rank=row_number(desc(degree))) %>%
  mutate(P.top5=ifelse(P.rank<=5,T,F),
         fit.top5=ifelse(fit.rank<=5,T,F),
         degree.top5=ifelse(degree.rank<=6,"1","2")) %>%
  mutate(degree.top5=ifelse(word=="remote sensing","0",degree.top5)) %>%
  filter(word %in% sel.key) %>%
  ggraph(layout="kk")+            #kk,drl
  geom_edge_link(aes(edge_width = weight), edge_colour="skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = degree,shape=degree.top5,colour=P.top5)) +
  geom_node_text(aes(label = word,colour=fit.top5), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none") +
  scale_color_manual(values=c("#000000","#FF0000")) +
  scale_shape_manual(breaks=c("0","1","2"),values=c(15, 17, 16))

##作图
tiff("G:\\生态学知识图谱构建可行性分析\\文集\\PLOS_1\\第一次返修\\Fig_6.tif", 
     width = 1800,height = 1800,res = 300)
re.net %>%
  mutate(P.rank=row_number(desc(P)),
         fit.rank=row_number(desc(fitness)),
         degree.rank=row_number(desc(degree))) %>%
  mutate(P.top5=ifelse(P.rank<=5,T,F),
         fit.top5=ifelse(fit.rank<=5,T,F),
         degree.top5=ifelse(degree.rank<=6,"1","2")) %>%
  mutate(degree.top5=ifelse(word=="remote sensing","0",degree.top5)) %>%
  filter(word %in% sel.key) %>%
  ggraph(layout="kk")+            #kk,drl
  geom_edge_link(aes(edge_width = weight), edge_colour="skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = degree,shape=degree.top5,colour=P.top5)) +
  geom_node_text(aes(label = word,colour=fit.top5), repel = TRUE, 
                 fontface="bold",size=3,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none") +
  scale_color_manual(values=c("#000000","#FF0000")) +
  scale_shape_manual(breaks=c("0","1","2"),values=c(15, 17, 16))
# Make plot
dev.off()


#####################################通用作图接口 
"litter fall" -> string

which(a==string) -> b #241

tidy.full %>% 
  make_ego_graph(1,b) %>%
  .[[1]] %>%
  as_tbl_graph()-> re.net

re.net %>% 
  mutate(degree = centrality_degree())-> re.net

re.net %>%
  as_tibble() %>%
  gather(P,fitness,key="class",value="value") %>%
  group_by(class) %>%
  top_n(30) %>%
  pull(word) %>%
  c(string) %>%
  unique -> sel.key

re.net %>%
  mutate(P.rank=row_number(desc(P)),
         fit.rank=row_number(desc(fitness)),
         degree.rank=row_number(desc(degree))) %>%
  mutate(P.top5=ifelse(P.rank<=6,T,F),
         fit.top5=ifelse(fit.rank<=6,T,F),
         degree.top5=ifelse(degree.rank<=6,"1","2")) %>%
  mutate(degree.top5=ifelse(word==string,"0",degree.top5)) %>%
  filter(word %in% sel.key) %>%
  ggraph(layout="kk")+            #kk,drl
  geom_edge_link(aes(edge_width = weight), edge_colour="skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = degree,shape=degree.top5,colour=P.top5)) +
  geom_node_text(aes(label = word,colour=fit.top5), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none") +
  scale_color_manual(values=c("#000000","#FF0000")) +
  scale_shape_manual(breaks=c("0","1","2"),values=c(15, 17, 16))


