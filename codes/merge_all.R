
library(tidyverse)
setwd("G:\\ecology_journals_raw")


dir() -> filename
tibble() -> eco_raw

for(i in seq_along(filename))
{
  read_csv(filename[[i]]) %>% select('DOI','Year','Author Keywords','Source title','Document Type') -> a
  eco_raw %>% bind_rows(a) -> eco_raw
}

spe.j=c("AMERICAN MIDLAND NATURALIST",
        "BULLETIN OF THE AMERICAN MUSEUM OF NATURAL HISTORY",
        "FRONTIERS IN ECOLOGY AND THE ENVIRONMENT",
        "THE AMERICAN NATURALIST",
        "TRENDS IN ECOLOGY AND EVOLUTION",
        "INTERNATIONAL JOURNAL OF STD AND AIDS")

eco=eco_raw %>% 
  rename(doi='DOI',year='Year',keyword='Author Keywords',journal='Source title',type='Document Type') %>%
  filter(type=='Article') %>%
  select(-type) %>%
  na.omit() %>%
  mutate(journal=str_to_upper(journal)) %>%
  filter(year >= 1988,year<=2017) %>%
  mutate(journal=str_replace(journal,"&","AND")) %>%
  mutate(journal=str_squish(journal)) %>%
  mutate(journal = 
           str_replace(journal,
                       "EVOLUTION; INTERNATIONAL JOURNAL OF ORGANIC EVOLUTION",
                       "EVOLUTION")) %>%
  mutate(journal = 
           str_replace(journal,
                       "^PROCEEDINGS.+",
                       "PROCEEDINGS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES")) %>%
  mutate(journal = 
           str_replace(journal,
                       "FLORA: MORPHOLOGY, DISTRIBUTION, FUNCTIONAL ECOLOGY OF PLANTS",
                       "FLORA")) %>%
  mutate(journal = 
           str_replace(journal,
                       "THE JOURNAL OF ANIMAL ECOLOGY",
                       "JOURNAL OF ANIMAL ECOLOGY")) %>%
  mutate(journal = 
           str_replace(journal,
                       "THE AMERICAN NATURALIST",
                       "AMERICAN NATURALIST")) %>%
  mutate(journal = 
           str_replace(journal,
                       "CONSERVATION BIOLOGY : THE JOURNAL OF THE SOCIETY FOR CONSERVATION BIOLOGY",
                       "CONSERVATION BIOLOGY")) %>%
  filter(!journal %in% spe.j) %>%
  mutate(keyword=str_to_lower(keyword))


eco$journal %>% as.factor %>% summary 
eco$journal %>% as.factor %>% table %>% nrow  #137
nrow(eco)    #247,764

eco %>% 
  select(-journal) %>%
  tidytext::unnest_tokens(word, keyword, token = str_split, pattern = ";") %>% 
  mutate(word=str_trim(word)) %>% 
  filter(word != "") %>%
  distinct()-> tidy

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

write_csv(eco,"eco.csv")
write_csv(tidy,"tidy.csv")

#fread("eco.csv") %>% as_tibble -> eco
#fread("tidy.csv") %>% as_tibble -> tidy

#display the overall network

tidy %>%
  pairwise_count(word,doi,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  mutate(degree = centrality_degree()) -> tidy.net

tidy.net %>%
  filter(degree>=2884) %>%
  ggraph(layout="kk")+
  geom_edge_link(aes(edge_width = n), edge_colour = "skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = degree/10)) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none")

####draw_start
tiff("G:\\生态学知识图谱构建可行性分析\\文集\\PLOS_1\\第一次返修\\Fig_3.tif", 
     width = 2000,height = 1600,res = 300)
tidy.net %>%
  filter(degree>=2884) %>%
  ggraph(layout="kk")+
  geom_edge_link(aes(edge_width = n), edge_colour = "skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = degree/10)) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(legend.position = "none")
dev.off()
####draw_end

eco %>% count(journal) %>%
  rename(no.article=n) %>%
  write_csv("final_sel.csv")
