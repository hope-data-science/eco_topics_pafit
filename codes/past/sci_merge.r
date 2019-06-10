
library(tidyverse)
setwd("G:\\ecology_journals_raw")


dir() -> filename
tibble() -> eco_raw

for(i in seq_along(filename))
{
 read_csv(filename[[i]]) %>% select('Link','Year','Author Keywords','Source title','Document Type') -> a
 eco_raw %>% bind_rows(a) -> eco_raw
}

eco_raw %>% 
 rename(link='Link',year='Year',au_keyword='Author Keywords',journal='Source title',type='Document Type') %>%
 filter(type=='Article') %>%
 select(-type) %>%
 na.omit() %>%
 filter(year >= 1988,year<=2017) %>%
 filter(journal != "J. ECOL.") %>%
 distinct() %>%
 mutate(journal = str_to_upper(journal)) %>%
 filter(journal != "JOURNAL OF APPLIED ECOLOGY (CHINA)") %>%
 mutate(journal = str_replace(journal,"FUNCT. ECOL.","FUNCTIONAL ECOLOGY")) %>%
 mutate(journal = str_replace(journal,"THE JOURNAL OF ANIMAL ECOLOGY","JOURNAL OF ANIMAL ECOLOGY")) %>%
 mutate(journal = str_replace(journal,"THE ISME JOURNAL","ISME JOURNAL")) %>%
 mutate(journal = str_replace(journal,"^MICROBIAL.+","MICROBIAL ECOLOGY")) %>%
 mutate(journal = str_replace(journal,"FOREST ECOLOGY & MANAGEMENT","FOREST ECOLOGY AND MANAGEMENT")) %>%
 mutate(journal = str_replace(journal,"TRENDS IN ECOLOGY & EVOLUTION","TRENDS IN ECOLOGY AND EVOLUTION")) %>%
 mutate(journal = str_replace(journal,"ECOLOGICAL APP.+","ECOLOGICAL APPLICATIONS")) %>%
 filter(journal != "TRENDS IN ECOLOGY AND EVOLUTION") %>%
 filter(journal != "FRONTIERS IN ECOLOGY AND THE ENVIRONMENT") %>%
 filter(journal != "ECOSYSTEM SERVICES") %>%
 filter(journal != "INTERNATIONAL JOURNAL OF STD AND AIDS") -> eco

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")
read_csv("sci.csv") -> sci
sci %>% filter(IF>=3) -> sci3
eco %>% filter(journal %in% sci3$journal) -> eco1


eco1$journal %>% as.factor %>% summary 
eco1$journal %>% as.factor %>% table %>% nrow
nrow(eco1)

write_csv(eco1,"ecol.csv")




