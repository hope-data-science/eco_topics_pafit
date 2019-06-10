
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

eco$journal %>% as.factor %>% summary 
eco$journal %>% as.factor %>% table %>% nrow
nrow(eco)


read_csv("G:\\生态学知识图谱构建可行性分析\\数据源\\sci_ecol_2017.csv") -> sci.raw
sci.raw %>%
  rename(rank=X1,journal=`Full Journal Title`,IF=`Journal Impact Factor`) %>%
  select(rank,journal,IF) %>%
  mutate(journal = str_to_upper(journal))-> sci

eco %>%
  filter(journal %in% sci$journal) %>%
  tidytext::unnest_tokens(word, au_keyword, token = stringr::str_split, pattern = ";") %>% 
  mutate(word=str_trim(word)) %>% 
  filter(word != "") -> tidy.app

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

write_csv(tidy.app,"tidy_app.csv")
write_csv(sci,"sci.csv")

write_csv(tidy.app,"G:\\shiny\\eco_journal\\tidy_app.csv")
write_csv(sci,"G:\\shiny\\eco_journal\\sci.csv")




