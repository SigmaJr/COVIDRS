rm(list = ls())
library(readxl)
library(tidyverse)
library(ggthemes)

require(ggplot2) # para gr?ficos
library(qqplotr) # Gr?ficos e envelope no ggplot2
library(plotly) # gr?ficos interativos
library(geobr)          # Bases de dados de mapas do Brasil
library(sf)             # Edi??o de arquivos de superf?cie
library(RColorBrewer)   # Paleta de cores
library(shiny)

dados <- read_excel("dados_covid.xlsx")
dados%>%
  separate(`% pop. pelo menos uma dose`, c("percentual_1dose", "Total1dose"), "%", extra = "merge")%>%
  separate(`% pop. esquema vacinal completo`, c("percentual_completo", "Totalcompleto"), "%", extra = "merge")%>%
  mutate(percentual_1dose=as.numeric(percentual_1dose))%>%
  mutate(percentual_completo=as.numeric(percentual_completo))%>%
  mutate(nome=as.character(nome)) %>%
  mutate(Totalcompleto = as.numeric(gsub("\\(|)", "",Totalcompleto ))) %>%
  mutate(Total1dose = as.numeric(gsub("\\(|)", "",Total1dose))) ->dadosn


municipios<- read_municipality() %>%
  filter(code_state==43)

percentual_map<- inner_join(municipios, dadosn, 
                            by=c("code_muni"="codigo"))

mytext <- paste(
  "Município: ", percentual_map$nome,"</br>",
  sep="") %>%
  lapply(htmltools::HTML)

graf=ggplot(percentual_map, aes(fill=percentual_1dose, text=mytext)) +
  geom_sf(color= NA, size=.15) +
  scale_fill_gradientn(colours= brewer.pal(7,'Spectral')) +
  labs(#title = "Vacinação no Rio Grande do Sul",
       caption = "Fonte: Secretaria da Saúde - RS", fill = "% da população com a\n primeira dose") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

graf1<-ggplotly(graf)

graf=ggplot(percentual_map, aes(fill=percentual_completo, text=mytext)) +
  geom_sf(color= NA, size=.15) +
  scale_fill_gradientn(colours= brewer.pal(7,'Spectral')) +
  labs(#title = "Vacinação no Rio Grande do Sul",
       caption = "Fonte: Secretaria da Saúde - RS", fill = "% da população com a\n vacinação completa") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

graf2<-ggplotly(graf)


save.image("grafRS.RData")
