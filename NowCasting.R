#!/usr/bin/env Rscript
if(!require ('tidyverse')) {install.packages('tidyverse')}
if(!require ('data.table')) {install.packages('data.table')}
if(!require ('NobBS')) {install.packages('NobBS')}
if(!require ('scales')) {install.packages('scales')}
if(!require ('forecast')) {install.packages('forecast')}

library('tidyverse')
library('data.table')
library('NobBS')
library('scales')
library('forecast')

# Lê banco
dados <- fread(file = 'base_srag_boletim07Jul.csv', 
               sep = ';',
               quote = "\"",
               encoding = 'Latin-1',
               header = TRUE)

agregado <- c('SG_UF', '17DRS', 'ID_RG_RESI', 'ID_MN_RESI')

for (i in seq(1:length(agregado))) {
  
  dados_covid <- dados %>% 
    filter(classi == 'COVID-19')  %>% 
    select(.data[[agregado[i]]], DT_SIN_PRI, DT_NOTIFIC) %>% 
    mutate(DT_SIN_PRI = as.Date(DT_SIN_PRI)) %>%
    mutate(DT_NOTIFIC = as.Date(DT_NOTIFIC)) 
  
  ids_agregados <- distinct(dados_covid,.data[[agregado[i]]])
  for (j in seq(1:lengths(ids_agregados))) {
    nivel_agregado <- dados_covid %>%
      filter(.data[[agregado[i]]] == ids_agregados[[1]][j])
    nowcast <- NobBS(nivel_agregado, 
                     as.Date("2020-07-07"),
                     units="1 day",
                     onset_date="DT_SIN_PRI", 
                     report_date="DT_NOTIFIC")
    nowcast$estimates$ma <- ma(nowcast$estimates$estimate, order = 10)
    nowcast$estimates$n.reported[is.na(nowcast$estimates$n.reported)] <- 0
    
    ggplot(nowcast$estimates) +
      geom_point(mapping = aes(x = onset_date, y = estimate, color = 'red'), alpha = 0.6) +
      geom_line(mapping = aes(x = onset_date, y = estimate, color = 'red'), linetype = "dashed", size=0.1, alpha = 0.6) +
      geom_point(mapping = aes(x = onset_date, y = ma, color = 'navy'), alpha = 0.6) +
      geom_line(mapping = aes(x = onset_date, y = ma, color = 'navy'), alpha = 0.6) +
      geom_point(mapping = aes(x = onset_date, y = n.reported, color = 'darkgreen'), alpha = 0.6) +
      geom_line(mapping = aes(x = onset_date, y = n.reported, color = 'darkgreen'), alpha = 0.6) +
      geom_ribbon(mapping = aes(x = onset_date, ymin=lower, ymax=upper), alpha=0.1) +
      scale_color_identity(name = "Legenda",
                           breaks = c("red", "navy", "darkgreen"),
                           labels = c("Previsto (nowcasting)", "Média Movel (10 dias)", "Dado real"),
                           guide = "legend") +
      labs(x = "Data", y = "Casos") +
      ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = "")) 
    ggsave(filename = paste('NC-',agregado[i],"-",ids_agregados[[1]][j],".png",sep = ""),
           width = 12, height = 8, dpi = 300)
    }
}
