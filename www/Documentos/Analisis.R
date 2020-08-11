library(tidyverse)
library(readxl)

bdEcon <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
  rename("Clasificación" = `Población Objetivo`) %>% 
  mutate(Clasificación = str_remove(Clasificación, 
                                    pattern = "\n"))

unique(bdEcon$Clasificación)

PYM <- bdEcon %>% 
  filter(Clasificación %in% c("PYMES", "MIPYMES"))

PYM %>% 
  group_by(Entidad) %>% 
  count() %>% 
  arrange(-n) %>% 
  ggplot(aes(x = reorder(Entidad, n), y = n, fill = n)) + 
  geom_col() +
  geom_text(aes(label = n), hjust = -0.5) + 
  scale_fill_gradient(low = "#00d9ae", high = "#00705a") + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,10), 
                     breaks = seq(0,10,1)) + 
  labs(x = "", 
       caption = "Fuente: Datos de la Plataforma de\nPolíticas Económicas del LNPP del CIDE y Federalismo en COVID. 2020",
       y = "", 
       title = "Número de Programas de apoyo a\nPYMES y MIPYMES por Estado") +
  coord_flip() + 
  theme_minimal() + 
  theme(text = element_text(family = "Poppins"),
    axis.text.y = element_text(size = 15))
  
  

