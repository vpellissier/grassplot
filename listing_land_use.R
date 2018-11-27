library(readxl)
library(tidyverse)


path_grassplot <- 'C:/Users/pellissi/Documents/GrassPlot composition data'

# dataframe with all the informations at a plot level
df <- read_excel(file.path(path_grassplot, "GrassPlot 1.19.xlsx" ),
                                 sheet = 'Data', 
                                 col_types = c(rep('guess', 94), rep('text', 10)))

saveRDS(df, file.path(path_grassplot, 'Grassplot 1.19_Data.rds'))

# Only little info in this one about land-use data, but useful to extract 
# compositional datasets
info_df <- read_excel(file.path(path_grassplot, "GrassPlot 1.8.xlsx" ),
                       sheet = 'Datasets')

df <- readRDS(file.path(path_grassplot, 'Grassplot 1.8_Data.rds'))


names(df)[95:104]

# Replacing hardcoded NA by proper NA values
df <- df %>%
  mutate_at(.vars = c(95:104), funs(ifelse(. %in% c('NA', '[NA]'), NA, .)))

# Details

df%>%
  group_by(`Mowing frequency: cuts per year (2=2cut/yr; 1=1cut/yr; 0.5=1 cut/2 yr or 2 yr abandoned; 0.2=1 cut/5 yr or 5 yr abandones; 0=never mown)`, 
           `Mowing (1/0)`)%>%
  summarise(n=n())%>%
  spread(`Mowing (1/0)`, n)%>%
  kable()


mpg%>%
  group_by(`class`, `cyl`)%>%
  summarise(n=n())%>%
  spread(cyl, n)%>%
  kable()