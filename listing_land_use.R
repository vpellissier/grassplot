library(readxl)
library(tidyverse)


path_grassplot <- 'C:/Users/pellissi/Documents/GrassPlot composition data'
path_git <- "C:/Users/pellissi/Documents/grassplot"
# path_grassplot <- 'D:/Backup Aarhus 15122016/vincent/Grassplot/Preliminary work/GrassPlot composition data'

# dataframe with all the informations at a plot level
df <- read_excel(file.path(path_grassplot, "GrassPlot 1.21.xlsx" ),
                                 sheet = 'Data', 
                                 col_types = c(rep('guess', 94), rep('text', 10)))

saveRDS(df, file.path(path_git, 'Grassplot 1.21_Data.rds'))
