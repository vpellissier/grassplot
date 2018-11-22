options(warn = 0)
library(readxl)
library(tidyverse)
library(magrittr)
library(knitr)
library(kableExtra)


table_grazed <- function(data, id){
  data%>%
    subset(`Dataset ID` == id)%>%
    group_by(`grazed`, 
             `grazing_intensity`, `abandoned`, `natural_grassland`, 
             `mowing_frequency`, `mown`, `Mowing (1/0)`)%>%
    summarise(n=n())%>%
    spread(`grazed`, n)
}

table_mown <- function(data, id){
  data%>%
    subset(`Dataset ID` == id)%>%
    group_by(`mown`, 
             `grazing_intensity`, `abandoned`, `natural_grassland`, 
             `mowing_frequency`, `mown`, `Mowing (1/0)`)%>%
    summarise(n=n())%>%
    spread(`mown`, n)
}

path_grassplot <- './'

df <- readRDS(file.path(path_grassplot, 'Grassplot 1.8_Data.rds'))
df <- df %>%
  mutate_at(.vars = c(95:104), funs(ifelse(. %in% c('NA', '[NA]'), NA, .)))

# The columns 98 (mowing frequency) and 100 (grazing intensity) are renamed:
names(df)[c(98,100)] <- c('mowing_frequency', 'grazing_intensity')

### Correction of the land use column
lut <- read_excel(file.path(path_grassplot, "lookup_table_LU.xlsx" ),
                  sheet = 'global_land_use')

# The global_land_use lookup table is used to harmonize the column 95 with the new binary columns:
df <- df %>%
left_join(lut, by = c("Land use (5 standard categories: mown, grazed, abandoned, natural grassland, NA)" = "old_classification"))

# Replacement of odd values and conversion to numerical
df$mowing_frequency <- as.numeric(df$mowing_frequency)

df[df$grazing_intensity %in% c('overgrazing', 'high'),]%<>%
  mutate(grazing_intensity = 1)

df[df$grazing_intensity %in% c('middle'),]%<>%
  mutate(grazing_intensity = 0.5)

df[df$grazing_intensity %in% c('low'),]%<>%
  mutate(grazing_intensity = 0.1)

df$grazing_intensity <- as.numeric(df$grazing_intensity)

### Matching new binary columns and intensity columns
#### Mowing and mowing intensity
# Datasets containing plots for which mowing intensity > 0 & mown != 1) (here and after, we refer to the new binary columns)
df %>% 
filter(`mown` != 1 & `mowing_frequency` != 0 )%>%
distinct(`Dataset ID`)%>%
pull()

# EU_K contains resp. 4 and 4 plots classified as natural_grassland with a mowing frequency of resp. 0.03 and 4. Only the plots with a frequency >= 0.2 will be classified as mown (abandoned less than 5 years).
# EU_K contains resp. 12, 84, 30, 4 and 12 plots classified as abandoned with a mowing frequency of resp. 0.03, 0.05, 0.1, 0.2, 0.3. Only the plots with a frequency >= 0.2 will be classified as mown (abandoned less than 5 years).
df[df$`Dataset ID` == 'EU_K' & df$mowing_frequency %in% c('4', '1', '0.5', '0.3', '0.2'),] %<>% 
  mutate(mown = 1)

# CZ_J contains plot classified as abandoned which have a mowing frequency == 0.05. These will not be classified as mown. (I could not find the original land-use information in CZ_J.xls)



#### Grazing and Grazing intensity
# Datasets containing plots for which grazing intensity > 0 & grazed != 1)

df %>% 
filter(`grazed` != 1 & `grazing_intensity` != '0')%>%
distinct(`Dataset ID`)%>%
pull()

# IR_A contains 34 plots classified as mown which have a grazing_intensity == 0.1. These plots will be classified as grazed (and also remain mown as indicated in the original database IR_A.xls). **Note for Idoia** In the original DB, the grazing intensity is noted as 1, 2, 3. Is it correct that it stands for low, medium and high intensity, and hence translate as 0.1, 0.5, 1 in the master file?
table_grazed(df, 'IR_A')

df[df$`Dataset ID` == 'IR_A',] %<>% 
    mutate(grazed = ifelse(grazing_intensity != '0', 1, 0))

table_grazed(df, 'IR_A')

# PL_D contains 39 plots classified as mown which have a grazing_intensity == 0.5. These plots will be classified as grazed. (No composition data, so I could not check the original dataset)
table_grazed(df, 'PL_D')

df[df$`Dataset ID` == 'PL_D' & df$grazing_intensity  %in% '0.5',] %<>% 
    mutate(grazed = 1)

table_grazed(df, 'PL_D')

# TR_B contains 32 plots classified as abandoned which have a grazing_intensity == 0.1. These plots will be classified as grazed (no further info could be found in TR_B.xls)
table_grazed(df, 'TR_B')

df[df$`Dataset ID` == 'TR_B' & df$grazing_intensity  %in% '0.1',] %<>% 
  mutate(grazed = 1)

table_grazed(df, 'TR_B')

# UA_G contains reps. 12, 15, 30 and 3 plots classified as natural_grassland which have a grazing_intensity == 'high', 'low', 'middle' or 'overgrazing'. These plots will be classified as grazed (I could not find the original land-use information in UA_G.xls)
table_grazed(df, 'UA_G')
  
df[df$`Dataset ID` == 'UA_G' & df$grazing_intensity %in% 
     c('high', 'low', 'middle', 'overgrazing'),] %<>% 
  mutate(grazed = 1)

table_grazed(df, 'UA_G')

# EU_K contains 4 plots classified as natural_grassland which have a grazing_intensity == 0.5. These plots will be classified as grazed (no further info could be found in EU_K.xls)
table_grazed(df, 'EU_K')

df[df$`Dataset ID` == 'EU_K' & df$grazing_intensity %in% '0.5',] %<>% 
  mutate(grazed = 1)
table_grazed(df, 'EU_K')

### Matching new and old binary columns
# In this step, we identify and correct discrepancies between the newly created and corrected binary column, and the former ones


df%>%
group_by(`mown`, `Mowing (1/0)`)%>%
summarise(n=n())%>%
spread(`mown`, n)%>%

#Datasets containing plots with mown == 1 & Mowing (1/0) == 0 (new vs former column):
df %>% 
filter(`mown` == 1 & `Mowing (1/0)` == '1' )%>%
distinct(`Dataset ID`)%>%
pull()

# Datasets containing plots with mown == 0 & Mowing (1/0) == 1 (new vs former column):
df %>% 
filter(`mown` == 0 & `Mowing (1/0)` == '0' )%>%
distinct(`Dataset ID`)%>%
pull()

# Datasets containing plots with mown == 1 & Mowing (1/0) = NA (new vs former column)
df %>% 
filter(`mown` == 1 & is.na(`Mowing (1/0)`))%>%
distinct(`Dataset ID`)%>%
pull()

