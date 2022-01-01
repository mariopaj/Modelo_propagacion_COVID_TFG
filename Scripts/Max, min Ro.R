library(tidyverse)

Ro <- read.csv("Ro.csv")

Ro <- Ro %>% filter(X > 27)

Ro <- Ro$Mean.R.

max(Ro) ## 1.958
min(Ro) ## 0,648
