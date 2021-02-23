
# DY note:

# This script should be run seventh

# This script produces the two graphs found in the paper

# Requires: cv_data_analysed.csv, aggregate_predictors_new.csv

library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(stats)
library(ggpubr)
library(kableExtra)

library(cowplot)
library(purrr)
library(Rmisc)
library(tidyverse)
library(ggforce)
library(PupillometryR)

rm(list = ls())
options(scipen = 99999)


data <- read.csv("cv_data_analysed.csv")
preds <- read.csv("aggregate_predictors_new.csv")

preds %<>% dplyr::select(-colnames(preds)[
  which(is.na(match(colnames(preds), colnames(data))) == FALSE)])


# separately coding the two 2015 Greek elections:
data$ELECTION %<>% as.character
data$ELECTION[which(data$ELECTION == "GRC_2015" & data$module == 5)] %<>% paste0(., "_2")
data$name %<>% as.character
data$name[which(data$name == "GRC_2015" & data$module == 5)] %<>% paste0(., "_2")
preds$election %<>% as.character
preds$election[which(preds$election == "GRC_2015" & preds$mod == 5)] %<>% paste0(., "_2")

# merging the data with the aggregate-level predictors:
data %<>% cbind(preds[match(data$ELECTION, preds$election),])

# filtering out elections with missing data (and MEX_2009 + ROU_2012 which are not HoS):
data %<>% filter(ELECTION %in% c("MNE_2012", "MNE_2016", "ROU_2012", "FRA_2007", "PER_2016",
                                 "THA_2007", "THA_2011", "MEX_2009", "BRA_2014", "SRB_2012") == FALSE)

# need to merge DEU12002 and DEU22002
data$name[which(data$ELECTION == "DEU12002")] %<>% sub("U12", "U_2",.)
data$name[which(data$ELECTION == "DEU22002")] %<>% sub("U22", "U_2",.)

deus <- c(which(data$ELECTION == "DEU12002"), which(data$ELECTION == "DEU22002"))
data$ELECTION[deus] <- "DEU_2002"
data$election[deus] <- "DEU_2002"
data$name %<>% as.character

data %>% nrow
data$election %>% unique %>% length
data$country %>% unique %>% length
data$year %>% range
data %>% filter(is.na(continuous) == FALSE) %>% nrow

data$enep[grep("R2", data$name)] <- 2

data %>% filter(binary == 0) %>% ggplot(
  aes(x = continuous, group = as.factor(round(enep, 0)), colour = as.factor(round(enep, 0)))
) + geom_density()

data %>% group_by(name) %>% dplyr::summarise(
  cont = mean(continuous, na.rm=T),
  bin = mean(binary, na.rm=T)
)


# where I have SMD and MMD data for the same election I only want to include a non-overlapping
# subset of Ss from each, proportional to the number of seats allocated by SMD/MMD in the lower house
# no data on proportions of seats in each tier in Switzerland 2011 or Estonia 2011
# Switzerland has very complex voting system where voters can vote for lists or individual
# candidates, and the results are pooled. I do a 50:50 split.
data$mmd[which(data$election == "CHE_2011")] <- 50
# Estonia uses a D'Hondt system and does not have two electoral tiers - plus, only
# 6 votes differ between the records for SMD and MMD
# I assign the MMD votes as Estonia's votes and don't analyse SMD and MMD separately
data <- data[-which(data$name == "EST_2011_SMD"), ]
data$name[which(data$name == "EST_2011_MMD")] %<>% sub("_MMD", "", .)

# making votes cast for an ally of the correct party correct

data$continuous[which(data$correct_by_alliances == TRUE)] <- 1
data$binary[which(data$correct_by_alliances == TRUE)] <- 1

data$continuous_unprejudiced[which(data$correct_by_alliances_unprejudiced == TRUE)] <- 1
data$binary_unprejudiced[which(data$correct_by_alliances_unprejudiced == TRUE)] <- 1

data$continuous_unweighted[which(data$correct_by_alliances_unweighted == TRUE)] <- 1
data$binary_unweighted[which(data$correct_by_alliances_unweighted == TRUE)] <- 1

data$continuous_proximal[which(data$correct_by_alliances__proximal == TRUE)] <- 1
data$binary_proximal[which(data$correct_by_alliances__proximal == TRUE)] <- 1

# a table of mean scores:

summarytable <- data %>% group_by(name) %>%
  dplyr::summarise(Country = country[1], Year = year[1],
            "Mean Continuous Consistency" = mean(continuous, na.rm=T),
            "SD Continuous Consistency" = sd(continuous, na.rm=T),
            "Mean Binary Consistency" = mean(binary, na.rm=T),
            "SD Binary Consistency" = sd(binary, na.rm=T),
            n = n())

variant <- summarytable$name %>% strsplit("_") %>% map(function(x) ifelse(is.na(x[3]) == FALSE, paste0("(", x[3], ")"), ""))
variant %<>% sub("R", "Round ", .)

summarytable$Year = paste0(summarytable$Year, " ", variant)

summarytable$Year[which(summarytable$name == "GRC_2015")] <- "2015 (January)"
summarytable$Year[which(summarytable$name == "GRC_2015_2")] <- "2015 (September)"


st <- summarytable %>% select(-name) %>% arrange(-`Mean Continuous Consistency`)

st$`SD Continuous Consistency` %<>% sapply(function(x) round(x, 3))
st$`Mean Continuous Consistency` %<>% sapply(function(x) round(x, 3))
st$`SD Binary Consistency` %<>% sapply(function(x) round(x, 3))
st$`Mean Binary Consistency` %<>% sapply(function(x) round(x, 3))

write.table(st, "summary_stats.txt", sep = ",", quote = FALSE, row.names = F)



# now checking for differences between mmd and smd elections in the same country

data$include <- 1

splits <- grep("MD", data$name) %>% data$election[.] %>% unique
r2splits <- grep("R1", data$name) %>% data$election[.] %>% unique

orig <- data


reorder_for_plot <- function(col, ordered) {
  names(ordered) <- c(1:length(ordered))
  col %>% recode_factor(!!!ordered)
}

smdmmd <- orig %>% filter(election %in% splits)
smdmmd$type <- c("mmd")
smdmmd$type[grep("SMD", smdmmd$name)] <- "smd"

smdmmd$type %<>% as.factor



lm(data=smdmmd, continuous ~ type) %>% anova()
lm(data=smdmmd, continuous ~ type) %>% effectsize::cohens_f()

smdmmd %>% group_by(type) %>%
  dplyr::summarise("Mean Continuous Consistency" = mean(continuous_unprejudiced, na.rm=T),
                   "SD Continuous Consistency" = sd(continuous_unprejudiced, na.rm=T))

lm(data=smdmmd, continuous_unprejudiced ~ type) %>% anova()
lm(data=smdmmd, continuous ~ type) %>% effectsize::cohens_f()

smdmmd %>% group_by(type) %>%
  dplyr::summarise("Mean Continuous Consistency" = mean(continuous_unweighted, na.rm=T),
                   "SD Continuous Consistency" = sd(continuous_unweighted, na.rm=T))

lm(data=smdmmd, continuous_unweighted ~ type) %>% anova()
lm(data=smdmmd, continuous ~ type) %>% effectsize::cohens_f()

smdmmd %>% group_by(type) %>%
  dplyr::summarise("Mean Continuous Consistency" = mean(continuous_proximal, na.rm=T),
                   "SD Continuous Consistency" = sd(continuous_proximal, na.rm=T))

lm(data=smdmmd, continuous_proximal ~ type) %>% anova()
lm(data=smdmmd, continuous ~ type) %>% effectsize::cohens_f()


# Creating a plot of said differences...

smdmmd_data <- smdmmd %>% 
  group_by(name) %>% 
  dplyr::summarise(mean = mean(continuous, na.rm=T),
                   se = sd(continuous, na.rm=T)/sqrt(n()),
                   upper = mean + se,
                   lower = mean - se,
                   election = election[1],
                   type = type[1],
                   Country = country[1],
                   Year = year[1])


smdmmd_data$formal_name <- paste0(smdmmd_data$Country, " ", smdmmd_data$Year)

smdmmd_data$formal_name %<>% reorder_for_plot(pull(arrange(filter(smdmmd_data, type == "mmd"), mean), formal_name))
smdmmd_data %<>% arrange(formal_name)

smdmmd_data2 <- Rmisc::group.CI(continuous ~ election + type, smdmmd)
smdmmd_data2 %<>% merge(smdmmd_data, by = "election")

smdmmd_data2$type.x %<>% casefold(upper=TRUE)

averages <- Rmisc::group.CI(continuous ~ type, smdmmd)

smdmmd %>% group_by(type) %>%
  dplyr::summarise(M = mean(continuous, na.rm=T),
                   SD = sd(continuous, na.rm=T))


averages %<>% transmute(election = c("Average"), type.x = type %>% casefold(upper=TRUE), continuous.upper, continuous.mean, continuous.lower, 
                    name = " ", mean = 1, se = 1, upper = 1, lower = 1, type.y = type %>% casefold(upper=TRUE), Country = "Average",
                    Year = "", formal_name = "Sample Mean", facet = "")


smdmmd_data2 %<>% cbind(facet = " ") %>% rbind(averages)

smdmmd_plot <- smdmmd_data2 %>% 
  ggplot(aes(x = formal_name, 
             y = continuous.mean, 
             ymax = continuous.upper, 
             ymin = continuous.lower,
             colour = type.x,
             shape = type.x)) + 
  geom_pointrange(position = position_dodge(width = 0.5)) + theme_half_open() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  theme(strip.background = element_blank()) +
  facet_grid (.~ facet, scales = "free_x", space = "free_x")
  
  
ggpar(smdmmd_plot, ylab = "Mean Continuous Consistency", xlab = "Country and Year", legend.title = "Election Type",
      title = "Mean Continuous Consistency by Election and Type", subtitle = "With 95% confidence intervals")

ggsave("SMDMMD_plot.png", height = 10, width = 10)


# plotting continuous and binary scores and computing their averages

graphdat <- data %>% select(continuous, binary, name) %>% melt(id.vars = "name") %>% group.CI(value ~ name + variable, .) 
g2 <- data %>% select(continuous, binary) %>% melt %>% group.CI(value ~ variable, .)

graphdat$variable %<>% (function(x) case_when(x == "continuous" ~ "Continuous Consistency",
                                              x == "binary" ~ "Binary Consistency"))

g2$variable %<>% (function(x) case_when(x == "continuous" ~ "Continuous Consistency",
                                              x == "binary" ~ "Binary Consistency"))

filter(graphdat, variable == "Continuous Consistency") %>% pull(value.mean) %>%
  summary

filter(graphdat, variable == "Binary Consistency") %>% pull(value.mean) %>%
  summary

ggplot(graphdat, aes(x = variable, y = value.mean)) + 
  geom_flat_violin(aes(fill = variable), position = position_nudge(x = 0.15, y = 0)) + 
  geom_jitter(aes(colour = variable), width = 0.1) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.1) +
  geom_point(aes(x = variable, y = value.mean),
              shape = 4, size = 4, g2) +
  ylim(0.25, 1) + coord_flip() + ylab("Mean") + xlab("Variable") + labs(title = "Continuous and Binary Correctness Scores", 
                                                                        subtitle = "Points indicate election means, crosses indicate sample means",
                                                                        colour = "Variable", fill = "Variable") +
  theme_half_open() 

ggsave("MEANS_plot.png", height = 6, width = 10)


CI(data$continuous[which(is.na(data$continuous) == FALSE)])
CI(data$binary[which(is.na(data$binary) == FALSE)])


=======
# DY note:

# This script should be run seventh

# This script produces the two graphs found in the paper

# Requires: cv_data_analysed.csv, aggregate_predictors_new.csv

library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(stats)
library(ggpubr)
library(kableExtra)

library(cowplot)
library(purrr)
library(Rmisc)
library(tidyverse)
library(ggforce)
library(PupillometryR)

rm(list = ls())
options(scipen = 99999)


data <- read.csv("cv_data_analysed.csv")
preds <- read.csv("aggregate_predictors_new.csv")

preds %<>% dplyr::select(-colnames(preds)[
  which(is.na(match(colnames(preds), colnames(data))) == FALSE)])


# separately coding the two 2015 Greek elections:
data$ELECTION %<>% as.character
data$ELECTION[which(data$ELECTION == "GRC_2015" & data$module == 5)] %<>% paste0(., "_2")
data$name %<>% as.character
data$name[which(data$name == "GRC_2015" & data$module == 5)] %<>% paste0(., "_2")
preds$election %<>% as.character
preds$election[which(preds$election == "GRC_2015" & preds$mod == 5)] %<>% paste0(., "_2")

# merging the data with the aggregate-level predictors:
data %<>% cbind(preds[match(data$ELECTION, preds$election),])

# filtering out elections with missing data (and MEX_2009 + ROU_2012 which are not HoS):
data %<>% filter(ELECTION %in% c("MNE_2012", "MNE_2016", "ROU_2012", "FRA_2007", "PER_2016",
                                 "THA_2007", "THA_2011", "MEX_2009", "BRA_2014") == FALSE)

# need to merge DEU12002 and DEU22002
data$name[which(data$ELECTION == "DEU12002")] %<>% sub("U12", "U_2",.)
data$name[which(data$ELECTION == "DEU22002")] %<>% sub("U22", "U_2",.)

deus <- c(which(data$ELECTION == "DEU12002"), which(data$ELECTION == "DEU22002"))
data$ELECTION[deus] <- "DEU_2002"
data$election[deus] <- "DEU_2002"
data$name %<>% as.character

data %>% nrow
data$election %>% unique %>% length
data$country %>% unique %>% length
data$year %>% range
data %>% filter(is.na(continuous) == FALSE) %>% nrow

data$enep[grep("R2", data$name)] <- 2

data %>% filter(binary == 0) %>% ggplot(
  aes(x = continuous, group = as.factor(round(enep, 0)), colour = as.factor(round(enep, 0)))
) + geom_density()

data %>% group_by(name) %>% dplyr::summarise(
  cont = mean(continuous, na.rm=T),
  bin = mean(binary, na.rm=T)
)


# where I have SMD and MMD data for the same election I only want to include a non-overlapping
# subset of Ss from each, proportional to the number of seats allocated by SMD/MMD in the lower house
# no data on proportions of seats in each tier in Switzerland 2011 or Estonia 2011
# Switzerland has very complex voting system where voters can vote for lists or individual
# candidates, and the results are pooled. I do a 50:50 split.
data$mmd[which(data$election == "CHE_2011")] <- 50
# Estonia uses a D'Hondt system and does not have two electoral tiers - plus, only
# 6 votes differ between the records for SMD and MMD
# I assign the MMD votes as Estonia's votes and don't analyse SMD and MMD separately
data <- data[-which(data$name == "EST_2011_SMD"), ]
data$name[which(data$name == "EST_2011_MMD")] %<>% sub("_MMD", "", .)

# making votes cast for an ally of the correct party correct

data$continuous[which(data$correct_by_alliances == TRUE)] <- 1
data$binary[which(data$correct_by_alliances == TRUE)] <- 1

data$include <- 1

splits <- grep("MD", data$name) %>% data$election[.] %>% unique
r2splits <- grep("R1", data$name) %>% data$election[.] %>% unique

orig <- data


reorder_for_plot <- function(col, ordered) {
  names(ordered) <- c(1:length(ordered))
  col %>% recode_factor(!!!ordered)
}

smdmmd <- orig %>% filter(election %in% splits)
smdmmd$type <- c("mmd")
smdmmd$type[grep("SMD", smdmmd$name)] <- "smd"

smdmmd_data <- smdmmd %>% 
  group_by(name) %>% 
  dplyr::summarise(mean = mean(continuous, na.rm=T),
                   se = sd(continuous, na.rm=T)/sqrt(n()),
                   upper = mean + se,
                   lower = mean - se,
                   election = election[1],
                   type = type[1],
                   Country = country[1],
                   Year = year[1])


smdmmd_data$formal_name <- paste0(smdmmd_data$Country, " ", smdmmd_data$Year)

smdmmd_data$formal_name %<>% reorder_for_plot(pull(arrange(filter(smdmmd_data, type == "mmd"), mean), formal_name))
smdmmd_data %<>% arrange(formal_name)

smdmmd_data2 <- Rmisc::group.CI(continuous ~ election + type, smdmmd)
smdmmd_data2 %<>% merge(smdmmd_data, by = "election")

smdmmd_data2$type.x %<>% casefold(upper=TRUE)

smdmmd_plot <- smdmmd_data2 %>% 
  ggplot(aes(x = formal_name, 
             y = continuous.mean, 
             ymax = continuous.upper, 
             ymin = continuous.lower,
             colour = type.x,
             shape = type.x)) + 
  geom_pointrange(position = position_dodge(width = 0.5)) + theme_half_open() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
ggpar(smdmmd_plot, ylab = "Mean Continuous Consistency", xlab = "Country and Year", legend.title = "Election Type",
      title = "Mean Continuous Consistency by Election and Type", subtitle = "With 95% confidence intervals")

ggsave("SMDMMD_plot.png", height = 10, width = 10)


colnames(data) %<>% ifelse(. == "continuous", "cont", .)



codes <- c("Au", "Be", "Cn", "Hv", "Cz", "Dn",
           "Sp", "GB", "Nl", "Pl", "Pr", "Sv",
           "Sw", "Bu", "Fi", "Ir", "Ic", "It",
           "No", "At", "Gr", "Cr", "Is", "Lt",
           "Sl", "Tu", "SA", "Al", "Ge", "Hu",
           "NZ", "RK", "Li", "Ja", "Es", "Ch",
           "Tw", "Ph", "Mx", "Ro", "US", "Pe",
           "Ke", "Fr", "Br", "Ur", "Ar", "Se")

names(codes) <- data$country %>% unique

cn <- data$country %>% recode_factor(!!!codes) %>% as.character

yr <- data$year %>% as.character %>% strsplit("") %>%
  map(function(x) paste0(x[3], x[4]))

add <- data$name %>% strsplit("_") %>% map(function(x) x[3] %>% ifelse(is.na(.) == FALSE, ., ""))

data$code = paste0(cn, yr, add)

graphdat <- data %>% select(continuous, binary, name) %>% melt(id.vars = "name") %>% group.CI(value ~ name + variable, .) 
g2 <- data %>% select(continuous, binary) %>% melt %>% group.CI(value ~ variable, .)

graphdat$variable %<>% (function(x) case_when(x == "continuous" ~ "Continuous Consistency",
                                              x == "binary" ~ "Binary Consistency"))

g2$variable %<>% (function(x) case_when(x == "continuous" ~ "Continuous Consistency",
                                              x == "binary" ~ "Binary Consistency"))
filter(graphdat, variable == "Continuous Consistency") %>% pull(value.mean) %>%
  summary

filter(graphdat, variable == "Binary Consistency") %>% pull(value.mean) %>%
  summary

ggplot(graphdat, aes(x = variable, y = value.mean)) + 
  geom_flat_violin(aes(fill = variable), position = position_nudge(x = 0.15, y = 0)) + 
  geom_jitter(aes(colour = variable), width = 0.1) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.1) +
  geom_point(aes(x = variable, y = value.mean),
              shape = 4, size = 4, g2) +
  ylim(0.25, 1) + coord_flip() + ylab("Mean") + xlab("Variable") + labs(title = "Continuous and Binary Correctness Scores", 
                                                                        subtitle = "Points indicate election means, crosses indicate sample means",
                                                                        colour = "Variable", fill = "Variable") +
  theme_half_open() 

ggsave("MEANS_plot.png", height = 6, width = 10)

>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
