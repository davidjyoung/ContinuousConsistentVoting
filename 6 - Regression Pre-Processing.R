<<<<<<< HEAD

# this script should be run sixth

# this script gathers the data together required for the regression in stata,
# getting all the variables into all their final format

# requires: cv_data_analysed.csv, aggregate_predictors_new.csv
# outputs: cut_for_stata.dta, cut_for_stata_cses1-2.csv, cut_for_stata_cses1-4.csv

library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(stats)
library(ggpubr)

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

data$include <- 1

data$enep[grep("R2", data$name)] <- 2 # enep for Round 2 elections is always 2 

splits <- grep("MD", data$name) %>% data$election[.] %>% unique
r2splits <- grep("R1", data$name) %>% data$election[.] %>% unique


for (i in 1:length(splits)) {
  
  inds <- which(data$election == splits[i])
  SMDs <- grep("SMD", data$name[inds])
  MMDs <- grep("MMD", data$name[inds])
  mmd_number <- data$mmd[inds] %>% divide_by(100) %>% multiply_by(length(MMDs)) %>% round
  
  set.seed(0107)
  mmd_selection <- sample(
    1:length(MMDs), mmd_number
  )
  mmd_unselection <- setdiff(1:length(MMDs), mmd_selection) 
  
  data$include[inds[MMDs][mmd_unselection]] <- 0
  data$include[inds[SMDs][mmd_selection]] <- 0
}


for (i in 1:length(r2splits)) {
  indsr2 <- which(data$election == r2splits[i])
  r2s <- grep("R2", data$name[indsr2])
  r1s <- grep("R1", data$name[indsr2])
  number <- length(indsr2)/4
  
  set.seed(0107)
  r1s_selection <- sample(
    1:length(r1s), number
  )
  r1s_unselection <- setdiff(1:length(r1s), r1s_selection) 
  
  data$include[indsr2[r1s][r1s_unselection]] <- 0
  data$include[indsr2[r2s][r1s_selection]] <- 0
  
}

data %<>% filter(include == 1) # this splits the countries with SMD and MMD elections into separate samples


data$close_to_a_party <- 
  dplyr::select(data, contains("close_")) %>% 
  apply(1, function(x) ifelse(any(is.na(x) == FALSE), 1, 0)) %>%
  as.numeric()


# need to take the log of yrsdem
data$logyrsdem <- data$yrsdemoc %>% log
data$logyrsdem[which(is.infinite(data$logyrsdem) == TRUE)] <- 0


data$specific_formula <- data$first_tier_formula
data$specific_formula[grep("_MMD", data$name)] <- data$second_tier_formula[grep("_MMD", data$name)]
data$proportionality <- ifelse(data$overall_formula %in% c("MIXED", "PROPORTIONAL"), 1, 0)


scaled <- dplyr::select(data, PPI, enep, clearlines, pers_cut, media, affpol, logyrsdem, disproportionality) %>%
  transmute_all(function(x) scale(x) %>% as.numeric)
colnames(scaled) %<>% paste0(., "_scaled")

  
cut <- dplyr::select(data, ELECTION, SAMPLE_WEIGHTS, polsoph, EFFICACY, AGE,
                     IDEOLOGY, GDPdiff, demsatis, proportionality, ideorange,
                     contains("voted"), contains("binary"), contains("continuous"), country, year, mod,
                     vote_share_type, mean_ideology, PPI, enep, clearlines, pers_cut, media, overall_formula,
                     affpol, close_to_a_party, logyrsdem, name) %>% cbind(scaled)

cut$ELECTION %>% unique %>% length # 146 elections
cut$country %>% unique %>% length # 47
cut %>% nrow # 258,714
cut$year %>% range # 1996-2018

lm(continuous ~ PPI + enep + clearlines + pers_cut + 
     media + proportionality + logyrsdem, cut) %>% car::vif() # all under 5
lm(continuous ~ polsoph + AGE + EFFICACY, cut) %>% car::vif() # all under 5


haven::write_dta(cut, "cut_for_stata.dta")
haven::write_dta(filter(cut, mod < 5), "cut_for_stata_cses1-4.dta")
haven::write_dta(filter(cut, mod < 3), "cut_for_stata_cses1-2.dta")

election_levels <- cut %>% select(country, year, name, proportionality, PPI:media) %>% 
  group_by(country, year, name) %>%
  summarise_all(mean)

write.table(election_levels, "election_levels.txt", sep = ",", quote = FALSE, row.names = F)


election_levels %>% ungroup %>% dplyr::select(proportionality:media) %>%
  transmute_all(as.numeric) %>%
  cor.ci(., use="pairwise.complete.obs")

ggsave("corplot.png")

=======

# this script should be run sixth

# this script gathers the data together required for the regression in stata,
# getting all the variables into all their final format

# requires: cv_data_analysed.csv, aggregate_predictors_new.csv
# outputs: cut_for_stata.dta, cut_for_stata_cses1-2.csv, cut_for_stata_cses1-4.csv

library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(stats)
library(lme4)
library(lmerTest)
library(stargazer)
library(jtools)
library(heavy)
library(robustlmm)
library(ggpubr)

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

data$enep[grep("R2", data$name)] <- 2

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
                   type = type[1])

smdmmd_data$election %<>% reorder_for_plot(pull(arrange(filter(smdmmd_data, type == "mmd"), mean), election))
smdmmd_data %<>% arrange(election)

smdmmd_plot <- smdmmd_data %>% 
  ggplot(aes(x = election, 
             y = mean, ymax = upper, ymin = lower,
             colour = type)) + 
  geom_pointrange() + coord_flip() + theme_linedraw()
ggpar(smdmmd_plot, ylab = "Mean continuous correctness", xlab = "", legend.title = "")
dev.copy(pdf, "correctness_by_smd_vs_mmd.pdf")
dev.off()

smdmmd$type %<>% as.factor
smdmmd %>% group_by(type) %>% dplyr::summarise(mean = mean(continuous, na.rm=T),
                                               se = sd(continuous, na.rm=T)/sqrt(n()),
                                               binary = mean(binary, na.rm=T))

lm(data=smdmmd, continuous ~ type) %>% anova()
lm(data=smdmmd, continuous ~ type) %>% summary
glm(data=smdmmd, family=binomial(link = "logit"), binary ~ type) %>% summary

smdmmd$id[which(smdmmd$type == "smd")] <- c(1:length(which(smdmmd$type == "smd")))
smdmmd$id[which(smdmmd$type == "mmd")] <- c(1:length(which(smdmmd$type == "mmd")))

banned <- union(filter(smdmmd, type == "smd", is.na(continuous) == TRUE) %>% pull(id),
                filter(smdmmd, type == "mmd", is.na(continuous) == TRUE) %>% pull(id))

smdmmd %>% filter(id %in% banned == FALSE) %>%
  group_by(name, type) %>% dplyr::summarise(mean = mean(continuous),
                                                 n = n()) %>% View



r1r2 <- orig %>% filter(election %in% r2splits)
r1r2$type <- c("r2")
r1r2$type[grep("R1", r1r2$name)] <- "r1"

r1r2_data <- r1r2 %>% 
  group_by(name) %>% 
  dplyr::summarise(mean = mean(continuous, na.rm=T),
                   se = sd(continuous, na.rm=T)/sqrt(n()),
                   upper = mean + se,
                   lower = mean - se,
                   election = election[1],
                   type = type[1])

r1r2_data$election %<>% reorder_for_plot(pull(arrange(filter(r1r2_data, type == "r1"), mean), election))
r1r2_data %<>% arrange(election)

r1r2_plot <- r1r2_data %>% 
  ggplot(aes(x = election, 
             y = mean, ymax = upper, ymin = lower,
             colour = type)) + 
  geom_pointrange() + coord_flip() + theme_linedraw()
ggpar(r1r2_plot, ylab = "Mean continuous correctness", xlab = "", legend.title = "")
dev.copy(pdf, "correctness_by_r2_vs_r1.pdf")
dev.off()

r1r2$type %<>% as.factor
r1r2 %>% group_by(type) %>% dplyr::summarise(mean = mean(continuous, na.rm=T),
                                             se = sd(continuous, na.rm=T)/sqrt(n()),
                                             binary = mean(binary, na.rm=T))

lm(data=r1r2, continuous ~ type) %>% anova()
lm(data=r1r2, continuous ~ type) %>% summary
glm(data=r1r2, family=binomial(link = "logit"), binary ~ type) %>% summary


means <- data %>% group_by(name) %>% dplyr::summarise(
  country = country[1],
  year = year[1],
  module = module[1],
  binary = mean(binary, na.rm=T),
  mean = mean(continuous, na.rm=T),
  se = sd(continuous, na.rm=T)/sqrt(n()),
  upper = mean + se,
  lower = mean - se)
means$name %<>% reorder_for_plot(arrange(means, mean) %>% pull(name))


means %>% dplyr::select(name, binary, mean) %>% melt(id.vars = "name") %>%
  ggplot(aes(x = name, y = value, colour = variable)) + geom_point() + coord_flip()
dev.copy(pdf, "mean_vs_binary.pdf")
dev.off()

data %>% group_by(name) %>% dplyr::summarise(
  continuous_correctness = mean(continuous, na.rm=T),
  sd = sd(continuous, na.rm=T),
  binary = mean(binary, na.rm=T),
  N = n()
) %>% arrange(desc(continuous_correctness)) %>% kableExtra::kable("html", align = "l")


for (i in 1:length(splits)) {
  
  inds <- which(data$election == splits[i])
  SMDs <- grep("SMD", data$name[inds])
  MMDs <- grep("MMD", data$name[inds])
  mmd_number <- data$mmd[inds] %>% divide_by(100) %>% multiply_by(length(MMDs)) %>% round
  
  set.seed(0107)
  mmd_selection <- sample(
    1:length(MMDs), mmd_number
  )
  mmd_unselection <- setdiff(1:length(MMDs), mmd_selection) 
  
  data$include[inds[MMDs][mmd_unselection]] <- 0
  data$include[inds[SMDs][mmd_selection]] <- 0
}


for (i in 1:length(r2splits)) {
  indsr2 <- which(data$election == r2splits[i])
  r2s <- grep("R2", data$name[indsr2])
  r1s <- grep("R1", data$name[indsr2])
  number <- length(indsr2)/4
  
  set.seed(0107)
  r1s_selection <- sample(
    1:length(r1s), number
  )
  r1s_unselection <- setdiff(1:length(r1s), r1s_selection) 
  
  data$include[indsr2[r1s][r1s_unselection]] <- 0
  data$include[indsr2[r2s][r1s_selection]] <- 0
  
}

data %<>% filter(include == 1) # this splits the countries with SMD and MMD elections into separate samples


data$close_to_a_party <- 
  dplyr::select(data, contains("close_")) %>% 
  apply(1, function(x) ifelse(any(is.na(x) == FALSE), 1, 0)) %>%
  as.numeric()


# need to take the log of yrsdem
data$logyrsdem <- data$yrsdemoc %>% log
data$logyrsdem[which(is.infinite(data$logyrsdem) == TRUE)] <- 0


data$specific_formula <- data$first_tier_formula
data$specific_formula[grep("_MMD", data$name)] <- data$second_tier_formula[grep("_MMD", data$name)]
data$proportionality <- ifelse(data$overall_formula %in% c("MIXED", "PROPORTIONAL"), 1, 0)


scaled <- dplyr::select(data, PPI, enep, clearlines, pers_cut, media, affpol, logyrsdem, disproportionality) %>%
  transmute_all(function(x) scale(x) %>% as.numeric)
colnames(scaled) %<>% paste0(., "_scaled")

  
cut <- dplyr::select(data, ELECTION, SAMPLE_WEIGHTS, polsoph, EFFICACY, AGE,
                     IDEOLOGY, GDPdiff, demsatis, proportionality, ideorange, contains("discriminability"),
                     contains("voted"), contains("binary"), contains("continuous"), country, year, mod,
                     vote_share_type, mean_ideology, PPI, enep, clearlines, pers_cut, media, overall_formula,
                     affpol, close_to_a_party, logyrsdem, name) %>% cbind(scaled)

colnames(cut)[
  which(colnames(cut) %in% c("discriminability_proximal_weighted", "discriminability_proximal_unweighted"))
] <- c("discriminability_prox_weighted", "discriminability_prox_unweighted")

haven::write_dta(cut, "cut_for_stata.dta")
haven::write_dta(filter(cut, mod < 5), "cut_for_stata_cses1-4.dta")
haven::write_dta(filter(cut, mod < 3), "cut_for_stata_cses1-2.dta")


>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
