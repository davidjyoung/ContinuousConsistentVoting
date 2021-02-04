<<<<<<< HEAD
# DY note:

# this script should be run fifth

# this script constructs the predictors for the regression analyses
# by applying functions to external datasets and cses data and merging with
# the existing data

# Requires: cv_data_analysed.csv, votevariablestable.csv, 
#           gallagherindices.xlsx, POLITYIV.xls, DPI2015_basefile.v5.xlsx,
#           ESPV.xls, internet_users_percentage.csv,
#           mobile_subscriptions_percentage.csv, newspapers.csv
            
# Outputs: aggregate_predictors_new.csv


library(readxl)
library(haven)
library(ggplot2)
library(varhandle)
library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(stats)


rm(list = ls())

options(scipen = 999)

CSES1 <- read_sav("cses1.sav")
CSES2 <- read_sav("cses2.sav")
CSES3 <- read_sav("cses3.sav")
CSES4 <- read_sav("cses4.sav")
CSES5 <- read_sav("cses5.sav")



fetch <- function(X, df) {
  df[, grep(X, sapply(df, function(x) attr(x, "label")))]
}

rename_cols <- function(df, v) {
  colnames(df) <- v
  df
}

gamut_col <- function(X) {
  rbind.data.frame(fetch(X, CSES1) %>% rename_cols("X"), 
  fetch(X, CSES2) %>% rename_cols("X"), 
  fetch(X, CSES3) %>% rename_cols("X"), 
  fetch(X, CSES4) %>% rename_cols("X"), 
  fetch(X, CSES5) %>% rename_cols("X"), stringsAsFactors = FALSE) %>% as.vector()
}



rename_ps <- function(df, type) {
  v <- LETTERS[1:9] %>% sapply(function(x) paste0(x, "_", type))
  rename_cols(df, v)
}

vote_clean <- function(df) {
  transmute_all(df, 
    function(x) ifelse(x > 100, NA, x))
}


CSES1_ST_PERC <- 100 * (fetch("NUMBER OF SEATS - LOWER - 2ND SEGMENT", CSES1) / (fetch("NUMBER OF SEATS - LOWER - 1ST SEGMENT", CSES1) + fetch("NUMBER OF SEATS - LOWER - 2ND SEGMENT", CSES1)))
CSES2_ST_PERC <- 100 * (fetch("NUMBER OF SEATS-LOWER-2ND SEGMENT", CSES2) / (fetch("NUMBER OF SEATS-LOWER-1ST SEGMENT", CSES2) + fetch("NUMBER OF SEATS-LOWER-2ND SEGMENT", CSES2)))
MMD_PERC <- c(CSES1_ST_PERC$A5027_2 %>% as.numeric,
              CSES2_ST_PERC$B5033_2 %>% as.numeric,
              CSES3$C5073, CSES4$D5073, CSES5$E5070)

data <- data.frame(
           election = c(gamut_col("ALPHA")),
           country = c(gamut_col("POLITY NAME")),
           year = c(gamut_col("ELECTION YEAR")),
           mod = c(rep(1, times = nrow(CSES1)),
                   rep(2, times = nrow(CSES2)),
                   rep(3, times = nrow(CSES3)),
                   rep(4, times = nrow(CSES4)),
                   rep(5, times = nrow(CSES5))),
            rbind.data.frame(cbind(
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES1), G = NA, H = NA, I = NA) %>% 
                      rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES2) %>% rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES3) %>% rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES4) %>% rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES5) %>% rename_ps("LH") %>% vote_clean),
            rbind.data.frame(cbind(
                   fetch("PERCENT VOTE - PRESIDENT", CSES1), G = NA, H = NA, I = NA) %>% 
                      rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES2) %>% rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES3) %>% rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES4) %>% rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES5) %>% rename_ps("PRES") %>% vote_clean),
           mmd = MMD_PERC, stringsAsFactors = FALSE) %>% unique
colnames(data)[1:3] <- c("election", "country", "year")


data <- data[-which(data$country == "Portugal" & data$year == 2002 & data$mod == 2), ] 
# Portugal 2002 is coded in Modules 1 and 2 (identically), I take the data from Module 1
data %<>% filter(country %in% c("Thailand", "Montenegro") == FALSE) 
# Thailand not a democracy, Montenegro lacks data later on




# need to manually add in presidential vote share data for Romania 2014 # taken from wikipedia
data[which(data$election == "ROU_2014"), grep("_PRES", colnames(data))] <-
  c(40.44, 30.37, 5.20, 4.03, 3.68, 3.47, NA, 5.36, 4.44)

# adding in some more data from other sources:

# ideology:
ideologies <- read.csv("cv_data_analysed.csv") %>% dplyr::select(ELECTION, contains("ideo_")) %>% unique
data %<>% filter(election %in% ideologies$ELECTION)
data %<>% cbind.data.frame(ideologies[match(data$election, ideologies$ELECTION),] %>% dplyr::select(-ELECTION))

# vote share type:
vvt <- read.csv("votevariablestable.csv")
data$vote_share_type <- vvt[match(data$election, vvt$ELECTION),] %>% pull(votes)

# for two Presidential elections, the data are in the lower house columns: 
data$vote_share_type[which(data$election %in% c("ARG_2015", "PER_2016"))] <- "lower house"


# now adding in the party polarization index:

data$mean_ideology <- dplyr::select(data, contains("ideo")) %>% rowMeans(na.rm=T) %>% as.numeric


ppi_ind <- function(L, type, elec, module) {
  filt <- data %>% filter(election == elec, mod == module) %>% slice(1)
  
  vs <- as.numeric(dplyr::select(filt, paste0(L, case_when(type == "lower house" ~ "_LH", type == "presidential" ~ "_PRES"))))
  ideo <- as.numeric(dplyr::select(filt, paste0("ideo_", L)))
  mean <- dplyr::select(filt, mean_ideology)
  
  subtract(ideo, mean) %>% divide_by(5) %>% raise_to_power(2) %>% multiply_by(vs)
}

PPI <- function(elec, module) {
LETTERS[1:9] %>% 
    sapply(function(x) 
      ppi_ind(x, as.character(pull(filter(data, election == elec), vote_share_type)[1]), elec, module)) %>% 
    unlist %>% sum(na.rm=T) %>% raise_to_power(1/2)
}

data$PPI <- mapply(PPI, elec = data$election, module = data$mod) %>% as.numeric

# now adding in the enep:

enep <- function(elec, module) {
  type <- data %>% filter(election == elec) %>% slice(1) %>% pull(vote_share_type)
  data %>% filter(election == elec, mod == module) %>% slice(1) %>%
  dplyr::select(contains(case_when(
                  type == "lower house" ~ "_LH", 
                  type == "presidential" ~ "_PRES"))) %>%
    divide_by(100) %>% raise_to_power(2) %>% sum(na.rm=T) %>% 
    divide_by(1, .)
}

data$enep <- mapply(enep, elec = data$election, module = data$mod) %>% as.numeric %>% 
  ifelse(is.infinite(.), NA, .)

# below, R2 elections are assigned eneps of 2, 



# Years of democracy since 1955

Pol4 <- read_excel("POLITYIV.xls")

rename_pol4 <- c("United States", "United Kingdom", "Korea South", "Slovak Republic", "Czechoslovakia")

Pol4$country %<>% (function(x) ifelse(x %in% rename_pol4, 
                                     case_when(x == "United States" ~ "United States of America",
                                               x == "United Kingdom" ~ "Great Britain",
                                               x == "Korea South" ~ "Republic of Korea",
                                               x == "Slovak Republic" ~ "Slovakia",
                                               x == "Czechoslovakia" ~ "Czech Republic"), 
                                     x))

Pol4$sub <- 1955 - Pol4$byear
ref <- filter(Pol4, sub >= 0) %>% group_by(country) %>% dplyr::summarise(year = last(byear))

yrsdemoc <- function(elec) {
  
  C <- filter(data, election == elec) %>% pull(country)
  Y <- filter(data, election == elec) %>% pull(year)
  
  if (C == "Iceland") return(Y - 1955)

  ref_year <- ifelse(C %in% ref$country, 
                 pull(filter(ref, country == C), year), 
                 first(pull(filter(Pol4, country == C), byear)))
 
  filt <- filter(Pol4, country == C, byear >= ref_year, polity >= 6)
  
  filt$eyear[which(filt$present == 1)] <- 2018
  
  total <- mutate(filt, period = eyear - byear) %>% pull(period) %>% sum
  
  startdiff <- ifelse(first(filt$byear) >= 1955, 0, first(filt$byear) - 1955)
  enddiff <- Y - 2018
  
  sum(total, startdiff, enddiff)
}

data$yrsdemoc <- sapply(data$election, yrsdemoc)


# Clear Lines of Responsibility

DPI <- read_excel("DPI2015_basefile.v5.xlsx")

rename_dpi <- c("Czech Rep.", "FRG/Germany", "UK", "ROK", "USA", "S. Africa")

DPI$countryname %<>% (function(x) ifelse(
  x %in% rename_dpi,
  case_when(x == "Czech Rep." ~ "Czech Republic",
            x == "FRG/Germany" ~ "Germany",
            x == "UK" ~ "Great Britain",
            x == "ROK" ~ "Republic of Korea",
            x == "USA" ~ "United States of America",
            x == "S. Africa" ~ "South Africa"),
  x))

dpi <- function(elec) {
  
  if (elec == "SRB_2012") return(NA)
  
  C <- filter(data, election == elec) %>% slice(1) %>% pull(country)
  Y <- filter(DPI, countryname == C, year < pull(slice(filter(data, election == elec), 1), year)) %>% 
    pull(year) %>% last()
  
  filt <- DPI %>% filter(countryname == C, year == Y)
  
  if (filt$system == 2) {
    if (filt$gov1seat/filt$totalseats > 0.5) {
      return(1)
    } else {
      return(0)
    }
  } 
  
  if (filt$system %in% c(0, 1)) {
    return(filt$allhouse)
  }
  
}

data$clearlines <- sapply(data$election, dpi)



# I have checked and the situation was the same for both Greek 2015 elections


# Personal vote incentives

PERS <- read_excel("ESPV.xls")
colnames(PERS) <- PERS[1, ]

PERS <- PERS[3:nrow(PERS) , which(colnames(PERS) %in% c("Country", "Year", "pers_rank"))]

PERS$Country %<>% (function(x) ifelse(
  x %in% c("United Kingdom", "Korea (South, Republic of Korea)", "Taiwan (Republic of China)"),
  case_when(x == "United Kingdom" ~ "Great Britain",
            x == "Korea (South, Republic of Korea)" ~ "Republic of Korea",
            x == "Taiwan (Republic of China)" ~ "Taiwan"),
  x))


pers <- function(elec) {
  
  C <- pull(slice(filter(data, election == elec), 1), country)
  Y <- pull(slice(filter(data, election == elec), 1), year) 
  
  if (Y > 2005) {
    Y <- filter(PERS, Country == C, is.na(as.numeric(pers_rank)) == FALSE) %>% pull(Year) %>% last
  }
  
  PERS %>% filter(Country == C, Year == Y) %>% pull(pers_rank)
}

data$pers <- sapply(data$election, pers) %>% as.numeric

# missing data from Turkey, Spain

# Lau et al. award Spain 2
# I compare Turkey to similar countries (below) and award it 2 also

data$pers[which(data$country %in% c("Spain", "Turkey"))] <- 2

PERS2 <- read_excel("ESPV.xls")
colnames(PERS2) <- PERS2[2, ]

Turk <- filter(PERS2, country == "Turkey", year == 2000) 

Turk_sims <- filter(PERS2, bicameral == Turk$bicameral,
                    oneparty == Turk$oneparty,
                    propn == Turk$propn,
                    Propmmd == Turk$Propmmd,
                    propsmd == Turk$propsmd,
                    propcoded == Turk$propcoded,
                    multitier1 == Turk$multitier1,
                    tiervote1 == Turk$tiervote1,
                    indy == Turk$indy,
                    rank_vote == Turk$rank_vote,
                    multiround1 == Turk$multiround1,
                    year >= 2000)

Check <- Turk_sims$pers_rank %>% as.numeric(.) %>% mean(., na.rm = TRUE)

"As this scale is at best ordinal, we looked at its distribution across our 
sixty-nine elections and dichotomized it at a noticeable break in the distribution, 
which singles out the twenty-nine per cent of the cases with the highest incentives 
for a personal vote"

median_split <- function(x) {
  ifelse(x <= median(x, na.rm=T), 1, 2)
}

data$pers_mediansplit <- median_split(data$pers)



# cut-off should be 10:
data$pers %>% quantile(0.71, na.rm=T) # 10
data %>% filter(mod %in% c(1, 2)) %>% pull(pers) %>% quantile(0.71, na.rm=T) # 10

data$pers_cut <- ifelse(data$pers >= 10, 1, 0)

# media penetration:

internet <- read.csv("internet_users_percentage.csv")
mobile <- read.csv("mobile_subscriptions_percentage.csv")
np <- read.csv("newspapers.csv")

np$Reference.Area %<>% as.character
np$Reference.Area[which(np$Reference.Area == "United Kingdom of Great Britain and Northern Ireland")] <- c("Great Britain")


#need to recode some names to align

internet$Country.Name <- internet[,1] %>% as.character
mobile$Country.Name <- mobile[,1] %>% as.character

recoders <- c("United Kingdom", "Korea, Rep.", "United States", "Slovak Republic")
recodey <- (function(x) ifelse(x %in% recoders, case_when(
  x == "United Kingdom" ~ "Great Britain",
  x == "Korea, Rep." ~ "Republic of Korea",
  x == "United States" ~ "United States of America",
  x == "Slovak Republic" ~ "Slovakia"
), x))

internet$Country.Name %<>% recodey
mobile$Country.Name %<>% recodey

# 2004 internet data for Australia is missing so I replace it with the 2005 reading

internet[
  which(internet$Country.Name == "Australia"), 
  which(colnames(internet) == "X2004")] <- internet[
    which(internet$Country.Name == "Australia"), 
    which(colnames(internet) == "X2005")]


media <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  I <- internet %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
  M <- mobile %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
  
  filt_np <- np %>% filter(Reference.Area == C)
  N <- filt_np$Observation.Value[which.min(Mod(pull(filt_np, Time.Period) - Y))]
  
  data.frame(internet = I, mobile = M, newspapers = N)
}

newspapers <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  filt_np <- np %>% filter(Reference.Area == C)
  filt_np$Observation.Value[which.min(Mod(pull(filt_np, Time.Period) - Y))]
}
mobiles <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  M <- mobile %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
}
internets <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  I <- internet %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
}


data$mobiles <- data$election %>% sapply(mobiles) %>% (function(x) (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T)))
data$internet <- data$election %>% sapply(internets) %>% (function(x) (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T)))
data$newspapers <- data$election %>% sapply(newspapers) %>% (function(x) (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T)))


data$media <- dplyr::select(data, mobiles, internet, newspapers) %>% 
  apply(1, function(x) mean(x, na.rm=T)) %>% as.numeric



recode_with_labels <- function(x) {
  x %>% factor(levels = attr(x, "labels") %>% as.numeric,
               labels = attr(x, "labels") %>% attr(., "names")) %>% as.character
  
}

coded <- function(df, X) {
  which(sapply(df, function(x) attr(x, "label")) == X) %>% attr(.,"name") %>% pull(df, .)
}

tiers <- rbind.data.frame(
  data.frame(
    first = coded(CSES1, "ELECTORAL FORMULA - LOWER - 1ST SEGMENT") %>% as.numeric,
    second = coded(CSES1, "ELECTORAL FORMULA - LOWER - 2ND SEGMENT") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES2, "ELECTORAL FORMULA-LOWER-1ST SEGMENT") %>% as.numeric,
    second = coded(CSES2, "ELECTORAL FORMULA-LOWER-2ND SEGMENT") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES3, "ELECTORAL FORMULA - LOWEST TIER - LOWER HOUSE") %>% as.numeric,
    second = coded(CSES3, "ELECTORAL FORMULA - SECOND TIER - LOWER HOUSE") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES4, "ELECTORAL FORMULA - LOWEST TIER - LOWER HOUSE") %>% as.numeric,
    second = coded(CSES4, "ELECTORAL FORMULA - SECOND TIER - LOWER HOUSE") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES5, "ELECTORAL FORMULA - LOWEST SEGMENT (TIER) - LOWER HOUSE") %>% as.numeric,
    second = coded(CSES5, "ELECTORAL FORMULA - SECOND SEGMENT (TIER) - LOWER HOUSE") %>% as.numeric
  ))

tiers %<>% data.frame(
  country = c(gamut_col("POLITY NAME")),
  year = c(gamut_col("ELECTION YEAR"))) %>% unique

recode_formulae <- c(
  "11" = "PLURALITY - SINGLE MEMBER DISTRICTS",
  "12" = "PROPORTIONAL - MULTI MEMBER DISTRICTS",
  "20" = "MAJORITY - UNSPECIFIED",
  "21" = "MAJORITY - RUN-OFF",
  "22" = "MAJORITY - ALTERNATIVE",
  "30" = "PROPORTIONAL - UNSPECIFIED",
  "31" = "PROPORTIONAL - D'HONDT",
  "32" = "PROPORTIONAL - LARGEST REMAINDER - DROOP",
  "33" = "PROPORTIONAL - LARGEST REMAINDER - HARE",
  "34" = "PROPORTIONAL - STE-LAGUE"
)

tiers$first %<>% recode_factor(!!!recode_formulae)
tiers$second %<>% recode_factor(!!!recode_formulae)

tierify <- function(x, y) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  filter(tiers, X == C, X.1 == Y) %>% pull(y) %>% as.character
}

data$first_tier_formula <- sapply(data$election, function(x) tierify(x, "first"))
data$second_tier_formula <- sapply(data$election, function(x) tierify(x, "second"))

data$first_tier_formula_type <- data$first_tier_formula %>% sapply(function(x) strsplit(x, " - ") %>% unlist %>% first()) %>% as.character
data$second_tier_formula_type <- data$second_tier_formula %>% sapply(function(x) strsplit(x, " - ") %>% unlist %>% first()) %>% as.character

classifier <- function(f, s) {
  if (is.na(s) == TRUE | (s == f)) { 
    return(f) } else {
     return("MIXED")
    }
}

data$overall_formula <- mapply(classifier, 
       f = data$first_tier_formula_type, 
       s = data$second_tier_formula_type) %>% as.character

data %>% filter(overall_formula == "MIXED", vote_share_type == "lower house") %>%
  pull(election)

# all mixed-systems votes have been accounted for except those in Slovenia, but 
# voting data is only provided for the first tier
# in all cases, the first tier is SMD and the second is MMD

tabulate <- function(df, n) {
  tab <- dplyr::select(df, contains(n)) 
  colnames(tab) <- c(LETTERS[1:9])
  tab
}

likes <- rbind.data.frame(
  tabulate(CSES1, "A3020"), 
  tabulate(CSES2, "B3037"),
  tabulate(CSES3, "C3009"), 
  tabulate(CSES4, "D3011"), 
  tabulate(CSES5, "E3017")
) %>% transmute_all(function(x) ifelse(x > 10, NA, x)) %>% data.frame

likes$election = c(CSES1$A1004, CSES2$B1004, CSES3$C1004, CSES4$D1004, CSES5$E1004)
likes$module = c(rep(1, nrow(CSES1)), rep(2, nrow(CSES2)), 
                 rep(3, nrow(CSES3)), rep(4, nrow(CSES4)),
                 rep(5, nrow(CSES5)))


proportions <- function(x) {
  length(which(x >= 8))/length(x) * length(which(x <= 2))/length(x)
}

affpol <- function(e, m) {
  filt <- filter(likes, election == e, module == m)
  enep <- filter(data, election == e, mod == m) %>% pull(enep)
  
  LETTERS[1:9] %>% 
    sapply(function(x) pull(filt, x) %>% proportions) %>% 
    sum(na.rm=T) %>% divide_by(., 0.25 * enep)
}

data$affpol <- mapply(affpol, e = data$election, m = data$mod)



gi <- read_excel("gallagherindices.xlsx")

gi$country[which(gi$country == "South Korea")] <- "Republic of Korea"
gi$country[which(gi$country == "United States")] <- "United States of America"

gallagise <- function(elec) {

  yr <- filter(data, election == elec) %>% pull(year)
  ctry <- filter(data, election == elec) %>% pull(country)
  
  if (ctry == "Taiwan") {yr = 2020} else {}
  
  filter(gi, country == ctry, year < yr) %>% pull(lsq) %>% median
}

data$disproportionality <- data$election %>% sapply(gallagise)


write.csv(data, "aggregate_predictors_new.csv")



=======
# DY note:

# this script should be run fifth

# this script constructs the predictors for the regression analyses
# by applying functions to external datasets and cses data and merging with
# the existing data

# Requires: cv_data_analysed.csv, votevariablestable.csv, 
#           gallagherindices.xlsx, POLITYIV.xls, DPI2015_basefile.v5.xlsx,
#           ESPV.xls, internet_users_percentage.csv,
#           mobile_subscriptions_percentage.csv, newspapers.csv
            
# Outputs: aggregate_predictors_new.csv



library(readxl)
library(haven)
library(ggplot2)
library(varhandle)
library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(stats)


rm(list = ls())

options(scipen = 999)

CSES1 <- read_sav("cses1.sav")
CSES2 <- read_sav("cses2.sav")
CSES3 <- read_sav("cses3.sav")
CSES4 <- read_sav("cses4.sav")
CSES5 <- read_sav("cses5.sav")



fetch <- function(X, df) {
  df[, grep(X, sapply(df, function(x) attr(x, "label")))]
}

rename_cols <- function(df, v) {
  colnames(df) <- v
  df
}

gamut_col <- function(X) {
  rbind.data.frame(fetch(X, CSES1) %>% rename_cols("X"), 
  fetch(X, CSES2) %>% rename_cols("X"), 
  fetch(X, CSES3) %>% rename_cols("X"), 
  fetch(X, CSES4) %>% rename_cols("X"), 
  fetch(X, CSES5) %>% rename_cols("X"), stringsAsFactors = FALSE) %>% as.vector()
}



rename_ps <- function(df, type) {
  v <- LETTERS[1:9] %>% sapply(function(x) paste0(x, "_", type))
  rename_cols(df, v)
}

vote_clean <- function(df) {
  transmute_all(df, 
    function(x) ifelse(x > 100, NA, x))
}


CSES1_ST_PERC <- 100 * (fetch("NUMBER OF SEATS - LOWER - 2ND SEGMENT", CSES1) / (fetch("NUMBER OF SEATS - LOWER - 1ST SEGMENT", CSES1) + fetch("NUMBER OF SEATS - LOWER - 2ND SEGMENT", CSES1)))
CSES2_ST_PERC <- 100 * (fetch("NUMBER OF SEATS-LOWER-2ND SEGMENT", CSES2) / (fetch("NUMBER OF SEATS-LOWER-1ST SEGMENT", CSES2) + fetch("NUMBER OF SEATS-LOWER-2ND SEGMENT", CSES2)))
MMD_PERC <- c(CSES1_ST_PERC$A5027_2 %>% as.numeric,
              CSES2_ST_PERC$B5033_2 %>% as.numeric,
              CSES3$C5073, CSES4$D5073, CSES5$E5070)

data <- data.frame(
           election = c(gamut_col("ALPHA")),
           country = c(gamut_col("POLITY NAME")),
           year = c(gamut_col("ELECTION YEAR")),
           mod = c(rep(1, times = nrow(CSES1)),
                   rep(2, times = nrow(CSES2)),
                   rep(3, times = nrow(CSES3)),
                   rep(4, times = nrow(CSES4)),
                   rep(5, times = nrow(CSES5))),
            rbind.data.frame(cbind(
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES1), G = NA, H = NA, I = NA) %>% 
                      rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES2) %>% rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES3) %>% rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES4) %>% rename_ps("LH") %>% vote_clean,
                   fetch("PERCENT VOTE - LOWER HOUSE", CSES5) %>% rename_ps("LH") %>% vote_clean),
            rbind.data.frame(cbind(
                   fetch("PERCENT VOTE - PRESIDENT", CSES1), G = NA, H = NA, I = NA) %>% 
                      rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES2) %>% rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES3) %>% rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES4) %>% rename_ps("PRES") %>% vote_clean,
                   fetch("PERCENT VOTE - PRESIDENT", CSES5) %>% rename_ps("PRES") %>% vote_clean),
           mmd = MMD_PERC, stringsAsFactors = FALSE) %>% unique
colnames(data)[1:3] <- c("election", "country", "year")


data <- data[-which(data$country == "Portugal" & data$year == 2002 & data$mod == 2), ]
data %<>% filter(country %in% c("Thailand", "Montenegro") == FALSE) 
# Thailand not a democracy, Montenegro lacks data later on




# need to manually add in presidential vote share data for Romania 2014
data[which(data$election == "ROU_2014"), grep("_PRES", colnames(data))] <-
  c(40.44, 30.37, 5.20, 4.03, 3.68, 3.47, NA, 5.36, 4.44)

# adding in some more data from other sources:

# ideology:
ideologies <- read.csv("cv_data_analysed.csv") %>% dplyr::select(ELECTION, contains("ideo_")) %>% unique
data %<>% filter(election %in% ideologies$ELECTION)
data %<>% cbind.data.frame(ideologies[match(data$election, ideologies$ELECTION),] %>% dplyr::select(-ELECTION))

# vote share type:
vvt <- read.csv("votevariablestable.csv")
data$vote_share_type <- vvt[match(data$election, vvt$ELECTION),] %>% pull(votes)

# for two Presidential elections, the data are in the lower house columns: 
data$vote_share_type[which(data$election %in% c("ARG_2015", "PER_2016"))] <- "lower house"



# now adding in the party polarization index:

data$mean_ideology <- dplyr::select(data, contains("ideo")) %>% rowMeans(na.rm=T) %>% as.numeric

ppi_ind <- function(L, type, elec, module) {
  filt <- data %>% filter(election == elec, mod == module) %>% slice(1)
  
  vs <- as.numeric(dplyr::select(filt, paste0(L, case_when(type == "lower house" ~ "_LH", type == "presidential" ~ "_PRES"))))
  ideo <- as.numeric(dplyr::select(filt, paste0("ideo_", L)))
  mean <- dplyr::select(filt, mean_ideology)
  
  subtract(ideo, mean) %>% divide_by(5) %>% raise_to_power(2) %>% multiply_by(vs)
}

PPI <- function(elec, module) {
LETTERS[1:9] %>% 
    sapply(function(x) 
      ppi_ind(x, as.character(pull(filter(data, election == elec), vote_share_type)[1]), elec, module)) %>% 
    unlist %>% sum(na.rm=T) %>% raise_to_power(1/2)
}

data$PPI <- mapply(PPI, elec = data$election, module = data$mod) %>% as.numeric

# now adding in the enep:

enep <- function(elec, module) {
  type <- data %>% filter(election == elec) %>% slice(1) %>% pull(vote_share_type)
  data %>% filter(election == elec, mod == module) %>% slice(1) %>%
  dplyr::select(contains(case_when(
                  type == "lower house" ~ "_LH", 
                  type == "presidential" ~ "_PRES"))) %>%
    divide_by(100) %>% raise_to_power(2) %>% sum(na.rm=T) %>% 
    divide_by(1, .)
}

data$enep <- mapply(enep, elec = data$election, module = data$mod) %>% as.numeric %>% 
  ifelse(is.infinite(.), NA, .)

# below, R2 elections are assigned eneps of 2, 



# Years of democracy since 1955

Pol4 <- read_excel("POLITYIV.xls")

rename_pol4 <- c("United States", "United Kingdom", "Korea South", "Slovak Republic", "Czechoslovakia")

Pol4$country %<>% (function(x) ifelse(x %in% rename_pol4, 
                                     case_when(x == "United States" ~ "United States of America",
                                               x == "United Kingdom" ~ "Great Britain",
                                               x == "Korea South" ~ "Republic of Korea",
                                               x == "Slovak Republic" ~ "Slovakia",
                                               x == "Czechoslovakia" ~ "Czech Republic"), 
                                     x))

Pol4$sub <- 1955 - Pol4$byear
ref <- filter(Pol4, sub >= 0) %>% group_by(country) %>% dplyr::summarise(year = last(byear))

yrsdemoc <- function(elec) {
  
  C <- filter(data, election == elec) %>% pull(country)
  Y <- filter(data, election == elec) %>% pull(year)
  
  if (C == "Iceland") return(Y - 1955)

  ref_year <- ifelse(C %in% ref$country, 
                 pull(filter(ref, country == C), year), 
                 first(pull(filter(Pol4, country == C), byear)))
 
  filt <- filter(Pol4, country == C, byear >= ref_year, polity >= 6)
  
  filt$eyear[which(filt$present == 1)] <- 2018
  
  total <- mutate(filt, period = eyear - byear) %>% pull(period) %>% sum
  
  startdiff <- ifelse(first(filt$byear) >= 1955, 0, first(filt$byear) - 1955)
  enddiff <- Y - 2018
  
  sum(total, startdiff, enddiff)
}

data$yrsdemoc <- sapply(data$election, yrsdemoc)


# Clear Lines of Responsibility

DPI <- read_excel("DPI2015_basefile.v5.xlsx")

rename_dpi <- c("Czech Rep.", "FRG/Germany", "UK", "ROK", "USA", "S. Africa")

DPI$countryname %<>% (function(x) ifelse(
  x %in% rename_dpi,
  case_when(x == "Czech Rep." ~ "Czech Republic",
            x == "FRG/Germany" ~ "Germany",
            x == "UK" ~ "Great Britain",
            x == "ROK" ~ "Republic of Korea",
            x == "USA" ~ "United States of America",
            x == "S. Africa" ~ "South Africa"),
  x))

dpi <- function(elec) {
  
  if (elec == "SRB_2012") return(NA)
  
  C <- filter(data, election == elec) %>% slice(1) %>% pull(country)
  Y <- filter(DPI, countryname == C, year < pull(slice(filter(data, election == elec), 1), year)) %>% 
    pull(year) %>% last()
  
  filt <- DPI %>% filter(countryname == C, year == Y)
  
  if (filt$system == 2) {
    if (filt$gov1seat/filt$totalseats > 0.5) {
      return(1)
    } else {
      return(0)
    }
  } 
  
  if (filt$system %in% c(0, 1)) {
    return(filt$allhouse)
  }
  
}

data$clearlines <- sapply(data$election, dpi)



# I have checked and the situation was the same for both Greek 2015 elections


# Personal vote incentives

PERS <- read_excel("ESPV.xls")
colnames(PERS) <- PERS[1, ]

PERS <- PERS[3:nrow(PERS) , which(colnames(PERS) %in% c("Country", "Year", "pers_rank"))]

PERS$Country %<>% (function(x) ifelse(
  x %in% c("United Kingdom", "Korea (South, Republic of Korea)", "Taiwan (Republic of China)"),
  case_when(x == "United Kingdom" ~ "Great Britain",
            x == "Korea (South, Republic of Korea)" ~ "Republic of Korea",
            x == "Taiwan (Republic of China)" ~ "Taiwan"),
  x))


pers <- function(elec) {
  
  C <- pull(slice(filter(data, election == elec), 1), country)
  Y <- pull(slice(filter(data, election == elec), 1), year) 
  
  if (Y > 2005) {
    Y <- filter(PERS, Country == C, is.na(as.numeric(pers_rank)) == FALSE) %>% pull(Year) %>% last
  }
  
  PERS %>% filter(Country == C, Year == Y) %>% pull(pers_rank)
}

data$pers <- sapply(data$election, pers) %>% as.numeric

# missing data from Turkey, Spain

# Lau et al. award Spain 2
# I compare Turkey to similar countries (below) and award it 2 also

data$pers[which(data$country %in% c("Spain", "Turkey"))] <- 2

PERS2 <- read_excel("ESPV.xls")
colnames(PERS2) <- PERS2[2, ]

Turk <- filter(PERS2, country == "Turkey", year == 2000) 

Turk_sims <- filter(PERS2, bicameral == Turk$bicameral,
                    oneparty == Turk$oneparty,
                    propn == Turk$propn,
                    Propmmd == Turk$Propmmd,
                    propsmd == Turk$propsmd,
                    propcoded == Turk$propcoded,
                    multitier1 == Turk$multitier1,
                    tiervote1 == Turk$tiervote1,
                    indy == Turk$indy,
                    rank_vote == Turk$rank_vote,
                    multiround1 == Turk$multiround1,
                    year >= 2000)

Check <- Turk_sims$pers_rank %>% as.numeric(.) %>% mean(., na.rm = TRUE)

"As this scale is at best ordinal, we looked at its distribution across our 
sixty-nine elections and dichotomized it at a noticeable break in the distribution, 
which singles out the twenty-nine per cent of the cases with the highest incentives 
for a personal vote"

median_split <- function(x) {
  ifelse(x <= median(x, na.rm=T), 1, 2)
}

data$pers_mediansplit <- median_split(data$pers)



# cut-off should be 10:
data$pers %>% quantile(0.71, na.rm=T) # 10
data %>% filter(mod %in% c(1, 2)) %>% pull(pers) %>% quantile(0.71, na.rm=T) # 10

data$pers_cut <- ifelse(data$pers >= 10, 1, 0)

# media penetration:

internet <- read.csv("internet_users_percentage.csv")
mobile <- read.csv("mobile_subscriptions_percentage.csv")
np <- read.csv("newspapers.csv")

np$Reference.Area %<>% as.character
np$Reference.Area[which(np$Reference.Area == "United Kingdom of Great Britain and Northern Ireland")] <- c("Great Britain")

#need to recode some names to align

internet$Country.Name <- internet[,1] %>% as.character
mobile$Country.Name <- mobile[,1] %>% as.character

recoders <- c("United Kingdom", "Korea, Rep.", "United States", "Slovak Republic")
recodey <- (function(x) ifelse(x %in% recoders, case_when(
  x == "United Kingdom" ~ "Great Britain",
  x == "Korea, Rep." ~ "Republic of Korea",
  x == "United States" ~ "United States of America",
  x == "Slovak Republic" ~ "Slovakia"
), x))

internet$Country.Name %<>% recodey
mobile$Country.Name %<>% recodey

# 2004 internet data for Australia is missing so I replace it with the 2005 reading

internet[
  which(internet$Country.Name == "Australia"), 
  which(colnames(internet) == "X2004")] <- internet[
    which(internet$Country.Name == "Australia"), 
    which(colnames(internet) == "X2005")]


media <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  I <- internet %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
  M <- mobile %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
  
  filt_np <- np %>% filter(Reference.Area == C)
  N <- filt_np$Observation.Value[which.min(Mod(pull(filt_np, Time.Period) - Y))]
  
  data.frame(internet = I, mobile = M, newspapers = N)
}

newspapers <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  filt_np <- np %>% filter(Reference.Area == C)
  filt_np$Observation.Value[which.min(Mod(pull(filt_np, Time.Period) - Y))]
}
mobiles <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  M <- mobile %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
}
internets <- function(x) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  
  if (C %in% c("Hong Kong", "Taiwan", "Montenegro", "Serbia")) return(NA)
  
  I <- internet %>% filter(Country.Name == C) %>% dplyr::select(paste0("X", Y)) %>% as.numeric
}


data$mobiles <- data$election %>% sapply(mobiles) %>% (function(x) (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T)))
data$internet <- data$election %>% sapply(internets) %>% (function(x) (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T)))
data$newspapers <- data$election %>% sapply(newspapers) %>% (function(x) (x - min(x, na.rm=T))/(max(x, na.rm=T) - min(x, na.rm=T)))


data$media <- dplyr::select(data, mobiles, internet, newspapers) %>% 
  apply(1, function(x) mean(x, na.rm=T)) %>% as.numeric



recode_with_labels <- function(x) {
  x %>% factor(levels = attr(x, "labels") %>% as.numeric,
               labels = attr(x, "labels") %>% attr(., "names")) %>% as.character
  
}

coded <- function(df, X) {
  which(sapply(df, function(x) attr(x, "label")) == X) %>% attr(.,"name") %>% pull(df, .)
}

tiers <- rbind.data.frame(
  data.frame(
    first = coded(CSES1, "ELECTORAL FORMULA - LOWER - 1ST SEGMENT") %>% as.numeric,
    second = coded(CSES1, "ELECTORAL FORMULA - LOWER - 2ND SEGMENT") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES2, "ELECTORAL FORMULA-LOWER-1ST SEGMENT") %>% as.numeric,
    second = coded(CSES2, "ELECTORAL FORMULA-LOWER-2ND SEGMENT") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES3, "ELECTORAL FORMULA - LOWEST TIER - LOWER HOUSE") %>% as.numeric,
    second = coded(CSES3, "ELECTORAL FORMULA - SECOND TIER - LOWER HOUSE") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES4, "ELECTORAL FORMULA - LOWEST TIER - LOWER HOUSE") %>% as.numeric,
    second = coded(CSES4, "ELECTORAL FORMULA - SECOND TIER - LOWER HOUSE") %>% as.numeric
  ),
  data.frame(
    first = coded(CSES5, "ELECTORAL FORMULA - LOWEST SEGMENT (TIER) - LOWER HOUSE") %>% as.numeric,
    second = coded(CSES5, "ELECTORAL FORMULA - SECOND SEGMENT (TIER) - LOWER HOUSE") %>% as.numeric
  ))

tiers %<>% data.frame(
  country = c(gamut_col("POLITY NAME")),
  year = c(gamut_col("ELECTION YEAR"))) %>% unique

recode_formulae <- c(
  "11" = "PLURALITY - SINGLE MEMBER DISTRICTS",
  "12" = "PROPORTIONAL - MULTI MEMBER DISTRICTS",
  "20" = "MAJORITY - UNSPECIFIED",
  "21" = "MAJORITY - RUN-OFF",
  "22" = "MAJORITY - ALTERNATIVE",
  "30" = "PROPORTIONAL - UNSPECIFIED",
  "31" = "PROPORTIONAL - D'HONDT",
  "32" = "PROPORTIONAL - LARGEST REMAINDER - DROOP",
  "33" = "PROPORTIONAL - LARGEST REMAINDER - HARE",
  "34" = "PROPORTIONAL - STE-LAGUE"
)

tiers$first %<>% recode_factor(!!!recode_formulae)
tiers$second %<>% recode_factor(!!!recode_formulae)

tierify <- function(x, y) {
  C <- filter(data, election == x) %>% pull(country)
  Y <- filter(data, election == x) %>% pull(year)
  filter(tiers, X == C, X.1 == Y) %>% pull(y) %>% as.character
}

data$first_tier_formula <- sapply(data$election, function(x) tierify(x, "first"))
data$second_tier_formula <- sapply(data$election, function(x) tierify(x, "second"))

data$first_tier_formula_type <- data$first_tier_formula %>% sapply(function(x) strsplit(x, " - ") %>% unlist %>% first()) %>% as.character
data$second_tier_formula_type <- data$second_tier_formula %>% sapply(function(x) strsplit(x, " - ") %>% unlist %>% first()) %>% as.character

classifier <- function(f, s) {
  if (is.na(s) == TRUE | (s == f)) { 
    return(f) } else {
     return("MIXED")
    }
}

data$overall_formula <- mapply(classifier, 
       f = data$first_tier_formula_type, 
       s = data$second_tier_formula_type) %>% as.character

data %>% filter(overall_formula == "MIXED", vote_share_type == "lower house") %>%
  pull(election)

# all mixed-systems votes have been accounted for except those in Slovenia, but 
# voting data is only provided for the first tier
# in all cases, the first tier is SMD and the second is MMD

tabulate <- function(df, n) {
  tab <- dplyr::select(df, contains(n)) 
  colnames(tab) <- c(LETTERS[1:9])
  tab
}

likes <- rbind.data.frame(
  tabulate(CSES1, "A3020"), 
  tabulate(CSES2, "B3037"),
  tabulate(CSES3, "C3009"), 
  tabulate(CSES4, "D3011"), 
  tabulate(CSES5, "E3017")
) %>% transmute_all(function(x) ifelse(x > 10, NA, x)) %>% data.frame

likes$election = c(CSES1$A1004, CSES2$B1004, CSES3$C1004, CSES4$D1004, CSES5$E1004)
likes$module = c(rep(1, nrow(CSES1)), rep(2, nrow(CSES2)), 
                 rep(3, nrow(CSES3)), rep(4, nrow(CSES4)),
                 rep(5, nrow(CSES5)))


proportions <- function(x) {
  length(which(x >= 8))/length(x) * length(which(x <= 2))/length(x)
}

affpol <- function(e, m) {
  filt <- filter(likes, election == e, module == m)
  enep <- filter(data, election == e, mod == m) %>% pull(enep)
  
  LETTERS[1:9] %>% 
    sapply(function(x) pull(filt, x) %>% proportions) %>% 
    sum(na.rm=T) %>% divide_by(., 0.25 * enep)
}

data$affpol <- mapply(affpol, e = data$election, m = data$mod)



gi <- read_excel("gallagherindices.xlsx")

gi$country[which(gi$country == "South Korea")] <- "Republic of Korea"
gi$country[which(gi$country == "United States")] <- "United States of America"

gallagise <- function(elec) {

  yr <- filter(data, election == elec) %>% pull(year)
  ctry <- filter(data, election == elec) %>% pull(country)
  
  if (ctry == "Taiwan") {yr = 2020} else {}
  
  filter(gi, country == ctry, year < yr) %>% pull(lsq) %>% median
}

data$disproportionality <- data$election %>% sapply(gallagise)


write.csv(data, "aggregate_predictors_new.csv")
>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
