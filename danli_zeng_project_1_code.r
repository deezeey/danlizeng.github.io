library(dplyr)
library(ggplot2)
library(plyr)

# Load data atus_act 2011-2015
readLines("./atus_0315/atusact_0315.dat", n=5)
atus_act <- read.table("./atus_0315/atusact_0315.dat", sep = ',', colClasses = 'character', header = TRUE)
atus_act$TUCASEID <- as.numeric(atus_act$TUCASEID)
atus_act <- filter(atus_act, TUCASEID > 20110000000000)
atus_act <- select(atus_act, TUCASEID, TUACTIVITY_N, TUACTDUR24, TRCODEP, TRTIER1P, TRTIER2P, TUCUMDUR, TUCUMDUR24, TUACTDUR, TEWHERE)
atus_act$TUCASEID <- as.character(atus_act$TUCASEID)

# Load data atus_resp 2011-2015
readLines("./atus_0315/atusresp_0315.dat", n=5)
atus_resp <- read.table("./atus_0315/atusresp_0315.dat", sep = ",", colClasses = 'character', header = TRUE)
atus_resp$TUCASEID <- as.numeric(atus_resp$TUCASEID)
atus_resp <- filter(atus_resp, TUCASEID > 20110000000000)
atus_resp <- select(atus_resp, TUCASEID, TULINENO, TRERNWA, TRDPFTPT, TEHRUSLT, TEIO1OCD, TEIO1COW, TEIO1ICD, TESCHENR, TESCHLVL, TESCHFT)
atus_resp$TUCASEID <- as.character(atus_resp$TUCASEID)

# Load data atus_sum 2011-2015
readLines("./atus_0315/atussum_0315.dat", n=5)
atus_sum <- read.table("./atus_0315/atussum_0315.dat", sep = ",", colClasses = 'character', header =TRUE)
atus_sum$TUCASEID <- as.numeric(atus_sum$TUCASEID)
atus_sum <- filter(atus_sum, TUCASEID > 20110000000000)
atus_sum <- select(atus_sum, TUCASEID, TEAGE, TESEX, PEEDUCA)
atus_sum$TUCASEID <- as.character(atus_sum$TUCASEID)
which(duplicated(atus_sum$TUCASEID) == TRUE)

# Load data atus_cps 2011-2015
readLines("./atus_0315/atuscps_0315.dat", n=5)
atus_cps <- read.table("./atus_0315/atuscps_0315.dat", sep = ",", colClasses = 'character', header =TRUE)
atus_cps$TUCASEID <- as.numeric(atus_cps$TUCASEID)
atus_cps <- filter(atus_cps, TUCASEID > 20110000000000 & TULINENO == 1)
atus_cps <- select(atus_cps, TUCASEID, TULINENO, GEREG, GESTFIPS, GEMETSTA, HEFAMINC, HUFAMINC, HRHTYPE, HRNUMHOU, PRDTIND1, PRDTOCC1, PEMARITL)
atus_cps$TUCASEID <- as.character(atus_cps$TUCASEID)

# Joining tables by CASEID
Temp <- list(atus_cps, atus_sum, atus_resp, atus_act)
atus_1115 <- join_all(Temp, by = "TUCASEID", type = "inner", match = "all")

rm(atus_act, atus_cps, atus_resp, atus_sum)
rm(Temp)
atus_1115$TULINENO <-  NULL
which(is.na(atus_1115) == TRUE)
# The dataset we gonna work with has 1118532 obs. of 33 variables
n_distinct(atus_1115$TUCASEID)
# We have a sample size of 58804 respondents from 2011-2015


# Q1 - How's the trend of time using by year from 2011-2015 ?
# Creating data subset of top10 activities by year
atus_1115$TUACTDUR24 <- as.numeric(atus_1115$TUACTDUR24)
atus_1115$year <- substr(atus_1115$TUCASEID, 1, 4)
by_year <- atus_1115 %>%
  group_by(year, TRCODEP) %>%
  summarise(sum_dura = sum(TUACTDUR24)/60, freq = n(), sample = n_distinct(TUCASEID))

names(by_year)[2] <- "Activity"

atus_1115 %>%
  group_by(year) %>%
  summarise(n_distinct(TUCASEID))

getUniverse <- function(year){
  return(switch(year, '2011' = 12479, '2012' = 12443, '2013' = 11385, '2014' = 11592, '2015' = 10905))
}
by_year$universe <- sapply(by_year$year, getUniverse)

by_year <- by_year[order(by_year$year,desc(by_year$sum_dura)),]

by_year <- by_year %>%
  group_by(year) %>%
  top_n(10, sum_dura)

unique(by_year$Activity)

by_year$Activity <- factor(by_year$Activity,
                           levels = c("010101","120303","050101","110101","010201","120101","020201","020101","120312","120301"),
                           labels = c("Sleep", "TV & Movies", "Work", "Eat & Drink", "Grooming", "Social", "Cook", "Housework", "Reading", "Relaxing & Thinking"))

# Plot 1
ggplot(by_year,
       aes(x = reorder(Activity, desc(sum_dura)),
           y = sum_dura/universe)) +
  geom_bar(stat = 'identity', aes(fill = Activity)) + 
  xlab('Activity') + ylab('Time spent (hours/day/individual)') + 
  facet_wrap(~year) + 
  ggtitle("Top10 Activities American spent time on") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
rm(by_year)

# Q2 - What's the top 10 recreational activities by year from 2011-2015 ?
# Creating data subset of top10 recreational activities by year
Temp <-  c("0701", "0702", "1201", "1202", "1203", "1204", "1299", "1301", "1302", "1399")
leisure <- filter(atus_1115, TRTIER2P %in% Temp)
leisure$leisurecode <- ifelse(leisure$TRTIER2P == '1203' | leisure$TRTIER2P == '1204', leisure$TRCODEP, leisure$TRTIER2P)

getUniverse <- function(year){
  return(switch(year, '2011' = 12479, '2012' = 12443, '2013' = 11385, '2014' = 11592, '2015' = 10905))
}
leisure$universe <- sapply(leisure$year, getUniverse)

leisure_sum <- leisure %>%
  group_by(year, leisurecode) %>%
  summarise(sum_dura = sum(TUACTDUR24/60))

leisure_sum <- leisure_sum[order(leisure_sum$year,desc(leisure_sum$sum_dura)),]

leisure_sum<- leisure_sum %>%
  group_by(year) %>%
  mutate(all_sum_dura = sum(sum_dura))

leisure_top10 <- leisure_sum %>%
  group_by(year) %>%
  top_n(10, sum_dura)

leisure_top10 <- leisure_sum %>%
  group_by(year) %>%
  mutate(dura_ratio = sum_dura / all_sum_dura)

leisure_top10$universe <- sapply(leisure_top10$year, getUniverse)

unique(leisure_top10$leisurecode)

leisure_top10$leisurecode <- factor(leisure_top10$leisurecode,
                           levels = c("120303",      "1201",   "0701",     "120312",   "120301",             "1301",        "120308",                   "120307",  "1202",                            "1302",             "120309",                  "120499"),
                           labels = c("TV & Movies", "Social", "Shopping", "Reading", "Relaxing & Thinking", "Doing Sport", "Computer Use for Leisure", "Games", "Attending or Hosting Social Event", "Attending Sport", "Arts & Crafts as a hobby", "Other Arts & Entertainment"))

# Plot 2
ggplot(leisure_top10,
       aes(x = reorder(leisurecode, desc(sum_dura)),
           y = sum_dura/universe)) +
  geom_bar(stat = 'identity', aes(fill = leisurecode)) + 
  xlab('Activity') + ylab('Time spent (hours/day/individual)') + 
  facet_wrap(~year) + 
  ggtitle("Top10 Recreational Activities Americans spent time on") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
# Missing bars indicate that category was in one of the year's top 10s, however is replaced by another other activity in that year.

# Q3 - If we divide all the recreational activities into beneficial & non-beneficial, will there be difference across region of dif. people with dif. Family Income?
# Creating benef var
Total <- c("1201", "120312", "120301", "1301", "1202", "1302", "120309", "120499", "120303", "0701", "120308", "120307")
Bene <- c("1201", "120312", "120301", "1301", "1202", "1302", "120309", "120499")
Non_Bene <- c("120303", "0701", "120308", "120307")
leisure <- filter(leisure, leisurecode %in% Total) 
leisure$benef <- ifelse(leisure$leisurecode %in% Bene, "Beneficial", "Non_Beneficial")

time_bene <- leisure %>%
  group_by(GEREG, HEFAMINC, benef) %>%
  summarise(sum_benef_dura = sum(TUACTDUR24))
leisure <- inner_join(leisure, time_bene)

time_sum <- leisure %>%
  group_by(GEREG, HEFAMINC) %>%
  summarise(sum_dura = sum(TUACTDUR24))
leisure <- inner_join(leisure, time_sum)

rm(time_sum, time_bene)

leisure$benef_ratio = leisure$sum_benef_dura / leisure$sum_dura

leisure$HEFAMINC <- as.numeric(leisure$HEFAMINC)
HEFAMINC.code <- c('Less than $5,000' = 1,
                  '$5,000 - $7,499' = 2,
                  '$7,500 - $9,999' = 3,
                  '$10,000 - $12,499' = 4,
                  '$12,500 - $14,999' = 5,
                  '$15,000 - $19,999' = 6,
                  '$20,000 - $24,999' = 7,
                  '$25,000 - $29,999' = 8,
                  '$30,000 - $34,999' = 9,
                  '$35,000 - $39,999' = 10,
                  '$40,000 - $49,999' = 11,
                  '$50,000 - $59,999' = 12,
                  '$60,000 - $74,999' = 13,
                  '$75,000 - $99,999' = 14,
                  '$100,000 - $149,000' = 15,
                  '$150,000 and over' = 16) 
leisure$FFIncome <- names(HEFAMINC.code)[match(leisure$HEFAMINC, HEFAMINC.code)]

leisure_benef <- filter(leisure, benef == 'Beneficial')
leisure_benef$GEREG <- factor(leisure_benef$GEREG,
                                    levels = c("1",      "2",   "3",     "4"),
                                    labels = c("Northeast", "Midwest", "South", "West"))

colnames(leisure_benef)[which(names(leisure_benef) == 'GEREG')] <- 'Region'
# Plot3
ggplot(leisure_benef, aes(x = reorder(FFIncome, HEFAMINC), y = benef_ratio)) +
  geom_point(aes(color = Region, shape = Region), size = 3) +
  xlab('Family Income') + ylab('Ratio of Time spent on Beneficial Recreational Act.') + 
  ggtitle("Beneficial/All Recreational Act. Ratio by HH Income & Region") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
# Higher family income people enjoy more beneficial recreational activities.
# It seems people living in the west enjoy more beneficial recreational activities.

# rm(Bene, Non_Bene, getSampleSize, Temp, Total, leisure_top10, leisure_sum, leisure_benef)

# Q4 - Map out the states with their beneficial activity ratio
By_state <- select(leisure, TUCASEID, GEREG, GESTFIPS, TUACTDUR24, year, leisurecode, universe, benef)
time_bene <- By_state %>%
  group_by(GESTFIPS, benef) %>%
  summarise(sum_benef_dura = sum(TUACTDUR24))
By_state <- inner_join(By_state, time_bene)

time_sum <- By_state %>%
  group_by(GESTFIPS) %>%
  summarise(sum_dura = sum(TUACTDUR24))
By_state <- inner_join(By_state, time_sum)

rm(time_sum, time_bene)

By_state$benef_ratio = By_state$sum_benef_dura / By_state$sum_dura

By_state_benef <- filter(By_state, benef == 'Beneficial')

# Interpret State Code

Temp = c(1:56)
Temp = Temp[-c(3, 7, 14, 43, 52)]
By_state_benef$GESTFIPS <- factor(By_state_benef$GESTFIPS,
                                  levels = Temp,
                                  labels = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"))
# And then I found there's something called state.fips :(

# Plot4 - Choropleth Map
library(maps)
library(mapproj)

var <- as.integer(By_state_benef$benef_ratio*100)
percents <- cut(var, 52, include.lowest = TRUE, include.highest = TRUE, ordered = TRUE)

shades <- colorRampPalette(c("white", "darkcyan"))(52)

fills <- shades[percents]

map("state", fill = TRUE, col=fills,  resolution = 0, lty = 0,
    projection = "polyconic", myborder = 0, mar = c(0,0,0,0))

map.text(database="state", add=TRUE)

text(x=state.center$x, y=state.center$y, state.abb, cex=0.5, col = "black")

title(main = "Beneficial Recreational Activity Ratio by State", col = "darkcyan")

inc <- (max(var) - min(var)) / 4
legend.text <- c(paste0(min(var), " %"),
                 paste0(min(var) + inc, " %"),
                 paste0(min(var) + 2 * inc, " %"),
                 paste0(min(var) + 3 * inc, " %"),
                 paste0(max(var), " %"))
legend("bottomleft",
       legend = legend.text,
       fill = shades[c(1, 13, 26, 39, 52)],
       col = "darkcyan",
       title = "Beneficial Act. Ratio",
       cex = 0.65)

# rm(By_state, By_state_benef, fills, inc, legend.text, percents, shades, Temp, var)

# Q5 - Is there relationships between edu. level, personal income and recreational activities preference?

leisure$TRERNWA <- as.numeric(leisure$TRERNWA)
leisure1 <- filter(leisure, TRERNWA != -1)
leisure1$sum_benef_dura <- NULL
leisure1$sum_dura <- NULL
leisure1$benef_ratio <- NULL

time_bene <- leisure1 %>%
  group_by(TUCASEID, benef) %>%
  summarise(sum_benef_dura = sum(TUACTDUR24))
leisure1 <- inner_join(leisure1, time_bene)

time_sum <- leisure1 %>%
  group_by(TUCASEID) %>%
  summarise(sum_dura = sum(TUACTDUR24))
leisure1 <- inner_join(leisure1, time_sum)

leisure1$benef_ratio <- leisure1$sum_benef_dura / leisure1$sum_dura

leisure1_benef <- filter(leisure1, benef == 'Beneficial')

getEduLvl <- function(EDU){
  return(switch(EDU, "31" = "Highschool or Below", "32" = "Highschool or Below", "33" = "Highschool or Below", "34" = "Highschool or Below", "35" = "Highschool or Below", "36" = "Highschool or Below", "37" = "Highschool or Below", "38" = "Highschool or Below", "39" = "Highschool or Below", "40" = "College without Degree", "41" = "Associate Degree", "42" = "Associate Degree", "43" = "Bachelor's", "44" = "Master's", "45" = "Professional Degree", "46" = "Doctoral"))
}

leisure1_benef$EduLvl<- sapply(leisure1_benef$PEEDUCA, getEduLvl)

colnames(leisure1_benef)[which(names(leisure1_benef) == 'TRERNWA')] <- 'Personal_Weekly_Income'

# Plot 5

ggplot(leisure1_benef, aes(x = Personal_Weekly_Income, y = benef_ratio)) +
  geom_point(size = 0.25, aes(color = EduLvl)) +
  facet_wrap(~EduLvl) +
  xlab('Personal Weekly Income') + ylab('Ratio of Time spent on Beneficial Recreational Act.') + 
  ggtitle("Beneficial Recreational Activity Ratio by Personal Income & Edu. Lvl.") 

# ggplot(leisure_benef, aes(x = reorder(FFIncome, HEFAMINC), y = benef_ratio)) +
#   geom_point(aes(color = Region, shape = Region), size = 3) +
#   xlab('Family Income') + ylab('Ratio of Time spent on Beneficial Recreational Act.') + 
#   ggtitle("Beneficial/All Recreational Act. Ratio by HH Income & Region") +
#   theme(axis.text.x = element_text(angle = 70, hjust = 1))