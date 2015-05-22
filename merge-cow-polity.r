## ----read-nmc------------------------------------------------------------
raw_NMC <- read.csv(url("http://cow.dss.ucdavis.edu/data-sets/national-material-capabilities/nmc-v4-data/at_download/file"),
                    na.strings = "-9",
                    stringsAsFactors = FALSE)

## ----head-nmc------------------------------------------------------------
dim(raw_NMC)
head(raw_NMC)

## ----dplyr, message=FALSE------------------------------------------------
library("dplyr")
data_NMC <- raw_NMC %>%
  select(-version)

## ----read-polity---------------------------------------------------------
library("foreign")
raw_polity <- read.spss("http://www.systemicpeace.org/inscr/p4v2014.sav",
                         to.data.frame = TRUE)

## ----head-polity---------------------------------------------------------
dim(raw_polity)
head(raw_polity)

## ----clean-polity--------------------------------------------------------
library("stringr")
data_polity <- raw_polity %>%
  mutate_each(funs(ifelse(. %in% c(-66, -77, -88), NA, .)),
              -(ccode:year)) %>%
  mutate(country = str_trim(as.character(country))) %>%
  select(-cyear,
         -scode,
         -flag,
         -fragment,
         -durable,
         -(prior:regtrans))

## ----head-clean-polity---------------------------------------------------
head(data_polity)

## ----identify-discrepancies----------------------------------------------
in_NMC <- sort(unique(data_NMC$ccode))
in_polity <- sort(unique(data_polity$ccode))
polity_minus_NMC <- setdiff(in_polity, in_NMC)

for (x in polity_minus_NMC) {
    years_x <- with(data_polity, range(year[ccode == x])) %>%
      paste(collapse = "-")
    names_x <- with(data_polity, unique(country[ccode == x])) %>%
      paste(collapse = "/")
    cat(x, ": ", names_x, " (", years_x, ")\n", sep = "")
}

## ----overlap-function----------------------------------------------------
find_overlap <- function(c1, c2) {
    overlap_years <- with(data_polity,
                          intersect(year[ccode == c1],
                                    year[ccode == c2]))
    data_polity %>% filter(year %in% overlap_years,
                           ccode %in% c(c1, c2))
}

## ----colombia-entry------------------------------------------------------
with(data_NMC, min(year[ccode == 100]))

## ----colombia-overlap----------------------------------------------------
find_overlap(99, 100)

## ----recode-colombia-----------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 99 | year != 1832) %>%
  mutate(ccode = ifelse(ccode == 99, 100, ccode))

## ----sardinia-overlap----------------------------------------------------
find_overlap(324, 325)

## ----recode-sardinia-----------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 324 | year != 1861) %>%
  mutate(ccode = ifelse(ccode == 324, 325, ccode))

## ----serbia-overlap------------------------------------------------------
find_overlap(342, 345)
find_overlap(342, 347)
find_overlap(345, 347)

## ----recode-serbia-------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 347 | !(year %in% c(1991, 2006))) %>%
  mutate(ccode = ifelse(ccode %in% c(342, 347), 345, ccode))

## ----recode-montenegro---------------------------------------------------
data_polity <- data_polity %>%
  mutate(ccode = ifelse(ccode == 341, 347, ccode),
         ccode = ifelse(ccode == 348, 341, ccode))

## ----ussr-overlap--------------------------------------------------------
find_overlap(364, 365)

## ----recode-ussr---------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 364 | year != 1922) %>%
  mutate(ccode = ifelse(ccode == 364, 365, ccode))

## ----sudan-overlap-------------------------------------------------------
find_overlap(625, 626)

## ----recode-sudan--------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 626 | year != 2011) %>%
  mutate(ccode = ifelse(ccode == 626, 625, ccode),
         ccode = ifelse(ccode == 525, 626, ccode))

## ----pakistan-overlap----------------------------------------------------
find_overlap(769, 770)

## ----recode-pakistan-----------------------------------------------------
data_polity <- data_polity %>%
  mutate(ccode = ifelse(ccode == 769, 770, ccode))

## ----vietnam-overlap-----------------------------------------------------
find_overlap(816, 818)

## ----recode-vietnam------------------------------------------------------
data_polity <- data_polity %>%
  filter(ccode != 818 | year != 1976) %>%
  mutate(ccode = ifelse(ccode == 818, 816, ccode))

## ----recode-austria-hungary----------------------------------------------
years_ah <- with(data_NMC,
                 year[ccode == 300])
data_polity <- rbind(data_polity,
                     data_polity %>%
                       filter(year %in% years_ah,
                              ccode == 305) %>%
                       mutate(ccode = 300))

## ----merge---------------------------------------------------------------
data_state_year <- left_join(data_NMC,
                             data_polity %>% select(-country),
                             by = c("ccode", "year"))

## ----head-state-year-----------------------------------------------------
dim(data_state_year)  # Should be same as `data_NMC`
head(data_state_year)

## ----world-milex---------------------------------------------------------
data_milex <- data_state_year %>%
  group_by(year) %>%
  summarise(world_milex = sum(milex, na.rm = TRUE))

## ----merge-milex---------------------------------------------------------
data_state_year <- left_join(data_state_year,
                             data_milex,
                             by = "year") %>%
  mutate(prop_milex = milex / world_milex)

## ----plot-polity-cinc, message=FALSE-------------------------------------
library("ggplot2")
print(ggplot(data_state_year, aes(x = polity2, y = prop_milex)) +
        geom_smooth())

