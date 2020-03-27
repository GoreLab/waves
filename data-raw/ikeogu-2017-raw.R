# Jenna Hershberger
# jmh579@cornell.edu
# March 26, 2020

# Raw data from:
# Rapid analyses of dry matter content and carotenoids in fresh cassava roots using a portable
# visible and near infrared spectrometer (Vis/NIRS). PLoS One 12(12): 1–17.
# doi: 10.1371/journal.pone.0188918.

# Raw files downloaded directly from PLoS One website on 03/25/2020
# https://doi.org/10.1371/journal.pone.0188918

library(readxl)
library(tidyverse)

#### Load raw files and format into ikeogu.2017 ####
C16I66 <- read_excel("./data-raw/Ikeogu_et_al_2017/C16I66.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "C16I66") %>%
  mutate(location = "CIAT") %>%
  mutate(year = 2016) %>%
  mutate(prep.method = "intact") %>%
  rename(sample.id = Sampleid) %>%
  rename(DMC.oven = DMLAB) %>%
  rename(TCC = TCCLAB) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)

C16M66 <- read_excel("./data-raw/Ikeogu_et_al_2017/C16M66.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "C16M66") %>%
  mutate(location = "CIAT") %>%
  mutate(year = 2016) %>%
  mutate(prep.method = "mashed") %>%
  rename(sample.id = Sampleid) %>%
  rename(DMC.oven = DMLAB) %>%
  rename(TCC = TCCLAB) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)


C16Mcal <- read_excel("./data-raw/Ikeogu_et_al_2017/C16Mcal.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "C16Mcal") %>%
  mutate(location = "CIAT") %>%
  mutate(year = 2016) %>%
  mutate(prep.method = "mashed") %>%
  rename(sample.id = Sampleid) %>%
  rename(DMC.oven = DMLAB) %>%
  rename(TCC = TCCLAB) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)

C16Mval <- read_excel("./data-raw/Ikeogu_et_al_2017/C16Mval.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "C16Mval") %>%
  mutate(location = "CIAT") %>%
  mutate(year = 2016) %>%
  mutate(prep.method = "mashed") %>%
  rename(sample.id = Sampleid) %>%
  rename(DMC.oven = DMLAB) %>%
  rename(TCC = TCCLAB) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)

U15I <- read_excel("./data-raw/Ikeogu_et_al_2017/U15I.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "U15I") %>%
  mutate(location = "Umudike") %>%
  mutate(year = 2015) %>%
  mutate(prep.method = "intact") %>%
  rename(sample.id = `Sample Number`) %>%
  rename(DMC.oven = DM) %>%
  # The existing TCC column was measured with different methods in this trial as opposed to the
  # CIAT trials, so it has been removed here to reduce confusion.
  mutate(TCC = NA) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)

U16I <- read_excel("./data-raw/Ikeogu_et_al_2017/U16I.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "U16I") %>%
  mutate(location = "Umudike") %>%
  mutate(year = 2016) %>%
  mutate(prep.method = "intact") %>%
  rename(sample.id = Sampleid) %>%
  rename(DMC.oven = DMLAB) %>%
  mutate(TCC = NA) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)

U16M <- read_excel("./data-raw/Ikeogu_et_al_2017/U16M.xlsx") %>%
  rename_at(vars(`350`:`2500`), ~ paste0("X", 350:2500)) %>%
  mutate(study.name = "U16M") %>%
  mutate(location = "Umudike") %>%
  mutate(year = 2016) %>%
  mutate(prep.method = "mashed") %>%
  rename(sample.id = Sampeid) %>%
  rename(DMC.oven = DMLAB) %>%
  mutate(TCC = NA) %>%
  dplyr::select(study.name, location, year, prep.method, sample.id, DMC.oven, TCC, X350:X2500)

#ikeogu.2017 <- rbind(C16I66, C16M66, C16Mcal, C16Mval, U15I, U16I, U16M)
ikeogu.2017 <- rbind(C16I66, C16M66, C16Mcal, C16Mval)
ikeogu.2017$DMC.oven <- as.numeric(ikeogu.2017$DMC.oven)
ikeogu.2017$TCC <- as.numeric(ikeogu.2017$TCC)

# At this point, ikeogu.2017 is ready for export
usethis::use_data(ikeogu.2017, overwrite = TRUE)





