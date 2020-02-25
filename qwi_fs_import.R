library(data.table)
library(tidyverse)
library(dataCompareR)
library(readr)

qwi_all_ns_fs <- NULL

states <- tolower(state.abb)

all <- append(states, "dc")

qwipath <- function (x) {
  allpath <- c(paste("https://lehd.ces.census.gov/data/qwi/latest_release/", x, "/qwi_", x, "_se_fs_gs_ns_op_u.csv.gz", sep = ""))
}

path <- sapply(all, qwipath)

uspath <- c("https://lehd.ces.census.gov/data/qwi/latest_release/us/qwi_us_se_fs_gn_ns_op_u.csv.gz")

path <- append(path, uspath)


for (i in 1:length(path)) {
  file <- fread(path[i], 
                colClasses = c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
  qwi_all_ns_fs <- rbind(qwi_all_ns_fs, file)
}

qwi_all_ns_fs <- as_tibble(qwi_all_ns_fs)



# LEHD Public Use Data Schema
# https://lehd.ces.census.gov/data/schema/latest/lehd_public_use_schema.html

# Labels

labels <- c("agegrp", "education", "ethnicity", "firmage", "firmsize", "ownercode", 
            "race", "sex", "ind_level", "industry", "geo_level", "fipsnum", "stusps")

lblpath <- function (x) {
  paste("https://lehd.ces.census.gov/data/schema/latest/label_", x, ".csv", sep = "")
}

path2 <- sapply(labels, lblpath)


for (i in 1:length(labels)) {
  file <- read_csv(path2[i], col_types = "cc___")
  # file <- file %>% rename(labels[i] = label)
  # file <- file[, 1:2]
  assign(labels[i], file)
}

# Pluging labels into main QWI dataset

lbls <- labels

for (i in lbls) {
  df <- get(i)
  assign(as.character(i), df, envir = .GlobalEnv)
  newlbl <- paste(as.character(i), "_lbl", sep = "")
  if (newlbl != "stusps_lbl") {
    df <- df %>% rename(!!newlbl := label)
    qwi_all_ns_fs <- left_join(qwi_all_ns_fs, df)
  } else {
    df <- df %>% rename(!!newlbl := stusps)
    qwi_all_ns_fs <- left_join(qwi_all_ns_fs, df)
  }
}


# Save final qwi_all_ns_fs file

save(qwi_all_ns_fs, file = "H:/RESEARCH/RB1 - Employment by firm age and size/qwi_all_ns_fs.Rdata")

