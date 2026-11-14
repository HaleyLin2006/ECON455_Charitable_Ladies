library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# ========== Sampling Months ==========
set.seed(455)

randomMonth_1 <- sample(seq.Date(as.Date("1844-01-01"), by = "month", length.out = 30), 6)
randomMonth_2 <- sample(seq.Date(as.Date("1846-06-01"), by = "month", length.out = 31), 6)
randomMonth <- c(randomMonth_1 , randomMonth_2)
sort(randomMonth)

plot(randomMonth, rep(1, length(randomMonth)), pch=19, yaxt="n",
     ylab="", xlab="Date", main="Sampled Months")

# ====== Months ======
#   "1844-01-01" "1844-07-01" "1845-01-01" "1845-03-01" "1845-08-01" "1845-12-01" "1846-09-01" "1846-12-01" "1847-04-01" "1847-10-01"
#   "1848-02-01" "1848-07-01"
# ========================

# ========== Inspecting Data ==========
file_name <- "/Users/haleylin/Desktop/Economics/Econ 455/Final Project/Data/Econ455_RawData.xlsx"
month_date <- "Aug_1845"
raw_data <- read_excel(file_name, sheet = month_date)


# !!!!!!!!!! Compile All Sheets =======
# ----- Compile All Sheets -----
all_data <- excel_sheets(file_name) %>%
  lapply(function(s) {
    df <- read_excel(file_name, sheet = s)
    
    # set first row as column names
    names(df) <- as.character(df[1, ])
    df <- df[-1, ]
    names(df) <- trimws(names(df))
    
    df$Sheet <- s   # keep track of source sheet
    df
  }) %>%
  bind_rows()

raw_data <- all_data

# ========== Data Cleaning ===========
# ----- Drop Rows and Adjust Column Name -----
# set row 1 as column names
names(raw_data) <- as.character(raw_data[1, ])          
# remove row name (row 1) from data
raw_data <- raw_data[-1, ]      

# ----- Drop Unnecessary Columns -----
raw_data$Flag <- NULL
raw_data$Notes <- NULL
raw_data$Level <- NULL

# ----- Drop Empty & Disc Rows -----
raw_data <- raw_data %>%
  filter(Amount >= 0)

raw_data

# ----- Merge Raw Labels -----
raw_data <- raw_data %>%
  unite("raw_label", Raw_Label_1, Raw_Label_2, sep = " ", na.rm = TRUE)

# ----- Turn Amount in to Number -----
raw_data <- raw_data %>%
  mutate(Amount = as.numeric(Amount))

# ----- Fill County & Collector -----
raw_data <- raw_data %>%
  fill(County, .direction = "down")

raw_data <- raw_data %>%
  fill(Collector, .direction = "down")

# !!!!!!!!!! Fill State (before 1848, no need for 1848) -----
raw_data <- raw_data %>%
  mutate(State = case_when(
    str_detect(Collector, "New\\s+York\\s+City") ~ "NY",
    str_detect(Collector, "Board of Foreign Missions in Ref\\. Dutch Ch\\.") ~ "NY",
    str_detect(Collector, "Board of Foreign Missions in German Ref\\. Ch\\.") ~ "NY",
    
    str_detect(Collector, "\\bM\\.?\\s*s\\.?\\b") ~ "MA",
    str_detect(Collector, "\\bN\\.?\\s*Y\\.?\\s*(C\\.)?\\b") ~ "NY",
    str_detect(Collector, "\\bV\\.?\\s*t\\.?\\b") ~ "VT",
    str_detect(Collector, "\\bN\\.?\\s*H\\.?\\b") ~ "NH",
    str_detect(Collector, "\\bM\\.?\\s*e\\.?\\b") ~ "ME",
    str_detect(Collector, "\\bC\\.?\\s*t\\.?\\b") ~ "CT",
    str_detect(Collector, "\\b(R\\.?\\s*I\\.?|Rhode\\s+Island)\\b") ~ "RI",   
    str_detect(Collector, "\\bN\\.?\\s*J\\.?\\b") ~ "NJ",
    str_detect(Collector, "\\bP\\.?\\s*a\\.?\\b") ~ "PA",

    !is.na(Collector) ~ "misc",   # only non-NA Collector gets misc
    TRUE ~ State                  # keep existing / NA otherwise
  ))

raw_data <- raw_data %>%
  fill(State, .direction = "down")

raw_data %>%
  filter(State == "RI")

# ---- Create Female Label -----
raw_data <- raw_data %>%
  mutate(Female = if_else(
    str_detect(str_to_lower(raw_label),
               "fem\\.?|ms\\.?|miss\\.?|mrs\\.?|lady|ladies|female|girls|la\\.?") &
      !str_detect(str_to_lower(raw_label), "\\bgent\\.?\\b"),
    TRUE,
    FALSE
  ))

# ========== Calculate Amount Female ($) ===========
sum(raw_data$Amount, na.rm = TRUE)

result <- raw_data %>%
  group_by(State) %>%
  summarise(
    n = n(),
    n_female = sum(Female, na.rm = TRUE),
    total_donation = sum(Amount, na.rm = TRUE),
    female_donation = sum(Amount[Female == TRUE], na.rm = TRUE),
    female_share = female_donation / total_donation
  )

result

print(raw_data[raw_data$Female == TRUE, "raw_label"], n = 100)
