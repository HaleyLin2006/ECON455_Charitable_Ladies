library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(scales)
library(fixest)
library(knitr)
library(kableExtra)

file_name <- "/Users/haleylin/Desktop/Economics/Econ 455/Final Project/Data/Econ455_Final_Clean.xlsx"

# ========== Summary Statistics: Donation to ABCFM ==========
#
# Summary statistics by state, 1844–1848. For each state, the table reports
# the mean and standard deviation of five state-period outcomes: number of donors,
# number of female donors, total donations, female-attributed donations, and the
# female share of donations. The female share is calculated using the restricted
# sheet with more than ten donors per state-period. Miscellaneous and blank rows
# are excluded. The final output is an AER-style LaTeX table with standard
# deviations reported in brackets below the means.
#
# ========================================================

# ---------- Load Data ----------
dfn   <- read_excel(file_name, sheet = "N Donors")
dffn  <- read_excel(file_name, sheet = "N Female Donors")
dfd   <- read_excel(file_name, sheet = "Total Donation ($)")
dffd  <- read_excel(file_name, sheet = "Female Donation ($)")
dffs  <- read_excel(file_name, sheet = "Share of Donation (n>10)")

# ---------- Data Cleaning ----------
clean_sheet <- function(df) {
  names(df) <- as.character(df[1, ])
  df <- df[-1, ]
  names(df) <- trimws(names(df))
  return(df)
}

dfn  <- clean_sheet(dfn)
dffn <- clean_sheet(dffn)
dfd  <- clean_sheet(dfd)
dffd <- clean_sheet(dffd)
dffs <- clean_sheet(dffs)

# ---------- Summary Function ----------
summarize_by_state <- function(df, varname) {
  df %>%
    rename(State = 1) %>%
    mutate(State = trimws(State)) %>%
    filter(!is.na(State), State != "", State != "misc") %>%
    mutate(across(-State, as.numeric)) %>%
    rowwise() %>%
    mutate(
      mean_value = mean(c_across(-State), na.rm = TRUE),
      sd_value   = sd(c_across(-State), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(State, mean_value, sd_value) %>%
    rename(
      "{varname}_mean" := mean_value,
      "{varname}_sd"   := sd_value
    )
}


# ---------- Compute State-Level Summary Statistics ----------
n_stats               <- summarize_by_state(dfn,  "n")
female_n_stats        <- summarize_by_state(dffn, "female_n")
total_donation_stats  <- summarize_by_state(dfd,  "total_donation")
female_donation_stats <- summarize_by_state(dffd, "female_donation")
female_share_stats    <- summarize_by_state(dffs, "female_share")

# ---------- Merge Summary Statistics ----------
summary_stats <- n_stats %>%
  left_join(female_n_stats, by = "State") %>%
  left_join(total_donation_stats, by = "State") %>%
  left_join(female_donation_stats, by = "State") %>%
  left_join(female_share_stats, by = "State")

# ---------- Format LaTeX Cells ----------
cell <- function(mean, sd, digits = 2) {
  sprintf(
    "\\makecell{%.*f\\\\ {[%.2f]}}",
    digits,
    mean,
    sd
  )
}

# ---------- Create AER-Style LaTeX Table ----------
aer_summary_table <- summary_stats %>%
  transmute(
    State,
    `N donors`              = cell(n_mean, n_sd, digits = 2),
    `N female donors`       = cell(female_n_mean, female_n_sd, digits = 2),
    `Total donation`        = cell(total_donation_mean, total_donation_sd, digits = 2),
    `Female donation`       = cell(female_donation_mean, female_donation_sd, digits = 2),
    `Female donation share` = cell(female_share_mean, female_share_sd, digits = 3)
  )


# ---------- Export LaTeX Table ----------
latex_table <- kable(
  aer_summary_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "Summary Statistics of Donation to ABCFM by State, 1844 to 1848",
  label = "tab:summary_by_state",
  align = "lccccc"
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    position = "center"
  )


# ---------- Add Table Notes ----------
latex_table <- as.character(latex_table)

latex_table <- sub(
  "\\\\end\\{table\\}",
  paste0(
    "\\\\vspace{0.5em}\n",
    "\\\\begin{tablenotes}\n",
    "\\\\textit{Notes}: ",
    "Each row reports the mean and standard deviation of state-level monthly ",
    "observations across the sampled donation records from 1844 to 1848. ",
    "Standard deviations are reported in brackets. ",
    "\\\\textit{N donors} is the total number of donors recorded for a state-month. ",
    "\\\\textit{N female donors} is the number of donors identified as female-related. ",
    "\\\\textit{Total donation} is total ABCFM donations in dollars. ",
    "\\\\textit{Female donation} is the total amount donated by female-related donors. ",
    "\\\\textit{Female donation share} is female donation divided by total donation.\n",
    "\\\\vspace{0.5em}\n",
    "\\\\textit{Source}: Authors' calculations using donation records from the ",
    "\\\\textit{Missionary Herald}.\n",
    "\\\\end{tablenotes}\n\n",
    "\\\\end{table}"
  ),
  latex_table
)


# ---------- Save Table as Separate .tex File ----------
writeLines(
  latex_table,
  con = "summary_statistics_by_state.tex"
)

# ========== Compiled Map of Total Donation across States ==========
#
# Donation per capita by state, 1844–1848. Total donations are aggregated across all periods 
# and divided by state population from the 1840 U.S. Census. The figure reports dollars per resident;
# higher values indicate greater donation intensity relative to population.
# 
# ========================================================
# ----- Read total donation sheet -----
dft <- read_excel(file_name, sheet = "Total Donation ($)")
dfp <- read_excel(file_name, sheet = "Population (1840)")

# ----- Clean -----
names(dft) <- as.character(dft[1, ])
dft <- dft[-1, ]
names(dft) <- trimws(names(dft))

names(dfp) <- as.character(dfp[1, ])
dfp <- dfp[-1, ]
names(dfp) <- trimws(names(dfp))

dfp <- dfp %>%
  mutate(
    State = trimws(toupper(State)),
    population = as.numeric(gsub(",", "", Population))
  )

# ----- Reshape -----
dft_long <- dft %>%
  pivot_longer(-State, names_to = "Date", values_to = "total_donation") %>%
  mutate(total_donation = as.numeric(total_donation))

# ----- Compute ALL-TIME total donation -----
state_total_all_time <- dft_long %>%
  group_by(State) %>%
  summarise(
    total = sum(total_donation, na.rm = TRUE),
    .groups = "drop"
  )

df_pc <- state_total_all_time %>%
  mutate(State = trimws(toupper(State))) %>%
  left_join(dfp, by = "State")

df_pc <- df_pc %>%
  mutate(
    donation_per_capita = total / population
  )

# ----- States to include -----
states_use <- c("CT","MA","ME","NH","RI","VT","NY","NJ","PA")

state_map_names <- c(
  CT = "connecticut",
  MA = "massachusetts",
  ME = "maine",
  NH = "new hampshire",
  RI = "rhode island",
  VT = "vermont",
  NY = "new york",
  NJ = "new jersey",
  PA = "pennsylvania"
)

# ----- Prepare map data -----
map_values <- df_pc %>%
  filter(State %in% states_use) %>%
  mutate(region = state_map_names[State])

map_plot_data <- region_map %>%
  left_join(map_values, by = "region")

# ----- Plot donation per capital -----
ggplot(map_plot_data,
       aes(x = long, y = lat, group = group, fill = donation_per_capita)) +
  geom_polygon(color = "black", linewidth = 0.4) +
  coord_fixed(1.3, xlim = c(-81, -66), ylim = c(38, 47.5)) +
  scale_fill_gradient(
    low = "white",
    high = "red",
    labels = scales::dollar_format(accuracy = 0.001),
    name = "Donation per Capita",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Total Donation to ABCFM per Capita by State (1844-1848)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.95, 0.1),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


# ========== Share of Female Donation over Time ==========
#
# Female share of donations by state, 1844–1848. The outcome is defined as the ratio
# of female-attributed donations to total donations in each state–period; missing observations
# are linearly interpolated within state, and NJ, PA, and RI are excluded due to sparse data. 
# Massachusetts (red) is compared to other states (grey), with fitted linear trends overlaid; 
# the dashed line marks June 1846, the passage of the Massachusetts Married Women’s Property Act.
#
# ========================================================
df <- read_excel(file_name, sheet="Share of Donation (n>10)")

states <- c("CT","MA","ME","NH","NJ","NY","PA","RI","VT")
state_labels <- c(
  CT = "Connecticut",
  MA = "Massachusetts",
  ME = "Maine",
  NH = "New Hampshire",
  NJ = "New Jersey",
  NY = "New York",
  PA = "Pennsylvania",
  RI = "Rhode Island",
  VT = "Vermont"
)

i <- 1
highlight_state <- states[i]
highlight_label <- state_labels[highlight_state]

# ---------- Data Cleaning ----------
names(df) <- as.character(df[1, ])          
df <- df[-1, ]      
names(df) <- trimws(names(df))              

# ---------- Reshape ----------
data_long <- df %>%
  pivot_longer(
    cols = -State,
    names_to = "Date",
    values_to = "female_share"
  ) %>%
  mutate(
    female_share = as.numeric(female_share),
    Date = as.Date(paste0("01_", Date), format = "%d_%b_%Y")
  )

# ---------- Interpolate ----------
data_filled <- data_long %>%
  arrange(State, Date) %>%
  group_by(State) %>%
  mutate(
    female_share = approx(
      x = Date,
      y = female_share,
      xout = Date,
      method = "linear",
      rule = 2   # don't extrapolate outside range
    )$y
  ) %>%
  ungroup()

data_filled <- data_filled %>%
  filter(!State %in% c("NJ"))

# ---------- Plot ----------
ggplot(data_filled, aes(x = Date, y = female_share, group = State)) +
  
  # control states
  geom_smooth(
    data = subset(data_filled, State != highlight_state),
    aes(color = "Other States"),
    se = FALSE,
    linewidth = 1
  ) +
  
  # highlighted state
  geom_smooth(
    data = subset(data_filled, State == highlight_state),
    aes(color = highlight_label),
    se = FALSE,
    linewidth = 1.5
  ) +
  
  # points
  geom_point(
    data = subset(data_filled, State != highlight_state),
    aes(color = "Other States"),
    alpha = 0.5,
    size = 1.5
  ) +
  geom_point(
    data = subset(data_filled, State == highlight_state),
    aes(color = highlight_label),
    size = 2.5
  ) +
  
  # reform line
  geom_vline(
    xintercept = as.Date("1846-06-01"),
    linetype = "dashed",
    color = "black"
  ) +
  
  labs(
    title = "Share of Female Donation Over Time",
    x = "Date",
    y = "% Female Donation ($ Female / $ All)",
    color = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  
  scale_color_manual(
    values = setNames(
      c("red", "grey70"),
      c(highlight_label, "Other States")
    )
  )

# ------ Side-by-Side Plot ------
plot_data <- lapply(states, function(s) {
  data_filled %>%
    mutate(
      highlight_label = state_labels[s],
      group_label = if_else(State == s, "Highlighted State", "Other States")
    )
}) %>%
  bind_rows()

ggplot(plot_data, aes(x = Date, y = female_share, group = State)) +
  geom_smooth(
    aes(color = group_label),
    se = FALSE,
    linewidth = 0.8
  ) +
  geom_point(
    aes(color = group_label),
    alpha = 0.55,
    size = 1
  ) +
  geom_vline(
    xintercept = as.Date("1846-06-01"),
    linetype = "dashed",
    color = "black",
    linewidth = 0.4
  ) +
  facet_wrap(~ highlight_label, ncol = 3) +
  scale_color_manual(
    values = c(
      "Highlighted State" = "red",
      "Other States" = "grey70"
    )
  ) +
  labs(
    title = "Share of Female Donation Over Time by Highlighted State",
    x = "Date",
    y = "% Female Donation ($ Female / $ All)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.justification = "right",
    legend.box.just = "right"
  ) +
  guides(color = guide_legend(ncol = 1))

# ========== Share of Female Donors over Time ==========
#
# Female share of donors by state, 1844–1848.
# Outcome: female-attributed donors / total donors in each state-period.
# Missing observations are linearly interpolated within state.
# 
# ---------- Data Cleaning ----------
names(df) <- as.character(df[1, ])          
df <- df[-1, ]      
names(df) <- trimws(names(df)) 

# ---------- Reshape ----------
data_long <- df %>%
  filter(State %in% states) %>%        # drops misc row
  pivot_longer(
    cols = -State,
    names_to = "Date",
    values_to = "female_share"
  ) %>%
  mutate(
    female_share = as.numeric(female_share),
    Date = as.Date(paste0("01_", Date), format = "%d_%b_%Y")
  )

# ---------- Interpolate ----------
data_filled <- data_long %>%
  arrange(State, Date) %>%
  group_by(State) %>%
  mutate(
    female_share = approx(
      x = Date,
      y = female_share,
      xout = Date,
      method = "linear",
      rule = 2
    )$y
  ) %>%
  ungroup()

# ---------- Plot ----------
ggplot(data_long, aes(x = Date, y = female_share, group = State)) +
  geom_smooth(
    data = data_long %>% filter(State != highlight_state),
    aes(color = "Other States"),
    se = FALSE,
    linewidth = 1
  ) +
  geom_smooth(
    data = data_long %>% filter(State == highlight_state),
    aes(color = highlight_label),
    se = FALSE,
    linewidth = 1.5
  ) +
  geom_point(
    data = data_long %>% filter(State != highlight_state),
    aes(color = "Other States"),
    alpha = 0.5,
    size = 1.5
  ) +
  geom_point(
    data = data_long %>% filter(State == highlight_state),
    aes(color = highlight_label),
    size = 2.5
  ) +
  geom_vline(
    xintercept = as.Date("1846-06-01"),
    linetype = "dashed",
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      setNames("red", highlight_label),
      "Other States" = "grey70"
    )
  ) +
  labs(
    title = "Share of Female Donors Over Time",
    x = "Date",
    y = "% Female Donors",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ---------- Side-by-Side Highlight Plot ----------

plot_data <- data_long %>%
  filter(State %in% states) %>%
  group_by(State) %>%
  group_split() %>%
  lapply(function(df_state) {
    
    s <- unique(df_state$State)
    
    data_long %>%
      mutate(
        highlight_label = state_labels[s],
        group_label = if_else(State == s, "Highlighted State", "Other States")
      )
    
  }) %>%
  bind_rows()

ggplot(plot_data, aes(x = Date, y = female_share, group = State)) +
  
  geom_smooth(
    aes(color = group_label),
    se = FALSE,
    linewidth = 0.9
  ) +
  
  geom_point(
    aes(color = group_label),
    alpha = 0.55,
    size = 1.2
  ) +
  
  geom_vline(
    xintercept = as.Date("1846-06-01"),
    linetype = "dashed",
    color = "black",
    linewidth = 0.4
  ) +
  
  facet_wrap(~ highlight_label, ncol = 3) +
  
  scale_color_manual(
    values = c(
      "Highlighted State" = "red",
      "Other States" = "grey70"
    )
  ) +
  
  labs(
    title = "Share of Female Donors Over Time",
    x = "Date",
    y = "% Female Donors",
    color = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


# ========== Regression: Share of Female Donation and State Characteristics ==========
#
# This section constructs a panel dataset at the state–year level (1844–1848) and
# estimates the relationship between the female share of donations and state-level
# characteristics. Female donation share is merged with (i) interpolated income per
# capita (from 1840 and 1880 endpoints, following Easterlin (1960)) and (ii) a proxy
# for female educational attainment from the 1850 Census, defined as the ratio of
# females attending school to the female population of school age.
#
# The regression includes year fixed effects and clusters standard errors at the
# state level. A key constraint is that educational attainment is time-invariant 
# (because there's no data before 1850 census), which creates tension with fixed 
# effects: including state fixed effects would fully absorb this variable. As a 
# result, the specification relies on cross-sectional variation to estimate the 
# association between education and female donation share, and should be 
# interpreted with caution.
#
# =============================================================================

df_pc <- read_excel(file_name, sheet = "Income pc")
df_edu <- read_excel(file_name, sheet = "Edu Attainment")
df <- read_excel(file_name, sheet="Share of Donation (n>10)")

# ----- Clean (row 1 as header) -----
names(df_pc) <- as.character(df_pc[1, ])
df_pc <- df_pc[-1, ]
names(df_pc) <- trimws(names(df_pc))

names(df_edu) <- as.character(df_edu[1, ])
df_edu <- df_edu[-1, ]
names(df_edu) <- trimws(names(df_edu))

names(df) <- as.character(df[1, ])
df <- df[-1, ]
names(df) <- trimws(names(df))

data_long <- df %>%
  pivot_longer(
    cols = -State,
    names_to = "Date",
    values_to = "female_share"
  ) %>%
  mutate(
    female_share = as.numeric(female_share),
    Date = as.Date(paste0("01_", Date), format = "%d_%b_%Y"),
    Year = as.numeric(format(Date, "%Y"))
  )

# ----- Rename for convenience (adjust if needed) -----
df_pc <- df_pc %>%
  rename(
    State = State,
    income_1840 = `1840 income pc`,
    income_1880 = `1880 income pc`
  ) %>%
  mutate(
    income_1840 = as.numeric(income_1840),
    income_1880 = as.numeric(income_1880)
  )

df_edu <- df_edu %>%
  mutate(
    State = trimws(toupper(State)),
    F_in_school = as.numeric(`F in School`),
    F_in_school_age = as.numeric(`F in School Age`),
    edu_attainment = F_in_school / F_in_school_age
  ) %>%
  dplyr::select(State, edu_attainment)

# ----- Interpolate annual data (1840–1850) -----
income_annual <- df_pc %>%
  rowwise() %>%
  mutate(
    data = list(tibble(
      Year = 1840:1850,
      income_pc = approx(
        x = c(1840, 1880),
        y = c(income_1840, income_1880),
        xout = 1840:1850
      )$y
    ))
  ) %>%
  dplyr::select(State, data) %>%
  unnest(data) %>%
  ungroup()

# ------ Merge Data -----
data_reg <- data_long %>%
  left_join(income_annual, by = c("State", "Year"))

data_reg <- data_reg %>%
  left_join(df_edu, by = "State")

# ----- Regression 1: Income only -----
reg_income <- feols(
  female_share ~ income_pc | State + Year,
  data = data_reg,
  cluster = ~State
)

summary(reg_income)

# ----- Regression 2: Education only -----
reg_edu <- feols(
  female_share ~ edu_attainment | Year,
  data = data_reg,
  cluster = ~State
)

summary(reg_edu)

# ----- Regression 3: Full specification -----
reg_full <- feols(
  female_share ~ income_pc + edu_attainment | Year,
  data = data_reg,
  cluster = ~State
)

summary(reg_full)

# ----- Export table -----
etable(
  reg_income,
  reg_edu,
  reg_full,
  tex = TRUE,
  file = "regression.tex"
)

# ========== Difference-in-Differences: Massachusetts Married Women's Property Act ==========
#
# This section estimates whether the female share of donations changed
# differentially in Massachusetts after the 1846 Married Women's Property Act.
# The treatment variable is MA x Post, where MA equals one for Massachusetts and
# Post equals one for periods after June 1846. The regression includes state and
# time fixed effects, with standard errors clustered at the state level.
#
# =============================================================================

df_pc <- read_excel(file_name, sheet = "Income pc")
df_edu <- read_excel(file_name, sheet = "Edu Attainment")
df <- read_excel(file_name, sheet="Share of Donation (n>10)")

# ----- Clean (row 1 as header) -----
names(df_pc) <- as.character(df_pc[1, ])
df_pc <- df_pc[-1, ]
names(df_pc) <- trimws(names(df_pc))

names(df_edu) <- as.character(df_edu[1, ])
df_edu <- df_edu[-1, ]
names(df_edu) <- trimws(names(df_edu))

names(df) <- as.character(df[1, ])
df <- df[-1, ]
names(df) <- trimws(names(df))

# ----- Reshape female donation share data -----
data_long <- df %>%
  mutate(State = trimws(toupper(State))) %>%
  pivot_longer(
    cols = -State,
    names_to = "Date",
    values_to = "female_share"
  ) %>%
  mutate(
    female_share = as.numeric(female_share),
    Date = as.Date(paste0("01_", Date), format = "%d_%b_%Y"),
    Year = as.numeric(format(Date, "%Y"))
  ) %>%
  filter(!is.na(female_share))

# ----- Clean income data -----
df_pc <- df_pc %>%
  rename(
    State = State,
    income_1840 = `1840 income pc`,
    income_1880 = `1880 income pc`
  ) %>%
  mutate(
    State = trimws(toupper(State)),
    income_1840 = as.numeric(income_1840),
    income_1880 = as.numeric(income_1880)
  )

# ----- Clean education data -----
df_edu <- df_edu %>%
  mutate(
    State = trimws(toupper(State)),
    F_in_school = as.numeric(`F in School`),
    F_in_school_age = as.numeric(`F in School Age`),
    edu_attainment = F_in_school / F_in_school_age
  ) %>%
  select(State, edu_attainment)

# ----- Interpolate annual income -----
income_annual <- df_pc %>%
  rowwise() %>%
  mutate(
    data = list(tibble(
      Year = 1840:1850,
      income_pc = approx(
        x = c(1840, 1880),
        y = c(income_1840, income_1880),
        xout = 1840:1850
      )$y
    ))
  ) %>%
  select(State, data) %>%
  unnest(data) %>%
  ungroup()

# ----- Construct DiD dataset -----
data_did <- data_long %>%
  mutate(
    MA = if_else(State == "MA", 1, 0),
    Post = if_else(Date >= as.Date("1846-06-01"), 1, 0),
    MA_Post = MA * Post
  ) %>%
  left_join(income_annual, by = c("State", "Year")) %>%
  left_join(df_edu, by = "State")

# ----- Baseline DiD -----
did_reg <- feols(
  female_share ~ MA_Post | State + Year,
  data = data_did,
  cluster = ~State
)

summary(did_reg)

# ----- DiD with income control -----
did_reg_income <- feols(
  female_share ~ MA_Post + income_pc | State + Year,
  data = data_did,
  cluster = ~State
)

summary(did_reg_income)

# ----- Optional: include education -----
# This will likely drop edu_attainment because it is absorbed by state FE.
did_reg_full <- feols(
  female_share ~ MA_Post + income_pc + edu_attainment |  Year,
  data = data_did,
  cluster = ~State
)

summary(did_reg_full)

# ----- Export regression table -----
etable(
  did_reg,
  did_reg_income,
  did_reg_full,
  tex = TRUE,
  file = "did_regression.tex"
)
