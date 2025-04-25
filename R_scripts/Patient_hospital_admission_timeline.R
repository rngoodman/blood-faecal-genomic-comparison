#________________________________________
#
###### Patient admission timelines ######
#_______________________________________

library(readxl)
library(dplyr)
library(ggplot2)

# 1. Read in the data
df = read_excel("data/admission_dates.xlsx")

# 2. Prepare

# All data
df2 = df %>%
  rename(isolateID    = IDnumber,
         study_site    = study_site,    # already named, but for clarity
         admit_date    = Admisiondate,
         discharge_date = Dischargedate) %>%
  mutate(
    admit_date     = as.Date(admit_date),
    discharge_date = as.Date(discharge_date),
    isolateID      = factor(isolateID)
  )

# Amana and Temeke only
df_no_MNH = df %>%
  filter(study_site != "MNH") %>% 
  rename(isolateID    = IDnumber,
         study_site    = study_site,    # already named, but for clarity
         admit_date    = Admisiondate,
         discharge_date = Dischargedate) %>%
  mutate(
    admit_date     = as.Date(admit_date),
    discharge_date = as.Date(discharge_date),
    isolateID      = factor(isolateID)
  )

# MNH only
df_MNH_only = df %>%
  filter(study_site == "MNH") %>% 
  rename(isolateID    = IDnumber,
         study_site    = study_site,    # already named, but for clarity
         admit_date    = Admisiondate,
         discharge_date = Dischargedate) %>%
  mutate(
    admit_date     = as.Date(admit_date),
    discharge_date = as.Date(discharge_date),
    isolateID      = factor(isolateID)
  )

# 3. Plot with facets by study_site
tl = ggplot(df_no_MNH, aes(y = isolateID, color = species)) +
  geom_segment(aes(x = admit_date, xend = discharge_date,
                   yend = isolateID),
               size = 4) +
  facet_wrap(~ study_site, scales = "free_y", ncol = 1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) + 
  scale_color_manual(
    values = c("E. coli" = "#60B332", "K. pneumoniae" = "#73D83C")) +
  labs(
    x     = "Month",
    y     = "Isolate ID",
    title = "Admission to Discharge Timeline"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 12),
    axis.text.y      = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

tl

#ggsave("imgs/patient_hospital_admission_timeline_no_MNH.svg", plot = tl, width = 16, height = 9, dpi = 400)
#ggsave("imgs/patient_hospital_admission_timeline_no_MNH.png", plot = tl, width = 16, height = 9, dpi = 400)
