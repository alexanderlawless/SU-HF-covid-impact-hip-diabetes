### Health Foundation: COVID and hip replacement and diabetes activity 

library(tidyverse)
library(janitor)
library(odbc)
library(DBI)
library(plotly)
#devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
library(lubridate)
library(ggrepel)
library(patchwork)
library(dygraphs)
library(xts) 
library(PropCIs)
library(scales)

# https://plotly.com/r/text-and-annotations/#multiple-annotations 
# https://datascott.com/blog/subtitles-with-ggplotly/  

## Set SU theme ####
My_rgb2hex <- function(r,g,b) sprintf('#%s',paste(as.hexmode(c(r,g,b)),collapse = ''))

# scale_colour_manual(values = c("#f9bf07", "#686f73", "#5881c1", "#ec6555"))
# scale_colour_manual(values = c("#686f73", "#5881c1", "#ec6555"))

SU_colours <- c (
  `orange` = My_rgb2hex(248,191,7),# "#f9bf07",
  `charcoal` = My_rgb2hex(44,40,37),# "#2c2825",
  `slate` = My_rgb2hex(104,111,115), # "#686f73",
  `blue` = My_rgb2hex(88,29,193), # "#5881c1",
  `red` = My_rgb2hex(236,101,85), # "#ec6555",
  #additional accent colours from word doc template
  `yellow` = My_rgb2hex(252,229,155),
  `grey` = My_rgb2hex(163,168,172),
  `white` = My_rgb2hex(255,255,255),
  #light and dark ends from colour theme in word doc
  `light orange` = My_rgb2hex(253,242,205),
  `dark orange` = My_rgb2hex(124,95,3),
  `light charcoal` = My_rgb2hex(235,233,231),
  `dark charcoal` = 	"#000000",#black
  `light slate` = My_rgb2hex(224,226,227),
  `dark slate` = My_rgb2hex(51,55,57),
  `light blue` = My_rgb2hex(221,229,242),
  `dark blue` = My_rgb2hex(38,61,102),
  `light red` = My_rgb2hex(251,224,220),
  `dark red` = My_rgb2hex(144,29,16),
  `light yellow` = My_rgb2hex(254,249,235),
  `dark yellow` = My_rgb2hex(197,152,5),
  `light grey`=My_rgb2hex(236,237,238),
  `dark grey` = My_rgb2hex(79,84,88),
  `light white`=My_rgb2hex(242,242,242),
  `dark white` =My_rgb2hex(127,127,127),
  `red2` = My_rgb2hex(215,25,28),
  `orange2` = My_rgb2hex(253,174,97),
  `yellow2` = My_rgb2hex(255,255,191),
  `green2` = My_rgb2hex(171,221,164),
  `blue2` = My_rgb2hex(43,131,186)
)



SU_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (SU_colours)
  
  SU_colours[cols]
}

SU_palettes <- list(
  `main` = SU_cols("orange","charcoal","slate","blue","red"),
  `oranges` = SU_cols("light orange","orange","dark orange"),
  `slates` = SU_cols("light slate","slate","dark slate"),
  `mixed` = SU_cols("dark red","orange","yellow","light blue","slate"),
  `oj_coal` = SU_cols("yellow","orange","red","dark red","dark charcoal"),
  `oj_red` = SU_cols("yellow","orange","red","dark red"),
  `white_oj_coal` = SU_cols("white","yellow","orange","red","dark red","dark charcoal"),#added since shared
  `lyellow_oj_coal` = SU_cols("light yellow","orange","red","dark red","dark charcoal"),#added since shared
  `wy_oj_coal` = SU_cols("white","light yellow","yellow","orange","red","dark red","charcoal","dark charcoal"),
  `red_coal` = SU_cols("red","dark red","charcoal","dark charcoal"),
  `blue_yellow_red` = SU_cols("red2","orange2","yellow2","green2","blue2"),
  `red_yellow_blue` = SU_cols("blue2","green2","yellow2","orange2","red2")
)

SU_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- SU_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}   

scale_color_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


scale_fill_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}  

##Theme


##set theme settings
theme_SU<-
  function (base_size){
    theme_minimal(
      #base_family = "Segoe UI", 
      base_size=12
      ) %+replace% 
      theme(axis.title = element_text(size=11, face="bold",colour=SU_cols("charcoal")),
            plot.title = element_text(hjust=0,face="bold",size=12,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
            plot.subtitle = element_text(hjust=0,face="italic",size=10,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
            plot.caption = element_text(hjust = 0,face="italic",size=10,colour=SU_cols("slate"),margin=margin(b=4,unit="pt")),
            legend.text = element_text(size=10,colour=SU_cols("charcoal")),
            legend.title = element_text(face="bold",size=11,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")))
  }



##set theme by 
theme_set(theme_SU())

## A. Import SUS data ####

# Connection
a.con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 server = "PRODNHSESQL101", 
                 Database = "NHSE_Sandbox_Spec_Neurology")

a_lsoa_to_stp <- "https://opendata.arcgis.com/datasets/d30531b5888a4e34be4746399d696409_0.csv" %>%
  read_csv() %>% 
  clean_names()

a_stp_to_nhsr <- "https://opendata.arcgis.com/datasets/888dc5cc66ba4ad9b4d935871dcce251_0.csv" %>%
  read_csv() %>% 
  clean_names()


# Import SUS inpatient hip activity (2018 onwards)
a.ip_hip <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[dbo].[HF_covid_hip_activity_2]')) %>% 
  mutate(procedure_type = case_when(
    (totalHipReplacementPrimary == 1 |
      totalProstheticReplacementHeadFemurPrimary == 1 |
      hybridProstheticHipReplacementPrimary == 1 |
      otherHipReplacementPrimary == 1 |
      resurfacingArthroplastyJointPrimary == 1 ) ~ "Primary",
    (totalHipReplacementRevision == 1 |
       totalProstheticReplacementHeadFemurRevision == 1 |
       hybridProstheticHipReplacementRevision == 1 |
       otherHipReplacementRevision == 1|
       resurfacingArthroplastyJointRevision == 1) ~ "Revision",
    TRUE ~ "NA" )) %>% 
  mutate(Age_range = case_when(Age_at_Start_of_Episode_SUS >= 0 & Age_at_Start_of_Episode_SUS < 20 ~ "0-19",
                               (Age_at_Start_of_Episode_SUS >= 20 & Age_at_Start_of_Episode_SUS <40) ~  "20-39",
                               (Age_at_Start_of_Episode_SUS >= 40 & Age_at_Start_of_Episode_SUS <60) ~  "40-59",
                               (Age_at_Start_of_Episode_SUS >= 60 & Age_at_Start_of_Episode_SUS <80) ~  "60-79",
                               (Age_at_Start_of_Episode_SUS >= 80 & Age_at_Start_of_Episode_SUS <100) ~ "80-99",
                               Age_at_Start_of_Episode_SUS >= 100 ~ "100+")) %>% 
  mutate(Age_range = factor(Age_range, levels = c("0-19", 
                                                  "20-39",
                                                  "40-59",
                                                  "60-79",
                                                  "80-99", 
                                                  "100+"))) %>% 
  left_join(a_lsoa_to_stp, by = c("Der_Postcode_LSOA_2011_Code" = "lsoa11cd")) %>% 
  left_join(a_stp_to_nhsr %>% 
              select(stp20cd, nhser20cd, nhser20nm) %>% 
              distinct(), 
            by = c("stp20cd"))

a.ip_hip_unplanned <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[dbo].[HF_covid_hip_activity_ip_unplanned]'))

a.opa_hip <-
as_tibble(
  dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[DBO].[HF_covid_hip_activity_OPA]'))

a.ed_hip <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[DBO].[HF_covid_hip_activity_ED]'))


# Disconnect! # 
dbDisconnect(a.con) 

# Lockdown dates
a_reference_dates <-
  tribble(
    ~Reference_date, ~Desc, ~Start_flag,
    
    "2020-03-23",	"National lockdown", "Y",  	
    "2020-07-04",	"National lockdown", "N",
    "2020-10-14", "3 Tier lockdown", "Y",
    "2021-01-04", "3 Tier lockdown", "N",
    "2020-05-13", "National lockdown label", "N",
    "2020-11-24", "3 Tier lockdown label", "N",
    "2020-08-24", "Recovery periond label", "N"
  ) %>% 
  mutate(Reference_date = as.Date(Reference_date)) %>% 
  mutate(date_numeric = as.numeric(Reference_date)) %>% 
  mutate(week_number = isoweek(Reference_date))


## B. High level summary - Hip ####
## B.1. Time series summaries ####
# Monthly 
# Inpatients
b_hip_ip_month <-
a.ip_hip %>% 
  group_by(substr(Admission_Date, 1, 7)) %>% #reduce discharge date to discharge month
  summarise(n_admissions = n_distinct(APCS_Ident)) %>%  #sum monthly discharges
  rename(Month = 1) %>% 
  mutate(Month = paste0(Month,"-01")) %>% #Add day value for as.Date function
  mutate(Month = as.Date(Month)) %>% #mutate from chr to date
  filter(Month > 2018-01) 
# Outpatients
b_hip_opa_month <-
a.opa_hip %>% 
  group_by(substr(Appointment_Date, 1, 7)) %>% #reduce discharge date to discharge month
  summarise(n_appointments = n_distinct(OPA_Ident)) %>%  #sum monthly appointments
  rename(Month = 1) %>% 
  mutate(Month = paste0(Month,"-01")) %>% #Add day value for as.Date function
  mutate(Month = as.Date(Month)) %>% #mutate from chr to date
  filter(Month > 2018-01) 
# ED
b_hip_ed_month <-
a.ed_hip %>% 
  group_by(substr(Arrival_Date, 1, 7)) %>% #reduce discharge date to discharge month
  summarise(n_attendances = n_distinct(EC_Ident)) %>%  #sum monthly attendances
  rename(Month = 1) %>% 
  mutate(Month = paste0(Month,"-01")) %>% #Add day value for as.Date function
  mutate(Month = as.Date(Month)) %>% #mutate from chr to date
  filter(Month > 2018-01)
# Merge
b_hip_all_month <-
  b_hip_ip_month %>% 
  left_join(b_hip_opa_month, by = ("Month")) %>% 
  left_join(b_hip_ed_month, by = ("Month")) %>% 
  pivot_longer(cols = -Month, names_to = "Activity", values_to = "Count") %>% 
  mutate(Activity = case_when(Activity == "n_admissions" ~ "Inpatient",
                              Activity == "n_appointments" ~ "Outpatient",
                              Activity == "n_attendances" ~ "Emergency Department",
                              TRUE ~ "NA")) %>% 
  mutate(Activity = factor(Activity, levels = c("Inpatient", "Outpatient", "Emergency Care")))

# Daily
# Inpatient
b_hip_ip_daily <-
  a.ip_hip %>% 
  group_by(Admission_Date) %>% 
  summarise(n_admissions = n_distinct(APCS_Ident)) %>% 
  mutate(Admission_Date = as.Date(Admission_Date)) %>% 
  filter(Admission_Date > 2018-01-01) 
# Outpatient
b_hip_opa_daily <-
  a.opa_hip %>% 
  group_by(Appointment_Date) %>% 
  summarise(n_appointments = n_distinct(OPA_Ident)) %>% 
  mutate(Appointment_Date = as.Date(Appointment_Date)) %>% 
  filter(Appointment_Date > 2018-01-01) 
#ED
b_hip_ed_daily <-
  a.ed_hip %>% 
  group_by(Arrival_Date) %>% 
  summarise(n_attendances = n_distinct(EC_Ident)) %>% 
  mutate(Arrival_Date = as.Date(Arrival_Date)) %>% 
  filter(Arrival_Date > 2018-01-01)
#Merge
b_hip_all_daily <-
  b_hip_ip_daily %>%
  rename(Date = 1) %>% 
  left_join(b_hip_opa_daily %>% rename(Date = 1), by = ("Date")) %>% 
  left_join(b_hip_ed_daily %>% rename(Date = 1), by = ("Date")) %>% 
  pivot_longer(cols = -Date, names_to = "Activity", values_to = "Count") %>% 
  mutate(Activity = case_when(Activity == "n_admissions" ~ "Inpatient",
                              Activity == "n_appointments" ~ "Outpatient",
                              Activity == "n_attendances" ~ "Emergency Care",
                              TRUE ~ "NA")) %>% 
  mutate(Activity = factor(Activity, levels = c("Inpatient", "Outpatient", "Emergency Care")))



## B.1.1. Visualise Monthly time series ####
# Monthly
# Facet wrap - inpatient, outpatient and ED
b_hip_all_month_vis <-
b_hip_all_month %>% 
  filter(Month > "2018-01-01" & 
           Month < "2020-12-31") %>% 
  
  ggplot(aes(x = Month, y = Count)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 15000, 
           fill= "#686f73", alpha = 0.6) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 15000, 
           fill= "#686f73", alpha = 0.6) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 12500, label = "National lockdown",size = 5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 12500, label = "3 Tier lockdown",size = 5, angle = 90) +
  geom_area(fill="#f9bf07", alpha=0.5) +
  geom_line(color="#f9bf07", size = 1.2) +
  facet_wrap(~Activity) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73")) +
  labs(title = "Hip replacement activity",
       subtitle = "England, 2018-2020") 

## B.1.2. Visualise Daily time series ####
# All hip activity  
b_hip_all_daily_vis <-
b_hip_all_daily %>% 
  filter(Date > "2018-01-01" & 
           Date < "2020-12-31") %>% 
  mutate(week_day = weekdays(Date)) %>% 
  mutate(weekend_flag = case_when(substr(week_day,1,1) == "S" ~ "Weekend",
                                  TRUE ~ "Week day")) %>% 
  ggplot(aes(x = Date, y = Count)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 600, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 600, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  #geom_point(aes(colour = weekend_flag), size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~Activity) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73")) +
  scale_color_SU(palette = "main") +
  labs(title = "Planned hip replacement activity was halted by the pandemic",
       subtitle = "Hip replacement procedures and related activity: National, planned and unplanned care",
       x = "Date",
       y = "Activity",
       colour = "")

# Markdown chunk (non-interactive version)
b_hip_all_daily_vis +
  annotate("text", x = as.Date("2020-05-13"), y = 500, label = "National lockdown",size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 500, label = "3 Tier lockdown",size = 3, angle = 90) +
  labs(colour = "Daily count")


# Markdown chunk
b_hip_all_daily_vis %>% 
  ggplotly(tooltip = c("Date","Count")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0(
           '<b>Hip replacement procedures and related activity<b>',
           '<br>',
           '<sup>',
           #'<i>National, planned and unplanned care<i>',
           '</sup>'))) 

# Inpatient
b_hip_ip_daily_vis <-
b_hip_ip_daily %>% 
  filter(Admission_Date > "2018-01-01" & 
           Admission_Date < "2020-12-31") %>%
  mutate(week_day = weekdays(Admission_Date)) %>% 
  mutate(weekend_flag = case_when(substr(week_day,1,1) == "S" ~ "Weekend",
                                  TRUE ~ "Week day")) %>% 
  
  ggplot(aes(x = Admission_Date, y = n_admissions)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 450, label = "National lockdown",size = 5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 450, label = "3 Tier lockdown",size = 5, angle = 90) +
  geom_point(aes(colour = weekend_flag), size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  labs(title = "Inpatient activity: Hip replacement procedures",
       subtitle = "National, planned care",
       colour = "",
       x = "Date",
       y = "Admissions")

# Outpatient
b_hip_opa_daily_vis <-
b_hip_opa_daily %>% 
  filter(Appointment_Date > "2018-01-01" & 
           Appointment_Date < "2020-12-31") %>% 
  mutate(week_day = weekdays(Appointment_Date)) %>%
  mutate(weekend_flag = case_when(substr(week_day,1,1) == "S" ~ "Weekend",
                                  TRUE ~ "Week day")) %>% 
  
  ggplot(aes(x = Appointment_Date, y = n_appointments)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 420, label = "National lockdown",size = 5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 420, label = "3 Tier lockdown",size = 5, angle = 90) +
  geom_point(aes(colour = weekend_flag), size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  labs(title = "Outpatient activity: Hip replacement procedures and diagnoses",
       subtitle = "National, pre- and post- operative care",
       colour = "",
       x = "Date",
       y = "Appointments")

# ED 
b_hip_ed_daily_vis <-
b_hip_ed_daily %>% 
  filter(Arrival_Date > "2018-01-01" & 
           Arrival_Date < "2020-12-31") %>%
  mutate(week_day = weekdays(Arrival_Date)) %>%
  mutate(weekend_flag = case_when(substr(week_day,1,1) == "S" ~ "Weekend",
                                  TRUE ~ "Week day")) %>% 
  
  ggplot(aes(x = Arrival_Date, y = n_attendances)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = Inf, 
           fill= "#fd8d3c", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 100, label = "National lockdown",size = 5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 100, label = "3 Tier lockdown",size = 5, angle = 90) +
  geom_point(aes(colour = weekend_flag), size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  labs(title = "Emergency activity: Hip replacement attendances",
       subtitle = "National, hip-related injuries and emergencies",
       colour = "",
       x = "Date",
       y = "Attendances")


## B.2. Interactive graphs #### 
# Time series
# https://rstudio.github.io/dygraphs/gallery-series-options.html

b_hip_all_daily_grouped <-
b_hip_all_daily %>% 
  ungroup() %>% 
  filter(Date > "2018-01-01" & 
           Date < "2020-12-31") %>%
  drop_na(Count) %>% 
  group_by(Date) %>% 
  summarise(sum = sum(Count)) %>% 
  mutate(Admission_week = floor_date(as.Date(Date), unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week) %>% 
  summarise(Activity = sum(sum)) 
  
# Dygraph 
dygraph(xts(x = b_hip_ip_daily$n_admissions, 
            order.by = b_hip_ip_daily$Admission_Date),
        main = "<b>Inpatient hip replacement admissions<b> <br> 
        <small> <i>Daily count, England 2018-20<i></small>",
        xlab = "Date", ylab = "Admissions"
) %>%
  dyOptions(labelsUTC = TRUE, 
            fillGraph=TRUE,  
            drawGrid = FALSE, 
            colors="#f9bf07") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "both") %>%
  dyHighlight(highlightCircleSize = 10, 
              highlightSeriesBackgroundAlpha = 1, 
              hideOnMouseOut = FALSE)


# Try and plot multiple trends alongside each other
b_hip_all_month_wide <-
b_hip_all_month %>% 
  pivot_wider(id_cols = Month, names_from = Activity, values_from = Count) %>% clean_names()

ip <- xts(b_hip_all_month_wide$inpatient, order.by = b_hip_all_month_wide$month)
opa <- xts(b_hip_all_month_wide$outpatient, order.by = b_hip_all_month_wide$month)
ed <- xts(b_hip_all_month_wide$emergency_care, order.by = b_hip_all_month_wide$month)

hip_all <-
  cbind(ip, opa, ed)

interactive_dygraph_all_hip_activity <-
dygraph(hip_all, 
        main = "Hip replacement activity by care setting, England 2018-20", 
        xlab = "Date",
        ylab = "Monthly activity count") %>%
  dyOptions(drawGrid = FALSE, 
            colors = c("#f9bf07", "#5881c1", "#ec6555"),
            strokeWidth = 5) %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 1, 
              hideOnMouseOut = FALSE) %>% 
  dyShading(from = "2020-03-23", to = "2020-07-04", color = "#d9d9d9") %>% 
  dyShading(from = "2020-10-1", to = "2021-01-04", color = "#d9d9d9") %>% 
  dyAnnotation("2020-05-13", text = "National Lockdown") %>% 
  dyAnnotation("2020-11-24", text = "3 Tier lockdown") 
  

## C. Cohort characteristics ####

## C.1. Age ####
c_ip_hip_age <-
a.ip_hip %>% 
  group_by(Admission_Date, Age_range) %>% 
  summarise(Admissions = n_distinct(APCS_Ident)) %>% 
  mutate(daily_sum = sum(Admissions)) %>% 
  mutate(prop = Admissions/daily_sum*100) %>% 
  mutate(prop = round(prop, 2)) %>% 
  ungroup()

c_ip_hip_age_vis <-
  c_ip_hip_age %>% 
  filter(Age_range %in% c("60-79", "80-99")) %>% 
  mutate(Admission_Date = as.Date(Admission_Date)) %>% 

  ggplot(aes(x = Admission_Date, y = prop, group = Age_range)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 100, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 100, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  #annotate("text", x = as.Date("2020-05-13"), y = 95, label = "National lockdown",size = 3, angle = 90) +
  #annotate("text", x = as.Date("2020-11-24"), y = 85, label = "3 Tier lockdown",size = 3, angle = 90) +
  geom_point(aes(colour = Age_range), alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  labs(title = "Daily proportion of hip procedures performed by age range",
       subtitle = "National, elective inpatient episodes",
       x = "Date",
       y = "Proportion admissions",
       colour = "")

# markdown chunk
ggplotly(
  c_ip_hip_age_vis +
    annotate("text", x = as.Date("2020-05-13"), y = 95, label = "National lockdown",size = 3, angle = 90) +
    annotate("text", x = as.Date("2020-11-24"), y = 85, label = "3 Tier lockdown",size = 3, angle = 90),
  tooltip = c("Admission_Date","prop")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0('<b>Daily proportion of hip procedures performed by age range<b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>National, elective inpatient episodes<i>',
                                    '</sup>'))) 
# Stacked bar chart 
c_ip_hip_age_stacked_data <-
  c_ip_hip_age %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, Age_range) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Proportion = round(prop, digits = 2)) %>% 
  drop_na(Proportion) 

c_ip_hip_age_stacked_vis <-
  c_ip_hip_age_stacked_data %>% 
  ggplot(aes(x = Admission_week, y = Proportion, fill = Age_range)) +
  geom_bar(position = "stack", stat = "identity", width = 7) +
  scale_fill_brewer(palette = "Spectral", limits=c("0-19", "20-39", "40-59", "60-79", "80-99", "100+")) +
  labs(title = "Proportion of elective hip procedures by age range",
       x = "Admission week",
       y = "Proportion",
       fill = "")

# Admissions in all ages as reference
c_ip_hip_admission_volume <-
a.ip_hip %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week) %>% 
  summarise(Admission_count = n()) %>% 
  mutate(ID = "Inpatient") 

c_ip_hip_admission_volume_vis <- 
  c_ip_hip_admission_volume %>%     
  ggplot(aes(x = Admission_week, y = Admission_count, colour = ID)) +
  geom_area(fill="#f9bf07", alpha=0.5) +
  geom_line(color="#f9bf07", size = 0.91) +
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-07-04"), linetype = "dashed") +
  annotate("text", x = as.Date("2020-05-13"), y = 2500, label = str_wrap("National lockdown", 10) ,size = 3) +
  geom_vline(xintercept = as.Date("2020-10-14"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-01-04"), linetype = "dashed") +
  annotate("text", x = as.Date("2020-11-24"), y = 2500, label = str_wrap("3 Tier lockdown", 10) ,size = 3) +
  annotate("text", x = as.Date("2020-08-24"), y = 2500, label = str_wrap("Recovery period", 10) ,size = 3) +
  labs(x = "Admission week", y = "Count", colour = "",
       title = "Count of weekly admissions for elective hip procedures")

# Bar and line chart - flat
(c_ip_hip_age_stacked_vis +
    geom_vline(xintercept = 18344, linetype = "dashed") +
    geom_vline(xintercept = 18447, linetype = "dashed") +
    geom_vline(xintercept = 18549, linetype = "dashed") +
    geom_vline(xintercept = 18631, linetype = "dashed")) /
  (c_ip_hip_admission_volume_vis 
   ) +
  plot_layout(heights = c(5,1)) 
  
# Combine interactive stacked bar and line graph 
c_ip_hip_stack_and_line <-
subplot(
  # Stacked bar 
ggplotly(c_ip_hip_age_stacked_vis +
           annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = 0, yend = 100, lty = 2) +
           annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = 0, yend = 100, lty = 2) +
           annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = 0, yend = 100, lty = 2) +
           annotate("segment", x = as.Date("2021-01-04"), xend = as.Date("2021-01-04"), y = 0, yend = 100, lty = 2),
         tooltip = c("Age_range", "Proportion")
         ),

  # Line chart
style(
  ggplotly(c_ip_hip_admission_volume_vis +
             annotate("segment", x = as.Date("2021-01-04"), xend = as.Date("2021-01-04"), y = 0, yend = 2800, lty = 2) +
             annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = 0, yend = 2800, lty = 2) +
             annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = 0, yend = 2800, lty = 2) +
             annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = 0, yend = 2800, lty = 2),
           tooltip = c("Admission_week", "Admission_count")),
  showlegend = FALSE
  ),
  # Organise plots
nrows = 2, heights = c(0.8,0.2), shareY = TRUE, shareX = TRUE 
      ) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0('<b> At the height of disruption, care was limited to the older patients despited increased covid risk <b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>Proportion and count of elective hip procedures by age range: National, weekly average<i>',
                                    '</sup>')))  
# 2018 average
c_ip_hip_age_2018_avg <-
  c_ip_hip_age %>% 
  filter(Admission_Date > "2018-01-01" & 
           Admission_Date < "2018-12-31") %>% 
  drop_na(Age_range) %>% 
  group_by(Age_range) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Proportion = round(prop, digits = 3)) %>% 
  mutate(Date = "2018-01-01") %>%
  mutate(Date = as.Date(Date)) %>% 
  
  ggplot(aes(x = Date, y = Proportion, fill = Age_range)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral", limits=c("0-19", "20-39", "40-59", "60-79", "80-99", "100+")) +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  labs(x = "2018",
       y = "Proportion")

# 2019 average
c_ip_hip_age_2019_avg <-
  c_ip_hip_age %>% 
  filter(Admission_Date > "2019-01-01" & 
           Admission_Date < "2019-12-31") %>% 
  drop_na(Age_range) %>% 
  group_by(Age_range) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Proportion = round(prop, digits = 3)) %>% 
  mutate(Date = "2019-01-01") %>%
  mutate(Date = as.Date(Date)) %>% 
  
  ggplot(aes(x = Date, y = Proportion, fill = Age_range)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral", limits=c("0-19", "20-39", "40-59", "60-79", "80-99", "100+")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  labs(x = "2019",
       y = "")

# Weekly average
c_ip_hip_age_2020_wk <-
  c_ip_hip_age %>% 
  filter(Admission_Date > "2020-01-01" & 
           Admission_Date < "2020-12-31") %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, Age_range) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Proportion = round(prop, digits = 3)) %>% 
  
  ggplot(aes(x = Admission_week, y = Proportion, fill = Age_range)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral", limits=c("0-19", "20-39", "40-59", "60-79", "80-99", "100+")) +
  theme(axis.text.y = element_blank()) +
  labs(x = "Admission week",
       y = "",
       fill = "Age range")

#Patch together 
c_ip_hip_age_2018_avg + c_ip_hip_age_2019_avg + c_ip_hip_age_2020_wk + 
  plot_layout(ncol = 3, widths = c(1,1,4)) +
  plot_annotation(title = "Proportion of elective hip procedures by age range",
                  subtitle = "National, 2018 and 2019 annual averages compared to 2020 weekly averages")

# Stream graph  
c_ip_hip_age_stacked_vis %>% 
  ggplotly(tooltip = c("Age_range", "Proportion")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0('<b>Proportion of elective hip procedures by age range<b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>National, weekly average<i>',
                                    '</sup>'))) 




## C.2. Deprivation #### 
con_ref <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 server = "PRODNHSESQL101", 
                 Database = "NHSE_Reference")

c_lsoa_imd <- tbl(con_ref, "tbl_Ref_Other_Deprivation_By_LSOA") %>% 
  collect()

# Disconnect! # 
dbDisconnect(con_ref) 

c_ip_hip_imd <-
  a.ip_hip  %>% 
  select(1:9, procedure_type, Age_range) %>% 
  left_join(select(c_lsoa_imd, LSOA_Code, IMD_Score, IMD_Rank, IMD_Decile), by = c("Der_Postcode_LSOA_2011_Code" = "LSOA_Code")) %>% 
  mutate(IMD_Quintile = case_when(IMD_Decile %in% c(1,2) ~ 1,
                                  IMD_Decile %in% c(3,4) ~ 2,
                                  IMD_Decile %in% c(5,6) ~ 3,
                                  IMD_Decile %in% c(7,8) ~ 4,
                                  IMD_Decile %in% c(9,10) ~ 5,
                                  TRUE ~ 99)) %>% 
  group_by(Admission_Date, IMD_Quintile) %>% 
  summarise(Admissions = n_distinct(APCS_Ident)) %>% 
  mutate(daily_sum = sum(Admissions)) %>% 
  mutate(prop = Admissions/daily_sum*100) %>% 
  mutate(prop = round(prop, 2)) 

c_ip_hip_imd_wk <-
  c_ip_hip_imd %>% 
  ungroup() %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, IMD_Quintile) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(IMD_Quintile = as.character(IMD_Quintile))

c.ip_hip_imd_vis <-
  c_ip_hip_imd_wk %>% 
  ggplot(aes(x = Admission_week, y = prop, group = IMD_Quintile, colour = IMD_Quintile  )) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 40, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 40, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 37, label = "National lockdown",size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 34, label = "3 Tier lockdown",size = 3, angle = 90) +
  geom_point( alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.2) +
  scale_colour_brewer(palette = "Spectral") +
  ylim(0,40) +
  labs(title = "During the initial lockown, hip care was focused on the very rich and very poor",
       subtitle = "Weekly proportion of hip procedures performed by IMD deprivation quintile: National, elective inpatient episodes",
       x = "Admission week",
       y = "Proportion admissions",
       caption = "Note: IMD quintile 1 represents the poorest 20% of the population",
       colour = "")

# Markdown chunk
ggplotly(c.ip_hip_imd_vis,
         tooltip = c("Admission_week","prop")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0(
           '<b>During the initial lockown, hip care was focused on the very rich and very poor<b>',
           '<br>',
           '<sup>',
           '<i>Weekly proportion of hip procedures performed by IMD deprivation quintile: National, elective inpatient episodes<i>',
           '</sup>'))) 

# Total stacked bar 
c_ip_hip_imd_stack_vis <-
c_ip_hip_imd %>%  
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, IMD_Quintile) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Proportion = round(prop, digits = 2)) %>% 
  drop_na(Proportion) %>% 
  
  ggplot(aes(x = Admission_week, y = Proportion, fill = as.character(IMD_Quintile))) +
  geom_bar(position = "stack", stat = "identity", width = 7) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Proportion of elective hip procedures by IMD Quintile",
       subtitle = "National, weekly average", 
       x = "Admission week",
       y = "Proportion",
       fill = "")

# Stacked bar and line graph
c_hip_ip_stacked_bar_and_line <-
subplot(
  # Stacked bar 
  ggplotly(c_ip_hip_imd_stack_vis +
             annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = 0, yend = 100, lty = 2) +
             annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = 0, yend = 100, lty = 2) +
             annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = 0, yend = 100, lty = 2) +
             annotate("segment", x = as.Date("2021-01-04"), xend = as.Date("2021-01-04"), y = 0, yend = 100, lty = 2),
           tooltip = c("Admission_week","Proportion")
  ),
  
  # Line chart
  style(
    ggplotly(c_ip_hip_admission_volume +
               annotate("segment", x = as.Date("2021-01-04"), xend = as.Date("2021-01-04"), y = 0, yend = 2800, lty = 2) +
               annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = 0, yend = 2800, lty = 2) +
               annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = 0, yend = 2800, lty = 2) +
               annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = 0, yend = 2800, lty = 2),
             tooltip = c("Admission_week", "Admission_count")),
    showlegend = FALSE
  ),
  # Organise plots
  nrows = 2, heights = c(0.8,0.2), shareY = TRUE, shareX = TRUE 
) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0('<b>Richer people received a greater than average proportion of hip care during lockdown<b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>Proportion and count of elective hip procedures by deprivation quintile: National, weekly average<i>',
                                    '</sup>'))) 
# Stacked bar chart
# 2018 average
c_ip_hip_imd_2018 <-
  c_ip_hip_imd %>% 
  filter(Admission_Date > "2018-01-01" & 
           Admission_Date < "2018-12-31") %>% 
  drop_na(IMD_Quintile) %>% 
  group_by(IMD_Quintile) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Date = "2018-01-01") %>%
  
  ggplot(aes(x = as.Date(Date), y = prop, fill = as.character(IMD_Quintile))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  labs(x = "2018",
       y = "Proportion")

# 2019 average
c_ip_hip_imd_2019 <-
  c_ip_hip_imd %>% 
  filter(Admission_Date > "2019-01-01" & 
           Admission_Date < "2019-12-31") %>% 
  drop_na(IMD_Quintile) %>% 
  group_by(IMD_Quintile) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Date = "2019-01-01") %>%
  
  ggplot(aes(x = as.Date(Date), y = prop, fill = as.character(IMD_Quintile))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text = element_blank(),
        legend.position = "none") +
  labs(x = "2019",
       y = "")

# Weekly average
c_ip_hip_IMD_2020_wk <-
  c_ip_hip_imd %>% 
  filter(Admission_Date > "2020-01-01" & 
           Admission_Date < "2020-12-31") %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, IMD_Quintile) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  
  ggplot(aes(x = as.Date(Admission_week), y = prop, fill = as.character(IMD_Quintile))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.y = element_blank()) +
  labs(x = "Admission week",
       y = "",
       fill = "IMD Quintile")

# Patch together 
c_ip_hip_imd_2018 + c_ip_hip_imd_2019 + c_ip_hip_IMD_2020_wk + 
  plot_layout(ncol = 3, widths = c(1,1,4)) +
  plot_annotation(title = "Proportion of elective hip procedures by deprivation quintile",
                  subtitle = "National, 2018 and 2019 annual averages compared to 2020 weekly averages")

# Stream graph
streamgraph(c_ip_hip_imd_wk %>% 
              filter(Admission_week > "2020-01-01" & 
                       Admission_week < "2020-12-31") %>% 
              ungroup(), 
            key="IMD_Quintile",
            value="prop",  
            date="Admission_week" #, height="300px", width="1000px"
            )

## C.2.2. Deprivation x Age ####
c_ip_hip_imd_age_wk <-
  a.ip_hip  %>% 
  select(1:9, procedure_type, Age_range) %>% 
  left_join(select(c_lsoa_imd, LSOA_Code, IMD_Score, IMD_Rank, IMD_Decile), by = c("Der_Postcode_LSOA_2011_Code" = "LSOA_Code")) %>% 
  mutate(IMD_Quintile = case_when(IMD_Decile %in% c(1,2) ~ 1,
                                  IMD_Decile %in% c(3,4) ~ 2,
                                  IMD_Decile %in% c(5,6) ~ 3,
                                  IMD_Decile %in% c(7,8) ~ 4,
                                  IMD_Decile %in% c(9,10) ~ 5,
                                  TRUE ~ 99)) %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, Age_range, IMD_Quintile) %>% 
  summarise(Admissions = n_distinct(APCS_Ident)) %>%
  ungroup() %>% group_by(Admission_week) %>% 
  mutate(weekly_sum = sum(Admissions)) %>% 
  mutate(Proportion = Admissions/weekly_sum*100) %>% 
  mutate(Proportion = round(Proportion, 2)) 

c_ip_hip_imd_age_wk_vis <-
c_ip_hip_imd_age_wk %>% 
  filter(Age_range %in% c("40-59", "60-79", "80-99")) %>% 
  drop_na(Age_range) %>% 
  filter(substr(Admission_week,1,4) > 2018) %>% 
  ggplot(aes(x = Admission_week, y = Proportion, 
             colour = as.character(IMD_Quintile), group = as.character(IMD_Quintile))) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 20, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 20, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  geom_point( alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  facet_grid(IMD_Quintile ~ Age_range, scale = "free_y") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73"),
        legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(title = "Deprivation has distinct influences on care access, independent of age range",
       subtitle = "Weekly proportion of hip procedures performed by age range and deprivation: National, elective inpatient episodes",
       x = "Admission week",
       y = "Proportion admissions",
       colour = "")


# Markdown chunk
c_ip_hip_imd_age_wk_vis %>% 
  ggplotly(tooltip = c("Admission_week","Admissions", "Proportion")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0(
           '<b>Weekly proportion of hip procedures performed by age range and deprivation<b>',
           '<br>',
           '<sup>',
           #'<i>National, planned and unplanned care<i>',
           '</sup>'))) 


## C.3. Ethnicity ####
c_ethnicity_lookup <-
tribble(
  ~Ethnic_Category, ~Ethnic_Category_Desc, ~Ethnicity_broad,
 
  "99",	"Not known", "Not known",	
  "A",	"British", "White",
  "B",	"Irish"	, "White",
  "C",	"Any other white background", "White",
  "D",	"White and black caribbean",	"Mixed",
  "E",	"White and black african",	"Mixed",
  "F",	"White and asian",	"Mixed",
  "G",	"Any other mixed background", "Mixed",
  "H",	"Indian", "Asian",
  "J",	"Pakistani",	"Asian",
  "K",	"Bangladeshi",	"Asian",
  "L",	"Any other asian background", "Asian",
  "M",	"Caribbean",	"Black",
  "N",	"African", "Black",
  "P",	"Any other black background", "Black",
  "R",	"Chinese", "Asian",
  "S",	"Any other ethnic group", "Other",
  "Z",	"Not stated", "Not known",
  "NA", "Not known", "Not known"
  )

## C.3.1. Full ethnicity ####
c_ip_hip_ethnicity <-
  a.ip_hip %>% 
  mutate(Ethnic_Group = case_when(Ethnic_Group %in% c("99", "NA") ~ Ethnic_Group,
                                  TRUE ~ substr(Ethnic_Group,1,1)
                                  )) %>% # Shorten Ethnic group to first character to clean data (except when coded as 99 or NA)
  mutate(Ethnic_Group = case_when(is.na(Ethnic_Group) ~ "99",
                                  TRUE ~ Ethnic_Group)) %>% # Replace NA's with "NA" to left_join to "Not Known"
  left_join(c_ethnicity_lookup, by = c("Ethnic_Group" = "Ethnic_Category")) %>% 
  group_by(Admission_Date, Ethnic_Group, Ethnic_Category_Desc, Ethnicity_broad) %>% 
  summarise(Admissions = n_distinct(APCS_Ident)) %>% 
  ungroup() %>%  group_by(Admission_Date) %>% 
  mutate(daily_sum = sum(Admissions)) %>% 
  mutate(prop = Admissions/daily_sum*100)

# Weekly ethnicity proportions 
c_ip_hip_ethnicity_wk <-
  c_ip_hip_ethnicity %>% 
  ungroup() %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, Ethnic_Category_Desc) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Proportion = round(prop, digits = 2)) %>% 
  drop_na(Proportion) %>% 
  mutate(Ethnicity = Ethnic_Category_Desc) %>% 
  ungroup()

c_ip_hip_ethnicity_wk_vis <-
c_ip_hip_ethnicity_wk %>% 
  ggplot(aes(x = Admission_week, y = Proportion, group = Ethnic_Category_Desc, colour = Ethnicity )) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 100, fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 100, fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 45, label = "National lockdown",size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 45, label = "3 Tier lockdown",size = 3, angle = 90) +
  geom_point( alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  labs(title = "Weekly proportion of hip procedures performed by ethnicity",
       subtitle = "National, elective inpatient episodes",
       x = "Admission week",
       y = "Proportion admissions",
       colour = "Ethnicity")

# Markdown chunk
c_ip_hip_ethnicity_wk_vis %>% 
  ggplotly(tooltip = c("Admission_week","Proportion", "Ethnicity")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0(
           '<b>Weekly proportion of hip procedures performed by ethnicity<b>',
           '<br>',
           '<sup>',
           '<i>National, elective inpatient episodes<i>',
           '</sup>'))) 


c_ethnicity_lookup %>% 
  select(Ethnicity_broad, Ethnic_Category_Desc) %>% 
  arrange(Ethnicity_broad,Ethnic_Category_Desc) 

# Asain           Any other asian background
# Asain           Bangladeshi               
# Asain           Chinese                   
# Asain           Indian                    
# Asain           Pakistani                 
# Black           African                   
# Black           Any other black background
# Black           Caribbean                 
# Mixed           Any other mixed background
# Mixed           White and asian           
# Mixed           White and black african   
# Mixed           White and black caribbean 
# Not known       Not known                 
# Not known       Not known                 
# Not known       Not stated                
# Other           Any other ethnic group    
# White           Any other white background
# White           British                   
# White           Irish  


# Facet wrap
c_ip_hip_ethnicity_wk_vis_facet <-
  c_ip_hip_ethnicity_wk %>% 
  left_join(c_ethnicity_lookup, by = c("Ethnic_Category_Desc")) %>% 
  mutate(
    Ethnicity = factor(
      Ethnicity,
        levels = c(
          # Asain
          "Bangladeshi", "Chinese", "Indian", "Pakistani", "Any other asian background",
          # Black
          "African", "Caribbean", "Any other black background",
          # Mixed
          "White and asian", "White and black african", "White and black caribbean", "Any other mixed background",
          # Not known
          "Not known", "Not stated", "Any other ethnic group",
          # White
          "British", "Irish", "Any other white background"
          ))) %>% 
  ggplot(aes(x = Admission_week, y = Proportion,  colour = Ethnicity_broad )) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = -Inf, ymax = Inf, fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = -Inf, ymax = Inf, fill= "#fed976", alpha = 0.8) + # 3 tier system
  geom_point( alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  facet_wrap(~Ethnicity, scales = "free") +
    theme(strip.text = element_text(size = 7, face = "bold"),
          #strip.background = element_rect(colour = "#686f73"),
          legend.position = c(0.8, 0.09)
          ) +
  guides(col = guide_legend(ncol = 2)) +
  scale_color_SU(palette = "main") +
  labs(title = "Weekly proportion of hip procedures performed by ethnicity",
       subtitle = "National, elective inpatient episodes",
       x = "Admission week",
       y = "Proportion admissions",
       colour = "Broad ethnicity")

# Full ethnicity - stacked bar
c_ip_hip_ethnicity_stack_vis <-
  c_ip_hip_ethnicity_wk %>% 
  
  ggplot(aes(x = Admission_week, y = Proportion, fill = Ethnicity)) +
  geom_bar(position = "stack", stat = "identity", width = 7) +
  scale_fill_SU(palette = "mixed") +
  theme(axis.text.y = element_blank()) +
  labs(title = "Proportion of elective hip procedures by ethnicity",
       subtitle = "National, weekly average", 
       x = "Admission week",
       y = "Proportion",
       fill = "")


# Stacked bar chart (2018 & 2019 annual averages vs 2020 weekly averages) 
# Full ethnicity descriptions
c_ip_hip_ethnicity_stack_vis <-
c_ip_hip_ethnicity_wk %>% 
  rename(Ethnicity = Ethnic_Category_Desc) %>% 
  
  ggplot(aes(x = Admission_week, y = Proportion, fill = Ethnicity)) +
  geom_bar(position = "stack", stat = "identity", width = 7) +
  scale_fill_SU(palette = "mixed") +
  theme(axis.text.y = element_blank()) +
  labs(title = "Proportion of elective hip procedures by ethnicity",
       subtitle = "National, weekly average", 
       x = "Admission week",
       y = "Proportion",
       fill = "")

# Markdown chunk
c_ip_hip_ethnicity_stack_vis %>% 
  ggplotly(tooltip = c("Ethnicity", "Proportion")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0(
           '<b>Proportion of elective hip procedures by ethnicity<b>',
           '<br>',
           '<sup>',
           '<i>National, weekly average<i>',
           '</sup>'))) 

# 2018 average
c_ip_hip_ethnicity_2018 <-
  c_ip_hip_ethnicity %>% 
  filter(Admission_Date > "2018-01-01" & 
           Admission_Date < "2018-12-31") %>% 
  group_by(Ethnic_Category_Desc) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Date = "2018-01-01") %>%
  ggplot(aes(x = as.Date(Date), y = prop, fill = Ethnic_Category_Desc)) +
  geom_bar(position = "stack", stat = "identity") +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  labs(x = "2018",
       y = "Proportion")

# 2019 average
c_ip_hip_ethnicity_2019 <-
  c_ip_hip_ethnicity %>% 
  filter(Admission_Date > "2019-01-01" & 
           Admission_Date < "2019-12-31") %>% 
  group_by(Ethnic_Category_Desc) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Date = "2019-01-01") %>%
  ggplot(aes(x = as.Date(Date), y = prop, fill = Ethnic_Category_Desc)) +
  geom_bar(position = "stack", stat = "identity") +
  theme(axis.text = element_blank(),
        legend.position = "none") +
  labs(x = "2019",
       y = "")

# Weekly average
c_ip_hip_ethnicity_2020_wk <-
  c_ip_hip_ethnicity_wk %>% 
  filter(Admission_week > "2020-01-01" & 
           Admission_week < "2020-12-31") %>% 
  ggplot(aes(x = as.Date(Admission_week), y = prop, fill = Ethnic_Category_Desc)) +
  geom_bar(position = "stack", stat = "identity") +
  theme(axis.text.y = element_blank()) +
  labs(x = "Admission week",
       y = "",
       fill = "Ethnicity")

# Patch together 
c_ip_hip_ethnicity_2018 + c_ip_hip_ethnicity_2019 + c_ip_hip_ethnicity_2020_wk + 
  plot_layout(ncol = 3, widths = c(1,1,4)) +
  plot_annotation(title = "Proportion of elective hip procedures by ethnicity",
                  subtitle = "National, 2018 and 2019 annual averages compared to 2020 weekly averages")


## C.3.2. Broad ethnicity ####
c_ip_hip_ethnicity_broad <-
  c_ip_hip_ethnicity %>% 
  ungroup() %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week, Ethnicity_broad) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  ungroup() %>% 
  mutate(Proportion = round(prop, digits = 2)) %>% 
  drop_na(Proportion)

c_ip_hip_ethnicity_broad_vis <-
  c_ip_hip_ethnicity_broad %>% 
  filter(Admission_week < "2021-01-03") %>% 
  ggplot(aes(x = Admission_week, y = Proportion, group = Ethnicity_broad, colour = Ethnicity_broad  )) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 100, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 100, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 50, label = "National lockdown",size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 50, label = "3 Tier lockdown",size = 3, angle = 90) +
  geom_point( alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  labs(title = "Weekly proportion of hip procedures performed by ethnicity",
       subtitle = "National, elective inpatient episodes",
       x = "Admission week",
       y = "Proportion admissions",
       colour = "")

#Facet wrap broad ethnicities 
c_ip_hip_ethnicity_broad_facet <-
  c_ip_hip_ethnicity_broad %>%
  drop_na(Proportion) %>% 
  filter(Admission_week < "2021-01-03") %>% 
  
  ggplot(aes(x = Admission_week, y = Proportion, colour = Ethnicity_broad  )) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = -Inf, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = -Inf, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  geom_point( alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_color_SU(palette = "main") +
  facet_wrap(~Ethnicity_broad, scale = "free_y") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73"),
        legend.position = "none") +
  labs(title = "Weekly proportion of hip procedures performed by ethnicity",
       subtitle = "National, elective inpatient episodes",
       x = "Admission week",
       y = "Proportion admissions",
       colour = "")

# Markdown chunk
c_ip_hip_ethnicity_broad_vis %>% 
  ggplotly(tooltip = c("Ethnicity", "Proportion")) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0(
           '<b>Weekly proportion of hip procedures performed by broad ethnicity<b>',
           '<br>',
           '<sup>',
           '<i>National, weekly average<i>',
           '</sup>'))) 

# Stacked bar chart 
c_ip_hip_ethnicity_broad_stack_vis <-
  c_ip_hip_ethnicity_broad %>%  
  mutate(Count = sum) %>% 
  ggplot(aes(x = Admission_week, y = Proportion, fill = Ethnicity_broad)) +
  geom_bar(position = "stack", stat = "identity", width = 7) +
  scale_fill_SU(palette = "main") +
  theme(axis.text.y = element_blank()) +
  labs(title = "Proportion of elective hip procedures by broad ethnicity",
       subtitle = "National, weekly average", 
       x = "Admission week",
       y = "Proportion",
       fill = "")


# Stacked bar and line graph
c_hip_ip_stacked_bar_and_line_ethnicity <-
  subplot(
    # Stacked bar 
    ggplotly(c_ip_hip_ethnicity_broad_stack_vis +
               annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = 0, yend = 100, lty = 2) +
               annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = 0, yend = 100, lty = 2) +
               annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = 0, yend = 100, lty = 2) +
               annotate("segment", x = as.Date("2021-01-04"), xend = as.Date("2021-01-04"), y = 0, yend = 100, lty = 2),
             tooltip = c("Admission_week","Proportion", "Count")
    ),
    
    # Line chart
    style(
      ggplotly(c_ip_hip_admission_volume +
                 annotate("segment", x = as.Date("2021-01-04"), xend = as.Date("2021-01-04"), y = 0, yend = 2800, lty = 2) +
                 annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = 0, yend = 2800, lty = 2) +
                 annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = 0, yend = 2800, lty = 2) +
                 annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = 0, yend = 2800, lty = 2),
               tooltip = c("Admission_week", "Admission_count")),
      showlegend = FALSE
    ),
    # Organise plots
    nrows = 2, heights = c(0.8,0.2), shareY = TRUE, shareX = TRUE 
  ) %>% 
  layout(legend = list(y = 0.5),
         title = list(text = paste0('<b>Proportion and count of elective hip procedures by broad ethnicity<b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>National, weekly average<i>',
                                    '</sup>'))) 

# Stacked bar chart (2018 & 2019 annual averages vs 2020 weekly averages)
# Broad ethnicities 
# 2018 average
c_ip_hip_ethnicity_broad_2018 <-
  c_ip_hip_ethnicity %>% 
  filter(Admission_Date > "2018-01-01" & 
           Admission_Date < "2018-12-31") %>% 
  group_by(Ethnicity_broad) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Date = "2018-01-01") %>%
  ggplot(aes(x = as.Date(Date), y = prop, fill = Ethnicity_broad)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_SU(palette = "main") +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  labs(x = "2018",
       y = "Proportion")

# 2019 average
c_ip_hip_ethnicity_broad_2019 <-
  c_ip_hip_ethnicity %>% 
  filter(Admission_Date > "2019-01-01" & 
           Admission_Date < "2019-12-31") %>% 
  group_by(Ethnicity_broad) %>% 
  summarise(sum = sum(Admissions)) %>% 
  mutate(prop = sum/sum(sum)*100) %>% 
  mutate(Date = "2019-01-01") %>%
  ggplot(aes(x = as.Date(Date), y = prop, fill = Ethnicity_broad)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_SU(palette = "main") +
  theme(axis.text = element_blank(),
        legend.position = "none") +
  labs(x = "2019",
       y = "")

# Weekly average
c_ip_hip_ethnicity_broad_2020_wk <-
  c_ip_hip_ethnicity_broad %>% 
  filter(Admission_week > "2020-01-01" & 
           Admission_week < "2020-12-31") %>% 
  ggplot(aes(x = as.Date(Admission_week), y = prop, fill = Ethnicity_broad)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_SU(palette = "main") +
  theme(axis.text.y = element_blank()) +
  labs(x = "Admission week",
       y = "",
       fill = "Ethnicity")

# Patch together 
c_ip_hip_ethnicity_broad_2018 + c_ip_hip_ethnicity_broad_2019 + c_ip_hip_ethnicity_broad_2020_wk + 
  plot_layout(ncol = 3, widths = c(1,1,4)) +
  plot_annotation(title = "Proportion of elective hip procedures by broad ethnicity",
                  subtitle = "National, 2018 and 2019 annual averages compared to 2020 weekly averages")


## C.4. Geography ####
c_ip_hip_nhsr <-
a.ip_hip %>% 
  group_by(nhser20nm, Admission_Date) %>% 
  summarise(n_admissions = n_distinct(APCS_Ident)) %>% 
  mutate(Admission_Date = as.Date(Admission_Date)) %>% 
  filter(Admission_Date > 2018-01-01) 

c_ip_hip_nhsr_wk <-
  c_ip_hip_nhsr %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, nhser20nm) %>% 
  summarise(weekly_admissions = sum(n_admissions))

c_ip_hip_nhsr_month <-
  c_ip_hip_nhsr %>% 
  mutate(Admission_month = floor_date(as.Date(Admission_Date), unit = "month")) %>% 
  ungroup() %>% 
  group_by(Admission_month, nhser20nm) %>% 
  summarise(monthly_admissions = sum(n_admissions))

c_ip_hip_nhsr_vis <-  
c_ip_hip_nhsr %>% 
  ungroup() %>% 
  filter(Admission_Date > "2018-01-01" & 
           Admission_Date < "2020-12-31") %>%
  mutate(week_day = weekdays(Admission_Date)) %>% 
  mutate(weekend_flag = case_when(substr(week_day,1,1) == "S" ~ "Weekend",
                                  TRUE ~ "Week day")) %>% 
  
  ggplot(aes(x = Admission_Date, y = n_admissions)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  #annotate("text", x = as.Date("2020-05-13"), y = 100, label = "National lockdown",size = 2, angle = 90) +
  #annotate("text", x = as.Date("2020-11-24"), y = 100, label = "3 Tier lockdown",size = 2, angle = 90) +
  #geom_point(aes(colour = weekend_flag), size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  facet_wrap(~nhser20nm, scales ="free_y") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73"),
        legend.position = c(0.8, 0.2)) +
  scale_color_SU(palette = "main") +
  labs(title = "Inpatient activity: Hip replacement procedures",
       subtitle = "NHS England regions, planned care",
       colour = "",
       x = "Date",
       y = "Daily Admissions")

# Weekly time series - NHSE Region  
c_ip_hip_nhsr_wide <-
  c_ip_hip_nhsr_wk %>% 
  pivot_wider(id_cols = Admission_week, 
              names_from = nhser20nm, 
              values_from = weekly_admissions) %>% 
  clean_names()
 
East_England <- xts(c_ip_hip_nhsr_wide$east_of_england, order.by = c_ip_hip_nhsr_wide$admission_week)         
London <- xts(c_ip_hip_nhsr_wide$london, order.by = c_ip_hip_nhsr_wide$admission_week)            
Midlands <- xts(c_ip_hip_nhsr_wide$midlands, order.by = c_ip_hip_nhsr_wide$admission_week)                
NorthEast_and_Yorkshire <- xts(c_ip_hip_nhsr_wide$north_east_and_yorkshire, order.by = c_ip_hip_nhsr_wide$admission_week) 
North_West <- xts(c_ip_hip_nhsr_wide$north_west, order.by = c_ip_hip_nhsr_wide$admission_week)              
South_East <- xts(c_ip_hip_nhsr_wide$south_east, order.by = c_ip_hip_nhsr_wide$admission_week)              
South_West <- xts(c_ip_hip_nhsr_wide$south_west, order.by = c_ip_hip_nhsr_wide$admission_week) 

nhsr_ip_hip <-
  cbind(East_England, 
        London, 
        Midlands,
        NorthEast_and_Yorkshire,
        North_West,
        South_East,
        South_West
        )

dygraph_ip_hip_nhsr_week <-
  dygraph(nhsr_ip_hip , 
          main = "Inpatient Hip activity, NHS England Regions 2018-20", 
          xlab = "Date",
          ylab = "Weekly activity count") %>%
  dyOptions(drawGrid = FALSE, 
            colors = c("#f9bf07","#2c2825","#686f73","#5881c1","#ec6555", "#ffeda0", "#e6550d"),
            fillGraph = FALSE,
            strokeWidth = 5) %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 1, 
              hideOnMouseOut = FALSE) %>% 
  dyShading(from = "2020-03-23", to = "2020-07-04", color = "#d9d9d9") %>% 
  dyShading(from = "2020-10-1", to = "2021-01-04", color = "#d9d9d9") %>% 
  dyAnnotation("2020-05-13", text = "National Lockdown") %>% 
  dyAnnotation("2020-11-24", text = "3 Tier lockdown") 
  
  
# Monthly time series - NHSE region  
c_ip_hip_nhsr_wide_month <-
  c_ip_hip_nhsr_month %>% 
  pivot_wider(id_cols = Admission_month, 
              names_from = nhser20nm, 
              values_from = monthly_admissions) %>% 
  clean_names()
  
East_England_month            <- xts(c_ip_hip_nhsr_wide_month$east_of_england, order.by = c_ip_hip_nhsr_wide_month$admission_month)         
London_month                  <- xts(c_ip_hip_nhsr_wide_month$london, order.by = c_ip_hip_nhsr_wide_month$admission_month)            
Midlands_month                <- xts(c_ip_hip_nhsr_wide_month$midlands, order.by = c_ip_hip_nhsr_wide_month$admission_month)                
NorthEast_and_Yorkshire_month <- xts(c_ip_hip_nhsr_wide_month$north_east_and_yorkshire, order.by = c_ip_hip_nhsr_wide_month$admission_month) 
North_West_month              <- xts(c_ip_hip_nhsr_wide_month$north_west, order.by = c_ip_hip_nhsr_wide_month$admission_month)              
South_East_month              <- xts(c_ip_hip_nhsr_wide_month$south_east, order.by = c_ip_hip_nhsr_wide_month$admission_month)              
South_West_month              <- xts(c_ip_hip_nhsr_wide_month$south_west, order.by = c_ip_hip_nhsr_wide_month$admission_month) 
  
nhsr_ip_hip_month <-
  cbind(East_England_month, 
        London_month, 
        Midlands_month,
        NorthEast_and_Yorkshire_month,
        North_West_month,
        South_East_month,
        South_West_month
  )
  
dygraph_ip_hip_nhsr_month <-
  dygraph(nhsr_ip_hip_month , 
        main = "Inpatient Hip activity, NHS England Regions 2018-20", 
        xlab = "Date",
        ylab = "Monthly activity count") %>%
  dyOptions(drawGrid = FALSE, 
              colors = c("#f9bf07","#2c2825","#686f73","#5881c1","#ec6555", "#ffeda0", "#e6550d"),
              fillGraph = FALSE,
              strokeWidth = 2.5) %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 1, 
                hideOnMouseOut = FALSE) %>% 
  dyShading(from = "2020-03-23", to = "2020-07-04", color = "#d9d9d9") %>% 
  dyShading(from = "2020-10-1", to = "2021-01-04", color = "#d9d9d9") %>% 
  dyAnnotation("2020-05-13", text = "National Lockdown") %>% 
  dyAnnotation("2020-11-24", text = "3 Tier lockdown")  

c_ip_hip_nhsr_month_smooth <-
c_ip_hip_nhsr_month %>% 
  ungroup() %>% 
  ggplot(aes(x = Admission_month, 
             y = monthly_admissions, 
             group = nhser20nm,
             colour = nhser20nm)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = 3000, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = 3000,
           fill= "#fed976", alpha = 0.8) +
  annotate("text", x = as.Date("2020-05-13"), y = 2500, label = "National lockdown",size = 3.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 2300, label = "3 Tier lockdown",size = 3.5, angle = 90) +
  geom_smooth(method = "loess", span = 0.2) +
  scale_color_SU(palette = "main") +
  labs(title = "Monthly hip replacement inpatient activity by NHSE Region",
       subtitle = "National, planned care",
       x = "Date",
       y = "Monthly admissions",
       colour = "")







## E.3. Comparing impact: Pre-covid mean #### 
e_hip_ip_week_number <-
  b_hip_ip_daily %>% 
  mutate(year = year(Admission_Date),
         month = months(Admission_Date), 
         week_number = isoweek(Admission_Date)) %>% 
  group_by(year, week_number) %>% 
  summarise(weekly_admissions = sum(n_admissions)) %>% 
  ungroup()

e_hip_ip_week_number_wide <-
e_hip_ip_week_number %>% 
  filter(year == 2020) %>% 
  rename(n_2020 = weekly_admissions) %>% 
  select(-year) %>% 
  left_join(e_hip_ip_week_number %>% 
              filter(year == 2018) %>% 
              select(-year) %>% 
              rename(n_2018 = weekly_admissions), by = c("week_number"), keep = FALSE) %>% 
  left_join(e_hip_ip_week_number %>% 
              filter(year == 2019) %>% 
              select(-year) %>% 
              rename(n_2019 = weekly_admissions), by = c("week_number"), keep = FALSE) %>% 
  group_by(week_number) %>% 
  mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
  ungroup() %>% 
  mutate(def = Avg1819 - n_2020) %>% 
  mutate(PC_def = case_when(
    week_number < 13 ~ 0,
    week_number >= 13 ~ Avg1819 - n_2020)) # Additional variable to start deficit from lockdown start


# "#f9bf07", "#686f73", "#5881c1", "#ec6555"
e_hip_ip_annual_trends <-
e_hip_ip_week_number_wide %>%   
  ggplot(aes(x = week_number, y = n_2020)) +
  geom_smooth(method = "loess", span = 0.2, colour = "#f9bf07") +
  geom_smooth(aes(y = Avg1819), method = "loess", span = 0.2, colour = "#5881c1") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 3250, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 3250, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 3250, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  # Labels and arrows 
  annotate(geom = "curve", x = 23, y = 1725, xend = 23, yend = 2250, 
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "curve", x = 35, y = 500, xend = 33, yend = 850, 
          curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 18, y = 1725, label = str_wrap("2018-19 average",10), size = 3) +
  annotate("text", x = 38, y = 500, label = "2020", size = 3) +
  labs(x = "Week number", y = "Admissions")

# Visualise admission deficit 
e_hip_ip_admission_def_vis <-
e_hip_ip_week_number_wide %>% 
  ggplot(aes(x = week_number, y = PC_def)) +
  geom_smooth(method = "loess", span = 0.2, colour = "#f9bf07") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 3250, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 3250, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 3250, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  # Labels and arrows 
  annotate(geom = "curve", x = 33, y = 500, xend = 34, yend = 850, 
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 31, y = 500, label = "2020", size = 3) +
  labs(x = "Week number", y = "Admission deficit")

# Patch
e_hip_ip_annual_trends + e_hip_ip_admission_def_vis + 
  plot_annotation(title = "Yearly patterns in hip replacement admissions",
                        subtitle = "National, Weekly elective inpatient procedures, 2018-20"
                        )

# Total admission deficit number to quote
e_hip_total_admission_def <-
e_hip_ip_week_number_wide %>% 
  select(def) %>% 
  sum(na.rm = TRUE)
  

## E.3.2. Demographic index: Comparing 2020 to 18/19 mean by subgroups ####
# Admission count by demographic subgroups  
e_hip_ip_demographic_index <-
a.ip_hip %>% 
  #Ethnicity
  mutate(
    Ethnic_Group = case_when(Ethnic_Group %in% c("99", "NA") ~ Ethnic_Group,
                             TRUE ~ substr(Ethnic_Group,1,1)
                             )) %>% # Shorten Ethnic group to first character to clean data (except when coded as 99 or NA)
  mutate(
    Ethnic_Group = case_when(is.na(Ethnic_Group) ~ "99",
                             TRUE ~ Ethnic_Group)) %>% # Replace NA's with "NA" to left_join to "Not Known"
  left_join(c_ethnicity_lookup, by = c("Ethnic_Group" = "Ethnic_Category")) %>% 
  # Deprivation 
  left_join(select(c_lsoa_imd, LSOA_Code, IMD_Score, IMD_Rank, IMD_Decile), by = c("Der_Postcode_LSOA_2011_Code" = "LSOA_Code")) %>% 
  mutate(IMD_Quintile = case_when(IMD_Decile %in% c(1,2) ~ 1,
                                  IMD_Decile %in% c(3,4) ~ 2,
                                  IMD_Decile %in% c(5,6) ~ 3,
                                  IMD_Decile %in% c(7,8) ~ 4,
                                  IMD_Decile %in% c(9,10) ~ 5,
                                  TRUE ~ 99)) %>% 
  # Activity count by demographic and region
  mutate(Admission_Date = as.Date(Admission_Date)) %>% 
  group_by(Admission_Date, nhser20nm, Age_range, Sex, Ethnic_Category_Desc, Ethnicity_broad, IMD_Quintile) %>% 
  summarise(Admission_count = n_distinct(APCS_Ident)) %>% 
  ungroup()


# Function: get admission count and deficit by demographic 
e_get_admission_deficit <- function(demographic_var) {
  
  e_hip_ip_demographic_index <- 
    e_hip_ip_demographic_index %>%
    mutate(demographic_var = {{demographic_var}})
  
  sub_data <-
  e_hip_ip_demographic_index %>% 
    mutate(year = year(Admission_Date),
           month = months(Admission_Date), 
           week_number = isoweek(Admission_Date)) %>% 
    group_by(year, week_number, demographic_var) %>% 
    summarise(weekly_admissions = sum(Admission_count)) %>% 
    ungroup()
  
  sub_data_wide <-
    sub_data %>% 
    filter(year == 2020) %>% 
    rename(n_2020 = weekly_admissions) %>% 
    select(-year) %>% 
    left_join(sub_data %>% 
                filter(year == 2018) %>% 
                select(-year) %>% 
                rename(n_2018 = weekly_admissions), by = c("week_number", 'demographic_var'), keep = FALSE) %>% 
    left_join(sub_data %>% 
                filter(year == 2019) %>% 
                select(-year) %>% 
                rename(n_2019 = weekly_admissions), by = c("week_number", 'demographic_var'), keep = FALSE) %>% 
    group_by(week_number, demographic_var) %>% 
    mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(def = Avg1819 - n_2020) %>% 
    mutate(PC_def = case_when(
      week_number < 13 ~ 0,
      week_number >= 13 ~ Avg1819 - n_2020)) %>%  # Additional variable to start deficit from lockdown start
    ungroup() %>% 
    filter(week_number != 53)
  
  sub_data_wide
}

# Function: Plot 2020 vs 2018/19 average, facet by demographic
facet_plot_function <- function(data) {
  
  data %>% 
    pivot_longer(cols = c(-week_number, - demographic_var),
                 names_to = "trend", 
                 values_to = "Admission_count") %>% 
    filter(trend %in% c("n_2020", "Avg1819")) %>% 
    mutate(trend = case_when(trend == "n_2020" ~ "2020",
                             trend == "Avg1819" ~ "2018-19"))  %>% 
    
    ggplot(aes(x = week_number, y = Admission_count, colour = trend, group = trend)) +
    geom_smooth(method = "loess", span = 0.2) +
    scale_colour_manual(values = c("#f9bf07", "#5881c1" )) +
    scale_y_continuous(limits = c(0, NA_real_), oob = squish) +
    facet_wrap(~ demographic_var) +
    #ylim(0,600) +
    # Covid timeline markers
    annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
    annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
    annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
    annotate("text", x = 20, y = 790, label = str_wrap("Lockdown", 10) ,size = 2.5) +
    annotate("text", x = 34, y = 790, label = str_wrap("Recovery", 10) ,size = 2.5) +
    annotate("text", x = 48, y = 790, label = str_wrap("Lockdown", 10) ,size = 2.5) +
    theme(strip.text = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 12)) 
  }

# Region
facet_plot_function(e_get_admission_deficit(nhser20nm)) +
  theme(legend.position = c(0.85, 0.2)) + 
  labs(x = "Week number", y = "Admissions", colour = "",
       title = "Admission deficit is determied by pre-pandemic admission volume and speed of 'recovery'",
       subtitle = "Elective inpatient hip admissions in 2020 compared to the average from 2018-19: Admissions by NHSE region")


# Admission deficit by region
e_get_admission_deficit(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  ggplot(aes(x = week_number, y = cum_def)) +
  geom_smooth(method = "loess", span = 0.2, colour = "#f9bf07") +
  facet_wrap(~demographic_var) +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = -100, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(title = "The area between annual trends can be quantified at an 'admission deficit'",
       subtitle = "Admission deficit by NHSE region, admissions for planned hip replacements",
       x = "Week number", y = "Admission deficit")

# Cumulative deficit
e_weekly_cum_def_by_region <-
e_get_admission_deficit(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") 

e_weekly_cum_def_by_region %>%
  filter(week_number < 53) %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>% 
  
  ggplot(aes(x = week_number, y = count, colour = demographic_var)) +
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = -100, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.2) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "The Midlands region has the highest absolute admission deficit for hip procedures", 
       subtitle = "Admission deficit for hip replacement procedures due to Covid-19 pandemic",
       x = "Week number",
       y = "Admission deficit")

# Age range
facet_plot_function(e_get_admission_deficit(Age_range) %>% 
                      filter(demographic_var %in% c("40-59", "60-79", "80-99")))

# Deprivation
facet_plot_function(e_get_admission_deficit(IMD_Quintile)) +
  theme(legend.position = c(0.85, 0.2)) + 
  labs(x = "Week number", y = "Admissions", colour = "",
       title = "...........",
       subtitle = "Elective inpatient hip admissions in 2020 compared to the average from 2018-19: Admissions by deprivation quintile")


# Summarise admission deficit by demographic sub-group
# NHSE regions
# Total Admission deficit by region - Table 
e_hip_ip_total_deficit_region <-
  e_get_admission_deficit(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_Avg1819*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = round(add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,2),
         UCI = round(add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100,2))

e_hip_ip_total_deficit_region %>% 
  ggplot(aes(y = reorder(demographic_var, def_prop), x = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.25, colour = "#636363") +
  coord_cartesian(xlim=c(20,60)) +
  labs(title = "Admission deficit due to covid-19 pandemic, by NHSE region",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "NHSE region", 
       y = "Admission deficit proportion")

# Deprivation
e_hip_ip_def_deprivation <-
e_get_admission_deficit(IMD_Quintile) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100) 

e_hip_ip_def_deprivation %>% 
  ggplot(aes(x = reorder(demographic_var, -demographic_var), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
 #  ylim(40,55) +
  geom_hline(yintercept = 48.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "A clear deprivation gradient is present in hip replacemt admission",
       subtitle = "Admission deficit due to covid-19 pandemic, by IMD Quintile",
       x = "Index of multiple deprivation quintile", 
       y = "Admission deficit proportion", 
       caption = "Deficit as a proportion of total expected admissions seen in '18 and '19")

# Comparing proportion of hip procedures by deprivation between 2018/19 avg and 2020
e_get_admission_deficit(IMD_Quintile) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_2020 = sum(n_2020),
            sum_1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(prop_2020 = sum_2020/sum(sum_2020)*100,
         prop_1819 = sum_1819/sum(sum_1819)*100,
         prop_change = prop_2020 - prop_1819) %>%
  
  ggplot(aes(x = demographic_var)) +
  geom_segment(aes(x = demographic_var, xend = demographic_var, y = 0, yend = prop_change)) +
  geom_point(aes(y = prop_change), fill = "orange", colour = "black",  shape = 21, size = 6) +
  geom_hline(aes(yintercept = 0)) +
  labs(title = "The most deprived 20% of the population were impacted the most by delayed or cancelled care", 
       subtitle = "Chaning proportion of planned, inpatient hip procedures before and during the pandemic, by deprivation",
       x = "Deprivation (IMD Quintile)", 
       y = "Percentage point difference",
       caption = "Note: Proportion from 2020 compared to 2018-19 average") 

# Broad ethnicity
e_hip_ip_def_ethnicity_broad <-
e_get_admission_deficit(Ethnicity_broad) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100) 

e_hip_ip_def_ethnicity_broad %>%
  ggplot(aes(x = reorder(demographic_var, def_prop), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  coord_flip() +
  labs(title = "White people were disproportionately disrupted by delayed or cancelled hip replacement care",
       subtitle = "Admission deficit due to covid-19 pandemic, by broad ethnicity",
       x = "Broad ethnicity", 
       y = "Admission deficit proportion",
       caption = "Note: Deficit as a proportion of total expected admissions seen in '18 and '19")

# Ethnicity
e_hip_ip_def_ethnicity <-
e_get_admission_deficit(Ethnic_Category_Desc) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  filter(def_prop > 0) %>% 
  mutate(LCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100) 

e_hip_ip_def_ethnicity %>%
  ggplot(aes(x = reorder(demographic_var, def_prop), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  coord_flip() +
  labs(title = "White people were most disrupted by delayed or cancelled hip replacement care",
       subtitle = "Admission deficit due to covid-19 pandemic, by ethnicity",
       x = "Ethnicity", 
       y = "Admission deficit proportion",
       caption = "Note: Deficit as a proportion of total expected admissions seen in '18 and '19")

# Comparing proportion of hip procedures by ethnicity between 2018/19 avg and 2020
e_get_admission_deficit(Ethnic_Category_Desc) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_2020 = sum(n_2020),
            sum_1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(prop_2020 = sum_2020/sum(sum_2020)*100,
         prop_1819 = sum_1819/sum(sum_1819)*100,
         prop_change = prop_2020 - prop_1819) %>%
  mutate(demographic_var = fct_reorder(demographic_var, prop_change)) %>% 
  
  ggplot(aes(x = demographic_var)) +
  geom_segment(aes(x = demographic_var, xend = demographic_var, y = 0, yend = prop_change)) +
  geom_point(aes(y = prop_change), fill = "orange", colour = "black",  shape = 21, size = 4) +
  coord_flip() +
  labs(title = "The greatest proportional increases in hip admissions were seen in 'Unknown' ethnicities", 
       subtitle = "Chaning proportion of planned, inpatient hip procedures before and during the pandemic, by ethnicity",
       x = "Ethnicity", 
       y = "Percentage point difference",
       caption = "Note: Proportion from 2020 compared to 2018-19 average") 

# Age range
e_hip_ip_def_age_range <-
e_get_admission_deficit(Age_range) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  filter(def_prop > 0) %>% 
  mutate(LCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100) 

e_hip_ip_def_age_range %>%
  ggplot(aes(x = reorder(demographic_var, def_prop), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  coord_flip() +
  labs(title = "Though initially prioritised for hip procedures, eldlerly age groups were most delayed or cancelled",
       subtitle = "Admission deficit due to covid-19 pandemic, by age range",
       x = "Age range", 
       y = "Admission deficit proportion", 
       caption = "Deficit as a proportion of total expected admissions seen in '18 and '19")


## E 3.3. Comparing admissions by demographic and region ####

# Function: get admission count and deficit by demographic and region
e_get_admission_deficit_mult <- function(demographic_var1, demographic_var2) {
  
  e_hip_ip_demographic_index <- 
    e_hip_ip_demographic_index %>%
    mutate(demographic_var1 = {{demographic_var1}},
           demographic_var2 = {{demographic_var2}})
  
  sub_data <-
    e_hip_ip_demographic_index %>% 
    mutate(year = year(Admission_Date),
           month = months(Admission_Date), 
           week_number = isoweek(Admission_Date)) %>% 
    group_by(year, week_number, demographic_var1, demographic_var2) %>% 
    summarise(weekly_admissions = sum(Admission_count)) %>% 
    ungroup()
  
  sub_data_wide <-
    sub_data %>% 
    filter(year == 2020) %>% 
    rename(n_2020 = weekly_admissions) %>% 
    select(-year) %>% 
    left_join(sub_data %>% 
                filter(year == 2018) %>% 
                select(-year) %>% 
                rename(n_2018 = weekly_admissions), by = c("week_number", "demographic_var1", "demographic_var2"), keep = FALSE) %>% 
    left_join(sub_data %>% 
                filter(year == 2019) %>% 
                select(-year) %>% 
                rename(n_2019 = weekly_admissions), by = c("week_number", "demographic_var1", "demographic_var2"), keep = FALSE) %>% 
    group_by(week_number, demographic_var1, demographic_var2) %>% 
    mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(def = Avg1819 - n_2020) %>% 
    mutate(PC_def = case_when(
      week_number < 13 ~ 0,
      week_number >= 13 ~ Avg1819 - n_2020)) %>%  # Additional variable to start deficit from lockdown start
    ungroup() 
  
  sub_data_wide 
  }

# NHSE region x Deprivation
e_hip_ip_nhserXdeprivation <-
e_get_admission_deficit_mult(nhser20nm, IMD_Quintile) %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
          sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  filter(def_prop > 0) %>% 
  mutate(LCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100) %>%
  arrange(demographic_var1, demographic_var2)

e_hip_ip_nhserXdeprivation %>% 
  ggplot(aes(x = demographic_var2, y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  facet_grid(~str_wrap(demographic_var1,12), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73")) +
  labs(title = "Admission deficit due to covid-19 pandemic, by age range",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "IMD Quintile", 
       y = "Admission deficit proportion")

# NHSE region x Ethnicity
e_get_admission_deficit_mult(nhser20nm, Ethnicity_broad) %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  filter(def_prop > 0) %>% 
  #mutate(LCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,
  #       UCI = add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100) %>% 
  
  ggplot(aes(x = reorder(demographic_var2, -def_prop ), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  #geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  facet_wrap(~str_wrap(demographic_var1, 25), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        #strip.background = element_rect(colour = "#686f73"),
        axis.text.x = element_text(angle = 90)
        ) +
  labs(title = "Admission deficit due to covid-19 pandemic, by age range",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "Broad ethnicity", 
       y = "Admission deficit proportion")

# Proportion change 
e_get_admission_deficit_mult(nhser20nm, Ethnicity_broad) %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_n_2020 = sum(n_2020, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(prop_2020 = sum_n_2020/sum(sum_n_2020)*100,
         prop_1819 = sum_Avg1819/sum(sum_Avg1819)*100,
         prop_change = prop_2020 - prop_1819) %>%   # 2020-Avg201819 
  ggplot(aes(x = demographic_var2)) +
  geom_segment(aes(x = demographic_var2, xend = demographic_var2, y = 0, yend = prop_change)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(y = prop_change), fill = "orange", colour = "black",  shape = 21, size = 6) +
  facet_wrap(~demographic_var1) +
  coord_flip() +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Changes in White and Unknown ethnicity proportions are consistent across the country", 
       subtitle = "Chaning proportion of planned, inpatient hip procedures from before to during the pandemic, by ethnicity and region",
       x = "Broad ethnicity", 
       y = "Percentage point difference",
       caption = "Note: Proportion from 2020 compared to 2018-19 average") 
  

# NHSE region x Age range
e_get_admission_deficit_mult(nhser20nm, Age_range) %>% 
  filter(demographic_var2 != "100+") %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE)) %>% 
  left_join(
    e_get_admission_deficit_mult(nhser20nm, Age_range) %>% 
      group_by(demographic_var1, demographic_var2) %>% 
      summarise(sum_n = sum(Avg1819, na.rm = TRUE)), 
    by = c("demographic_var1", "demographic_var2")) %>% 
  mutate(proportion = sum_def/sum_n*100) %>% 
  #filter(proportion > 0) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  #mutate(LCI = add4ci(x = sum_def, n = sum_n,0.95)$conf.int[1]*100,
  #       UCI = add4ci(x = sum_def, n = sum_n,0.95)$conf.int[2]*100) %>% 
  
  ggplot(aes(x = demographic_var2, y = proportion)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  #geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  facet_grid(~str_wrap(demographic_var1, 12), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 90)
  ) +
  labs(title = "Admission deficit due to covid-19 pandemic, by age range",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "Age range", 
       y = "Admission deficit proportion")

# Deprivation x Broad ethnicity
e_hip_ip_def_deprivationXEthnicity_broad <-
e_get_admission_deficit_mult(IMD_Quintile, Ethnicity_broad) %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) %>% 
  mutate(sum_def_adj = case_when(sum_def >= 0 ~ sum_def,
                                 sum_def < 0 ~ -sum_def )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var1, demographic_var2) %>% 
  #filter(def_prop > 0) %>% 
  mutate(LCI = add4ci(x = sum_def_adj, n = sum_Avg1819,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def_adj, n = sum_Avg1819,0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI)) %>%  # Return CI's to negative where the def was negative
  arrange(demographic_var2, demographic_var1)

e_hip_ip_def_deprivationXEthnicity_broad %>% 
  ggplot(aes(x = demographic_var1, y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  facet_grid(~str_wrap(demographic_var2, 12), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73")
        ) +
  labs(title = "Admission deficit due to covid-19 pandemic",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "IMD Quintile", 
       y = "Admission deficit proportion")


# Deprivation x Ethnicity - Asian specific 
e_hip_ip_def_deprivationXEthnicity_broad_Asain <-
e_get_admission_deficit_mult(IMD_Quintile, Ethnic_Category_Desc ) %>% 
  left_join(c_ethnicity_lookup %>% 
              select(-Ethnic_Category), by = c("demographic_var2" = "Ethnic_Category_Desc")) %>% 
  filter(Ethnicity_broad == "Asian") %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = sum_def/sum_Avg1819*100) %>% 
  arrange(desc(def_prop)) 

e_hip_ip_def_deprivationXEthnicity_broad_Asain %>%   
  ggplot(aes(x = demographic_var1, y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_text(aes(label = sum_def), position = position_stack(vjust = 0.5)) +
  facet_grid(~str_wrap(demographic_var2,15)) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73")) +
  labs(title = "Admission deficit due to covid-19 pandemic",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "IMD Quintile", 
       y = "Admission deficit proportion")


# Deprivation x Broad ethnicity - change in proportion 
# Option 1
e_get_admission_deficit_mult(IMD_Quintile, Ethnicity_broad) %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_n_2020 = sum(n_2020, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(prop_2020 = sum_n_2020/sum(sum_n_2020)*100,
         prop_1819 = sum_Avg1819/sum(sum_Avg1819)*100,
         prop_change = prop_2020 - prop_1819) %>% 
  
  ggplot(aes(x = demographic_var2)) +
  geom_segment(aes(x = demographic_var2, xend = demographic_var2, y = 0, yend = prop_change)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(y = prop_change), fill = "orange", colour = "black",  shape = 21, size = 6) +
  facet_wrap(~demographic_var1) +
  coord_flip() +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Trends in broad ethnicity are consistent across deprivation quintile", 
       subtitle = "Chaning proportion of planned, inpatient hip procedures from before to during the pandemic, by ethnicity and deprivation",
       x = "Broad ethnicity", 
       y = "Percentage point difference",
       caption = "Note: Proportion from 2020 compared to 2018-19 average") 

# Option 2
e_get_admission_deficit_mult(IMD_Quintile, Ethnicity_broad) %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_n_2020 = sum(n_2020, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(prop_2020 = sum_n_2020/sum(sum_n_2020)*100,
         prop_1819 = sum_Avg1819/sum(sum_Avg1819)*100,
         prop_change = prop_2020 - prop_1819) %>% 
  ggplot(aes(x = demographic_var1)) +
  geom_segment(aes(x = demographic_var1, xend = demographic_var1, y = 0, yend = prop_change)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(y = prop_change), fill = "orange", colour = "black",  shape = 21, size = 6) +
  facet_wrap(~demographic_var2) +
  coord_flip() +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Trends in broad ethnicity are consistent across deprivation quintile ", 
       subtitle = "Chaning proportion of planned, inpatient hip procedures from before to during the pandemic, by ethnicity and deprivation",
       x = "Broad ethnicity", 
       y = "Percentage point difference",
       caption = "Note: Proportion from 2020 compared to 2018-19 average") 


# Deprivation x Age
e_hip_ip_def_deprivation_age <-
e_get_admission_deficit_mult(IMD_Quintile, Age_range) %>% 
  filter(demographic_var2 != "100+") %>% 
  drop_na(demographic_var1, demographic_var2) %>% 
  group_by(demographic_var1, demographic_var2) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE)) %>% 
  left_join(
    e_get_admission_deficit_mult(IMD_Quintile, Age_range) %>% 
      group_by(demographic_var1, demographic_var2) %>% 
      summarise(sum_n = sum(Avg1819, na.rm = TRUE)), 
    by = c("demographic_var1", "demographic_var2")) %>% 
  mutate(proportion = sum_def/sum_n*100) %>% 
  mutate(sum_def_adj = case_when(sum_def >= 0 ~ sum_def,
                                 sum_def < 0 ~ -sum_def )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var1, demographic_var2) %>% 
  mutate(LCI = add4ci(x = sum_def_adj, n = sum_n,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def_adj, n = sum_n,0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI)) %>%  # Return CI's to negative where the def was negative
  arrange(demographic_var2, demographic_var1) 

e_hip_ip_def_deprivation_age %>%
  ggplot(aes(x = demographic_var1, y = proportion)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  facet_grid(~str_wrap(demographic_var2, 12), scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(colour = "#686f73")
  ) +
  labs(title = "Inequalities according to deprivation are most clear in 20-39 year olds",
       subtitle = "Admission deficit due to covid-19 pandemic, by age range",
       caption = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "IMD Quintile", 
       y = "Admission deficit proportion")


## E.4. Emergency admissions for hip replacement procedures ####
a.ip_hip_unplanned <-
  a.ip_hip_unplanned %>% 
  mutate(Age_range = case_when(Age_at_Start_of_Episode_SUS >= 0 & Age_at_Start_of_Episode_SUS < 20 ~ "0-19",
                               (Age_at_Start_of_Episode_SUS >= 20 & Age_at_Start_of_Episode_SUS <40) ~  "20-39",
                               (Age_at_Start_of_Episode_SUS >= 40 & Age_at_Start_of_Episode_SUS <60) ~  "40-59",
                               (Age_at_Start_of_Episode_SUS >= 60 & Age_at_Start_of_Episode_SUS <80) ~  "60-79",
                               (Age_at_Start_of_Episode_SUS >= 80 & Age_at_Start_of_Episode_SUS <100) ~ "80-99",
                               Age_at_Start_of_Episode_SUS >= 100 ~ "100+")) %>% 
  mutate(Age_range = factor(Age_range, levels = c("0-19", 
                                                  "20-39",
                                                  "40-59",
                                                  "60-79",
                                                  "80-99", 
                                                  "100+"))) %>% 
  left_join(a_lsoa_to_stp, by = c("Der_Postcode_LSOA_2011_Code" = "lsoa11cd")) %>% 
  left_join(a_stp_to_nhsr %>% 
              select(stp20cd, nhser20cd, nhser20nm) %>% 
              distinct(), 
            by = c("stp20cd"))

e_hip_ip_demo_index_unplanned <-
  a.ip_hip_unplanned %>% 
  #Ethnicity
  mutate(
    Ethnic_Group = case_when(Ethnic_Group %in% c("99", "NA") ~ Ethnic_Group,
                             TRUE ~ substr(Ethnic_Group,1,1)
    )) %>% # Shorten Ethnic group to first character to clean data (except when coded as 99 or NA)
  mutate(
    Ethnic_Group = case_when(is.na(Ethnic_Group) ~ "99",
                             TRUE ~ Ethnic_Group)) %>% # Replace NA's with "NA" to left_join to "Not Known"
  left_join(c_ethnicity_lookup, by = c("Ethnic_Group" = "Ethnic_Category")) %>% 
  # Deprivation 
  left_join(select(c_lsoa_imd, LSOA_Code, IMD_Score, IMD_Rank, IMD_Decile), by = c("Der_Postcode_LSOA_2011_Code" = "LSOA_Code")) %>% 
  mutate(IMD_Quintile = case_when(IMD_Decile %in% c(1,2) ~ 1,
                                  IMD_Decile %in% c(3,4) ~ 2,
                                  IMD_Decile %in% c(5,6) ~ 3,
                                  IMD_Decile %in% c(7,8) ~ 4,
                                  IMD_Decile %in% c(9,10) ~ 5,
                                  TRUE ~ 99)) %>% 
  # Activity count by demographic and region
  mutate(Admission_Date = as.Date(Admission_Date)) %>% 
  group_by(Admission_Date, nhser20nm, Age_range, Sex, Ethnic_Category_Desc, Ethnicity_broad, IMD_Quintile) %>% 
  summarise(Admission_count = n_distinct(APCS_Ident)) %>% 
  ungroup()

# Time series
e_hip_ip_demo_index_unplanned %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  group_by(Admission_week) %>% 
  summarise(Admissions = sum(Admission_count)) %>% 
  filter(Admission_week > "2018-01-01" &  Admission_week < "2020-12-31") %>%
  
  ggplot(aes(x = Admission_week, y = Admissions)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = -Inf, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = -Inf, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 1000, label = "National lockdown",size = 3.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 1000, label = "3 Tier lockdown",size = 3.5, angle = 90) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.2) +
  labs(title = "Emergency hip replacements were largely unaffected by the pandemic",
       subtitle = "Weekly admissions for unplanned emergency hip replacement activity: National 2018-20",
       x = "Date",
       y = "Admissions")

# Admission deficit
e_hip_ip_unplanned_week_number <-
  e_hip_ip_demo_index_unplanned %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  group_by(Admission_week) %>% 
  summarise(Admissions = sum(Admission_count)) %>% 
  
  mutate(year = year(Admission_week),
         #month = months(Admission_Date), 
         week_number = isoweek(Admission_week)) %>% 
  group_by(year, week_number) %>% 
  #summarise(weekly_admissions = sum(Admission_count)) %>% 
  ungroup()

e_hip_ip_week_number_wide <-
  e_hip_ip_unplanned_week_number %>% 
  filter(year == 2020) %>% 
  rename(n_2020 = Admissions) %>% 
  select(-year, -Admission_week) %>% 
  left_join(e_hip_ip_unplanned_week_number %>% 
              filter(year == 2018) %>% 
              select(-year, -Admission_week) %>% 
              rename(n_2018 = Admissions), by = c("week_number"), keep = FALSE) %>% 
  left_join(e_hip_ip_unplanned_week_number %>% 
              filter(year == 2019) %>% 
              select(-year, -Admission_week) %>% 
              rename(n_2019 = Admissions), by = c("week_number"), keep = FALSE) %>% 
  group_by(week_number) %>% 
  mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
  ungroup() %>% 
  mutate(def = Avg1819 - n_2020)

e_hip_ip_unplanned_annual_trends <-
  e_hip_ip_week_number_wide %>%   
  ggplot(aes(x = week_number, y = n_2020)) +
  geom_smooth(method = "loess", span = 0.2, colour = "#f9bf07") +
  geom_smooth(aes(y = Avg1819), method = "loess", span = 0.2, colour = "#5881c1") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 1050, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 1050, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 1050, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  ## Labels and arrows 
  annotate(geom = "curve", x = 20, y = 1000, xend = 21.5, yend = 925, 
           curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "curve", x = 19, y = 750, xend = 16, yend = 775, 
           curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 18, y = 1000, label = str_wrap("2018-19 average",10), size = 3.5) +
  annotate("text", x = 20, y = 750, label = "2020", size = 3.5) +
  labs(x = "Week number", y = "Admissions") +
  labs(title = "The initial lockdown caused a small deficit before unplanned admissions recovered",
       subtitle = "Annual trends in weekly admissions for unplanned emergency hip replacement activity: National 2018-20",
       x = "Week number",
       y = "Weekly admissions")
  
e_hip_total_admission_def_unplanned <-
  e_hip_ip_week_number_wide %>% 
  select(def) %>% 
  sum(na.rm = TRUE)  

# Cumulative deficit
e_hip_ip_week_number_wide %>% 
  mutate(cum_def = cumsum(def)) %>% 
  ggplot(aes(x = week_number, y = cum_def)) +
  geom_smooth(method = "loess", span = 0.2, colour = "#f9bf07") +
  #geom_line() +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 250, label = str_wrap("National lockdown", 10) ,size = 4) +
  annotate("text", x = 34, y = 250, label = str_wrap("Recovery period", 10) ,size = 4) +
  annotate("text", x = 48, y = 250, label = str_wrap("3 Tier lockdown", 10) ,size = 4) +
  labs(x = "Week number",
       y = "Cumulative deficit",
       title = "Reductions from the norm in unplanned hip replacements closely match the lockdown timeline", 
       subtitle = "Cumulative unplanned admission deficit for unplanned hip replacement, National",
       caption = "Deficit = difference between 2020 admission count and expected admissions seen in '18 and '19")



# Sub-group proportions
e_get_admission_deficit_hip_unplanned <- function(demographic_var) {
  
  e_hip_ip_demo_index_unplanned <- 
    e_hip_ip_demo_index_unplanned %>%
    mutate(demographic_var = {{demographic_var}})
  
  sub_data <-
    e_hip_ip_demo_index_unplanned %>% 
    mutate(year = year(Admission_Date),
           month = months(Admission_Date), 
           week_number = isoweek(Admission_Date)) %>% 
    group_by(year, week_number, demographic_var) %>% 
    summarise(weekly_admissions = sum(Admission_count)) %>% 
    ungroup()
  
  sub_data_wide <-
    sub_data %>% 
    filter(year == 2020) %>% 
    rename(n_2020 = weekly_admissions) %>% 
    select(-year) %>% 
    left_join(sub_data %>% 
                filter(year == 2018) %>% 
                select(-year) %>% 
                rename(n_2018 = weekly_admissions), by = c("week_number", 'demographic_var'), keep = FALSE) %>% 
    left_join(sub_data %>% 
                filter(year == 2019) %>% 
                select(-year) %>% 
                rename(n_2019 = weekly_admissions), by = c("week_number", 'demographic_var'), keep = FALSE) %>% 
    group_by(week_number, demographic_var) %>% 
    mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(def = Avg1819 - n_2020) %>% 
    mutate(PC_def = case_when(
      week_number < 13 ~ 0,
      week_number >= 13 ~ Avg1819 - n_2020)) %>%  # Additional variable to start deficit from lockdown start
    ungroup() 
  
  sub_data_wide
}

e_get_admission_deficit_hip_unplanned(nhser20nm)
e_get_admission_deficit_hip_unplanned(Age_range)
e_get_admission_deficit_hip_unplanned(Ethnic_Category_Desc)
e_get_admission_deficit_hip_unplanned(IMD_Quintile)


# Region 
# Annual Trends 
e_get_admission_deficit_hip_unplanned(nhser20nm) %>% 
pivot_longer(cols = c(-week_number, - demographic_var),
             names_to = "trend", 
             values_to = "Admission_count") %>% 
  filter(trend %in% c("n_2020", "Avg1819")) %>% 
  mutate(trend = case_when(trend == "n_2020" ~ "2020",
                           trend == "Avg1819" ~ "2018-19"))  %>% 
  
  ggplot(aes(x = week_number, y = Admission_count, colour = trend, group = trend)) +
  geom_smooth(method = "loess", span = 0.2) +
  scale_colour_manual(values = c("#5881c1", "#f9bf07")) +
  facet_wrap(~ demographic_var, scales = "free") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 40, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 40, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 40, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = c(0.8, 0.2)) +
  labs(x = "Week number",
       y = "Admission count",
       title = "",
       subtitle = "",
       colour = "")

# Admission deficit
e_get_admission_deficit_hip_unplanned(nhser20nm) %>%  
  ggplot(aes(x = week_number, y = PC_def)) +
  geom_smooth(method = "loess", span = 0.2, colour = "#f9bf07") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  facet_wrap(~demographic_var) +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = -100, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(title = "The area between annual trends can be quantified at an 'admission deficit'",
       subtitle = "Admission deficit by NHSE region, admissions for planned hip replacements",
       x = "Week number", y = "Admission deficit")

# Cumulative deficit by region 
e_hip_unplanned_def_region <-
e_get_admission_deficit_hip_unplanned(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") 

# Plot together
e_hip_unplanned_def_region %>%
  filter(week_number < 53) %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>% 
  
  ggplot(aes(x = week_number, y = count, colour = demographic_var)) +
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = -100, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess', span = 0.2) +
  geom_label_repel(aes(label = label), nudge_x = 3, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "... ", 
       subtitle = "Admission deficit for unplanned hip replacement procedures due to Covid-19 pandemic",
       x = "Week number",
       y = "Admission deficit")
  
 # Facet wrap
  e_hip_unplanned_def_region %>%
    filter(week_number < 53) %>% 
    
    ggplot(aes(x = week_number, y = count)) +
    annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
    annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
    annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
    annotate("text", x = 20, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
    annotate("text", x = 34, y = -100, label = str_wrap("Recovery", 10) ,size = 3) +
    annotate("text", x = 48, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
    geom_hline(yintercept = 0) +
    geom_smooth(method = 'loess', span = 0.2, colour = "#f9bf07") +
    facet_wrap(~demographic_var) +
    theme(legend.position = "none",
          strip.text = element_text(face = 'bold')) +
    
    labs(title = "London and the South West have consistently had the largest unplanned admission deficits", 
       subtitle = "Admission deficit for unplanned hip replacement procedures due to Covid-19 pandemic",
       x = "Week number",
       y = "Admission deficit")
  # North East Yorkshire had almost reduced the deficit before the second lockdown
  # South West had an excess for a while

#
e_hip_ip_unplanned_def_region <-
e_get_admission_deficit_hip_unplanned(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_Avg1819*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = round(add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,2),
         UCI = round(add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100,2)) 

e_hip_ip_unplanned_def_region %>%
  ggplot(aes(y = reorder(demographic_var, def_prop), x = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.25, colour = "#636363") +
  #coord_cartesian(xlim=c(20,60)) +
  labs(title = "Admission deficit due to covid-19 pandemic, by NHSE region",
       subtitle = "Deficit as a proportion of total expected admissions seen in '18 and '19",
       x = "Admission deficit proportion", 
       y = "NHSE region")

# Region - planned vs unplanned admissions
e_hip_ip_total_deficit_region %>% 
  select(demographic_var, def_prop) %>% 
  rename(planned_def = def_prop) %>% 
  left_join(e_hip_ip_unplanned_def_region %>% 
              select(demographic_var, def_prop) %>% 
              rename(unplanned_def = def_prop), 
            by = c("demographic_var")) %>%
  ungroup() %>% 
  #mutate(planned_avg = mean(planned_def),
  #       unplanned_avg = mean(unplanned_def)) %>% 
  
  ggplot(aes(y = planned_def, x = unplanned_def, label = str_wrap(demographic_var, 12))) + 
  geom_hline(aes(yintercept = mean(planned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text",x = 6.8, y = 46.5, label = "Planned Average",size = 3) +
  geom_vline(aes(xintercept = mean(unplanned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text", x = 6.1, y = 40, label = "Unplanned Average", size = 3, angle = 90) +
  geom_point(fill = "#f9bf07", colour = "black", pch = 21, size = 8) +
  geom_text_repel(box.padding = 1) +
  labs(title = "Distruption of hip replacements is most pronouced in the South West NHSE region ",
       subtitle = "Admission deficit proportion by region and admission type",
       x = "Unplanned deficit proportion",
       y = "Planned deficit proportion ")
 
# Deprivation 
e_hip_ip_unplanned_def_deprivation <-
  e_get_admission_deficit_hip_unplanned(IMD_Quintile) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_Avg1819*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = round(add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[1]*100,2),
         UCI = round(add4ci(x = sum_def, n = sum_Avg1819,0.95)$conf.int[2]*100,2)) 

e_hip_ip_unplanned_def_deprivation %>%
  ggplot(aes(x = reorder(demographic_var, -demographic_var), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  coord_flip() +
  labs(title = "The richest 40% had the smallest proportional reduction in unplanned hip replacements",
       subtitle = "Unplanned hip replacement admission deficit due to covid-19 pandemic, by deprivation",
       y = "Admission deficit proportion", 
       x = "IMD quintile",
       caption = "Deficit as a proportion of total expected admissions seen in '18 and '19"
       )

# Ethnicity
e_hip_ip_unplanned_def_ethnicity <-
  e_get_admission_deficit_hip_unplanned(Ethnic_Category_Desc) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(def, na.rm = TRUE),
            sum_Avg1819 = sum(Avg1819, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_Avg1819*100,2)) %>% 
  arrange(desc(def_prop)) %>%  
  mutate(sum_def_adj = case_when(sum_def >= 0 ~ sum_def,
                                 sum_def < 0 ~ -sum_def
                                 )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var) %>% 
  mutate(LCI = round(add4ci(x = sum_def_adj, n = sum_Avg1819,0.95)$conf.int[1]*100,2),
         UCI = round(add4ci(x = sum_def_adj, n = sum_Avg1819,0.95)$conf.int[2]*100,2)) %>% 
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI) 
         ) # Return CI's to negative where the def was negative

e_hip_ip_unplanned_def_ethnicity %>%
  ggplot(aes(x = reorder(demographic_var, def_prop), y = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25, colour = "#636363") +
  coord_flip() +
  labs(title = "",
       subtitle = "Unplanned hip replacement admission deficit due to covid-19 pandemic, by ethnicity",
       y = "Admission deficit proportion", 
       x = "Ethnicity",
       caption = "Deficit as a proportion of total expected admissions seen in '18 and '19"  )

## E.5. Scatter plots - Planned admission deficit vs unplanned ####
# Region 
e_hip_ip_total_deficit_region %>% 
  select(demographic_var, def_prop) %>% 
  rename(planned_def = def_prop) %>% 
  left_join(e_hip_ip_unplanned_def_region %>% 
              select(demographic_var, def_prop) %>% 
              rename(unplanned_def = def_prop), 
            by = c("demographic_var")) %>%
  ungroup() %>% 
  
  ggplot(aes(y = planned_def, x = unplanned_def, label = str_wrap(demographic_var, 12))) + 
  geom_hline(aes(yintercept = mean(planned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text",x = 6.8, y = 46.5, label = "Planned Average",size = 3) +
  geom_vline(aes(xintercept = mean(unplanned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text", x = 6.1, y = 40, label = "Unplanned Average", size = 3, angle = 90) +
  geom_point(fill = "#f9bf07", colour = "black", pch = 21, size = 8) +
  geom_text_repel(box.padding = 1) +
  labs(title = "Distruption of hip replacements is most pronouced in the South West NHSE region ",
       subtitle = "Admission deficit proportion by region and admission type",
       x = "Unplanned deficit proportion",
       y = "Planned deficit proportion ")

# Deprivation 
e_hip_ip_def_deprivation %>% 
  select(demographic_var, def_prop) %>% 
  rename(planned_def = def_prop) %>% 
  left_join(e_hip_ip_unplanned_def_deprivation %>% 
              select(demographic_var, def_prop) %>% 
              rename(unplanned_def = def_prop), 
            by = c("demographic_var")) %>%
  ungroup() %>% 
  
  ggplot(aes(y = planned_def, x = unplanned_def, label = str_wrap(demographic_var, 12))) + 
  geom_hline(aes(yintercept = mean(planned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text",x = 4, y = 48.4, label = "Planned Average",size = 3.5) +
  geom_vline(aes(xintercept = mean(unplanned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text", x = 5.5, y = 47, label = "Unplanned Average", size = 3.5, angle = 90) +
  geom_point(fill = "#f9bf07", colour = "black", pch = 21, size = 10) +
  geom_text_repel(box.padding = 1.5) +
  labs(title = "Inequities in hip replacement distuption are consistent across admission types",
       subtitle = "Admission deficit proportion by deprivation and admission type",
       x = "Unplanned deficit proportion",
       y = "Planned deficit proportion ")

# Ethnicity
e_hip_ip_def_ethnicity %>% 
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, def_prop, conf) %>% 
  rename(planned_def = def_prop) %>% 
  left_join(e_hip_ip_unplanned_def_ethnicity %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, def_prop, conf) %>% 
              rename(unplanned_def = def_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) %>% 
  
  ggplot(aes(y = planned_def, x = unplanned_def, label = str_wrap(demographic_var, 12))) + 
  geom_hline(aes(yintercept = mean(planned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text",x = 30, y = 25, label = "Planned Average",size = 3.5) +
  geom_vline(aes(xintercept = mean(unplanned_def)), linetype = "dashed", colour = "#636363") +
  annotate("text", x = 7.5, y = 42, label = "Unplanned Average", size = 3.5, angle = 90) +
  geom_point(aes(alpha = conf_tertile), fill = "#f9bf07", colour = "black", pch = 21, size = 10) +
  geom_text_repel(box.padding = 1, size = 3) +
  labs(title = "Proportional disruption in hip replacement care was highest for Pakistani and Indian groups",
       subtitle = "Admission deficit proportion by ethnicity and admission type",
       x = "Unplanned deficit proportion",
       y = "Planned deficit proportion",
       alpha = "Confidence")



## F. QALY effects of distribution: Data for Steven ####
# Admission deficit
#write_csv(e_hip_ip_week_number_wide, "hip_ip_admission_deficit.csv")

# Age and gender profile 
f_hip_ip_Age_Gender_profile <-
a.ip_hip %>% 
  mutate(Admission_Date = as.Date(Admission_Date)) %>% 
  mutate(Period = case_when(
    (Admission_Date >= as.Date("2019-04-01") & Admission_Date <= as.Date("2019-12-31")) ~ "Apr_Dec_19",
    (Admission_Date >= as.Date("2020-04-01") & Admission_Date <= as.Date("2020-12-31")) ~ "Apr_Dec_20",
    TRUE ~ "Other")) %>% 
  filter(Period != "Other") %>% # Exclude not in periods of interest
  filter(Sex %in% c("1", "2")) %>% # Exclude 'Other' genders
  group_by(Sex, Age_at_Start_of_Episode_SUS, Period) %>% 
  summarise(Admission_count = n_distinct(APCS_Ident)) %>% 
  arrange(Age_at_Start_of_Episode_SUS, Period)

#write_csv(f_hip_ip_Age_Gender_profile, "hip_ip_Age_Gender_profile.csv")

f_life_expectancy <-
read_csv("life_expectancy.csv")

f_life_expectancy_discounted <-
  read_csv("life_expectancy_discounted.csv") %>% 
  clean_names()

# Pull in LE and LE discounted
f_hip_ip_Age_Gender_profile_LE <-
  f_hip_ip_Age_Gender_profile %>% 
  left_join(f_life_expectancy %>%
              filter(Gender == "Male") %>% 
              select(Age, ex), 
            by = c("Age_at_Start_of_Episode_SUS" = "Age"), keep = FALSE) %>% 
  rename(male_ex = ex) %>% 
  left_join(f_life_expectancy %>%
              filter(Gender == "Female") %>% 
              select(Age, ex), 
            by = c("Age_at_Start_of_Episode_SUS" = "Age"), keep = FALSE) %>% 
  rename(female_ex = ex) %>% 
  mutate(LE = case_when(Sex == 1 ~ male_ex,
                                     Sex == 2 ~ female_ex)) %>% 
  select(-male_ex, -female_ex) %>% 
  mutate(LE_round = round(LE,0)) %>% 
  left_join(f_life_expectancy_discounted %>% select(1,2), by = c("LE_round" = "le"))
  
# 
f_waiting_times <-
e_hip_ip_week_number_wide %>% 
  mutate(PC_def = case_when(
    week_number < 12 ~ 0,
    week_number >= 12 ~ Avg1819 - n_2020)) %>%  # Additional variable to start deficit from lockdown start
  mutate(cum_def = cumsum(PC_def)) %>% 
  mutate(weeks_left = 52 - week_number + 0.5) %>% 
  mutate(add_weeks_waiting = PC_def*weeks_left) %>% 
  mutate(cum_add_weeks_waiting = cumsum(add_weeks_waiting))

# Literature derived estimate of effect of 1 week delay in hip procedure on QALYs
f_qaly_loss_week <- 0.062/100 


#Total estimated QALY lost in population due to covid
f_total_qaly_loss <-
  sum(f_waiting_times$cum_add_weeks_waiting) * f_qaly_loss_week 

# Cost estimate
# £20,000 per QALY 
# 1 year
f_total_qaly_loss_value <- 
  f_total_qaly_loss *20000

# Life time
f_total_qaly_loss_value_lifetime <-
  f_total_qaly_loss_value * 
  (sum(f_hip_ip_Age_Gender_profile_LE$Admission_count) + 
     sum(f_hip_ip_Age_Gender_profile_LE$le_disc)) / 
  sum(f_hip_ip_Age_Gender_profile_LE$le_disc)

# Disutility waiting 
f_disutility_total <-
  sum(f_waiting_times$cum_add_weeks_waiting) * 19 



## G. Adhoc request(s) ####

## London-style recovery in other regions

# y = mx + c

# So, m will be the slope, c is the intercept.
# I would modify this formula a touch, something like (x - X), where X is the last week of 0. We then have 
# a set of equations that we could solve.

# 0 = m(x-X) + c
# x-X is 0, so c = 0
# Now we have y = m(x-X)
# This time, set x = the week London returned to normal. Then look what the value of y was. We can rearrange
# at that point to get m = y/(x-X)

# Then you can build a function:
#   If x in lockdown: 0
# If x in recovery: m(x-X)
# If x after recovery: use 18/19 value

# oh, the one thing forgot to mention, you need to recalculate m for each region - as the value of y is 
#slightly different

dummy <-
e_get_admission_deficit(nhser20nm) %>% 
  filter(demographic_var == "London")

rm(dummy)

# London recovery starts from week 23 
# Week number between 13 and 23 = 0 admissions
# Week number between 23 and 33 = linear function 
# After week 33 = pre-covid rate 


y <- 1234 # set to whatever the value is at end of recovery period that you want to achieve
X <- 12 # set this to the last week of lockdown
x <- 34 # set this to the week recovery began at, when you reach y
m <- y / (x - X)

# Midlands example
regional_def_midlands <-
  e_get_admission_deficit(nhser20nm) %>% 
  filter(demographic_var == "Midlands") %>% 
  filter(week_number != 53)
  
y <- regional_def_midlands$Avg1819[regional_def_midlands$week_number == 33]
X <- 23
x <- 33
m <- y/(x-X)
xs <- 23:33

regional_def_midlands_pl <- regional_def_midlands %>%
  pivot_longer(c(n_2020, Avg1819)) %>%
  group_nest(name) %>%
  mutate(loess = map(data, ~loess(value ~ week_number, .x, span = 0.2) %>%
                       predict(newdata = .x, se = TRUE) %>%
                       as_tibble())) %>%
  mutate(across(data, map, select, week_number)) %>%
  unnest(c(data, loess)) %>%
  mutate(across(fit, pmax, 0))

regional_def_midlands_pl %>%
  select(week_number, fit, name) %>%
  pivot_wider(names_from = name, values_from = fit) %>%
  mutate(london_type_recovery = case_when(
    week_number < 13 ~ n_2020,
    week_number < 24 ~ 0,
    week_number < 34 ~ sum(Avg1819 * as.numeric(week_number == 33)) / (33 - 24) * (week_number - 24),
    TRUE             ~ Avg1819
  )) %>%
  transmute(week_number, name = "london_type_recovery", fit = london_type_recovery) %>%
  bind_rows(regional_def_midlands_pl) %>%
  mutate(fit_lwr = fit - se.fit, fit_upr = fit + se.fit) %>%
  ggplot(aes(week_number, fit, colour = name)) +
  geom_ribbon(data = ~drop_na(.x, fit_lwr),
              aes(ymin = fit_lwr, ymax = fit_upr, group = name),
              colour = NA, fill = "grey", alpha = 0.5, na.rm = TRUE) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA_real_), oob = squish) 

#test <-
regional_def_midlands %>% 
  mutate(london_type_recovery = case_when(
    week_number < 13 ~ as.numeric(n_2020), # pre-lockdown
    week_number >= 13 & week_number <= 23 ~ 0, # lockdown
    week_number > 23 & week_number <= 33 ~ m*(week_number-X), # London style recovery  <- This bit is wrong ###########
    week_number > 33 ~ Avg1819 # Pre-lockdown levels
  )) %>% 
  
  select(-n_2018, -n_2019, -def, -PC_def, -demographic_var) %>% 
  pivot_longer(cols = -week_number,
               names_to = "Trend", 
               values_to = "Admissions") %>% 
  
  ggplot(aes(x = week_number, y = Admissions, colour = Trend)) +
  #geom_line()
  geom_smooth(method = "loess", span = 0.2)


# Regin specific data frames 

london_type_recovery <- function(.data) {
  df <- .data %>%
    pivot_longer(c(n_2020, Avg1819)) %>%
    group_nest(name) %>%
    mutate(fit = map(data, ~loess(value ~ week_number, .x, span = 0.2) %>%
                         predict(newdata = .x))) %>%
    mutate(across(data, map, select, week_number)) %>%
    unnest(c(data, fit)) %>%
    mutate(across(fit, pmax, 0)) %>%
    pivot_wider(names_from = name, values_from = fit) %>%
    mutate(london_type_recovery = case_when(
      week_number < 13 ~ n_2020,
      week_number < 24 ~ 0,
      week_number < 38 ~ sum(Avg1819 * as.numeric(week_number == 37)) / (37 - 24) * (week_number - 24),
      TRUE             ~ Avg1819
    )) %>%
    pivot_longer(-week_number)
  
  class(df) <- c("london_type_recovery", class(df))
  
  df
}

# s3 generic plot function
plot.london_type_recovery <- function(x, y, ...) {
  x %>%
    ggplot(aes(week_number, value, colour = name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, NA_real_), oob = squish) 
}


# Regional data frames
LTR_Midlands <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                      filter(demographic_var == "Midlands")) %>% 
  mutate(ID = "Midlands")

LTR_EastOfEngland <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                      filter(demographic_var == "East of England")) %>% 
  mutate(ID = "East Of England")

LTR_London <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                           filter(demographic_var == "London")) %>% 
  mutate(ID = "London")

LTR_NorthEast_Yorkshire <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                           filter(demographic_var == "North East and Yorkshire")) %>% 
  mutate(ID = "North East and Yorkshire")

LTR_NorthWest <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                           filter(demographic_var == "North West")) %>% 
  mutate(ID = "North West")

LTR_SouthEast <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                           filter(demographic_var == "South East")) %>% 
  mutate(ID = "South East")

LTR_SouthWest  <-london_type_recovery(e_get_admission_deficit(nhser20nm) %>% 
                                           filter(demographic_var == "South West")) %>% 
  mutate(ID = "South West")

LTR_union <-
LTR_Midlands %>% 
  union(LTR_London) %>% 
  union(LTR_NorthEast_Yorkshire) %>% 
  union(LTR_NorthWest) %>% 
  union(LTR_SouthEast) %>% 
  union(LTR_SouthWest) %>% 
  union(LTR_EastOfEngland) %>% 
  mutate(name = case_when(
    name == "Avg1819" ~ "2018-19",
    name == "london_type_recovery" ~ "Modelled",
    name == "n_2020" ~ "2020"
  ))

LTR_union %>% 
  ggplot(aes(week_number, value, colour = name)) +
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 790, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 790, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 790, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_line(size = 1.1) +
  facet_wrap(~ID) +
  scale_y_continuous(limits = c(0, NA_real_), oob = squish) +
  scale_colour_manual(values = c("#f9bf07", "#5881c1", "#ec6555")) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = c(0.8,0.2)) +
  labs(title = "Comparing regional trends to a London-style recovery highlights potentially unnecessary lost admissions",
       subtitle = "Elective inpatient hip admissions in 2020 compared to the average from 2018-19 and to London-style modelled recovery: Admissions by NHSE region",
       x = "Week number", 
       y = "Admissions",
       colour = "",
       caption = "Note: Modelled trend matches the recovery profile of London NHSE region"
       )

# 

# Regional difference between 2020 trend and London-modelled trend
LTR_union_diff_london <-
LTR_union %>% 
  filter(name != "2018-19",
         ID != "London") %>% 
  pivot_wider(id_cols = c(week_number, ID),
              names_from = name,
              values_from = value) %>% 
  clean_names() %>% 
  mutate(diff_london = modelled - x2020) %>% 
  group_by(id) %>% 
  mutate(diff_london_cum = cumsum(diff_london)) 

LTR_union_diff_london %>% 
  ggplot(aes(week_number, diff_london_cum, colour = id)) +
    geom_line()


LTR_union_diff_london %>%
  summarise(Deficit = sum(diff_london)) %>% 
  mutate(total = sum(Deficit)) %>% 
  rename(Region = id) %>% 
  select(-total) %>% 
  arrange(desc(Deficit))

# # A tibble: 6 x 3
# id                       total_admission_diff  total
# <chr>                                   <dbl>  <dbl>
# 1 East Of England                         2101. 19013.
# 2 Midlands                                4843. 19013.
# 3 North East and Yorkshire                3519. 19013.
# 4 North West                              3159. 19013.
# 5 South East                              1809. 19013.
# 6 South West                              3582. 19013.