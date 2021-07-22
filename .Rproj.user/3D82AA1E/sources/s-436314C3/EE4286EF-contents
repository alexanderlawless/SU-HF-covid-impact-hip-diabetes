### Health Foundation: COVID and diabetes activity 

library(tidyverse)
library(janitor)
library(odbc)
library(DBI)
library(lubridate)
library(ggrepel)
library(patchwork)
library(PropCIs)

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

a.ed_diabetes <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[DBO].[HF_covid_diabetes_activity_ED]'))

a.ip_diabetes <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[DBO].[HF_covid_diabetes_activity]')) %>% 
  mutate(Age_range = case_when(Der_Age_At_CDS_Activity_Date >= 0 & Der_Age_At_CDS_Activity_Date < 20 ~ "0-19",
                               (Der_Age_At_CDS_Activity_Date >= 20 & Der_Age_At_CDS_Activity_Date <40) ~  "20-39",
                               (Der_Age_At_CDS_Activity_Date >= 40 & Der_Age_At_CDS_Activity_Date <60) ~  "40-59",
                               (Der_Age_At_CDS_Activity_Date >= 60 & Der_Age_At_CDS_Activity_Date <80) ~  "60-79",
                               (Der_Age_At_CDS_Activity_Date >= 80 & Der_Age_At_CDS_Activity_Date <100) ~ "80-99",
                               Der_Age_At_CDS_Activity_Date >= 100 ~ "100+")) %>% 
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

a.diabetes_ip_icd10 <- c('E10', 'E11', 'E12', 'E13', 'E14')

a.diabetes_ip_planned_index <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[DBO].[HF_covid_diabetes_activity_planned_index]')) %>% 
  mutate(Age_range = case_when(Der_Age_At_CDS_Activity_Date >= 0 & Der_Age_At_CDS_Activity_Date < 20 ~ "0-19",
                               (Der_Age_At_CDS_Activity_Date >= 20 & Der_Age_At_CDS_Activity_Date <40) ~  "20-39",
                               (Der_Age_At_CDS_Activity_Date >= 40 & Der_Age_At_CDS_Activity_Date <60) ~  "40-59",
                               (Der_Age_At_CDS_Activity_Date >= 60 & Der_Age_At_CDS_Activity_Date <80) ~  "60-79",
                               (Der_Age_At_CDS_Activity_Date >= 80 & Der_Age_At_CDS_Activity_Date <100) ~ "80-99",
                               Der_Age_At_CDS_Activity_Date >= 100 ~ "100+")) %>% 
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


a.diabetes_ip_unplanned_index <-
  as_tibble(
    dbGetQuery(a.con, '
select *
from [NHSE_Sandbox_Spec_Neurology].[DBO].[HF_covid_diabetes_activity_unplanned_index]')) %>% 
  mutate(Age_range = case_when(Der_Age_At_CDS_Activity_Date >= 0 & Der_Age_At_CDS_Activity_Date < 20 ~ "0-19",
                               (Der_Age_At_CDS_Activity_Date >= 20 & Der_Age_At_CDS_Activity_Date <40) ~  "20-39",
                               (Der_Age_At_CDS_Activity_Date >= 40 & Der_Age_At_CDS_Activity_Date <60) ~  "40-59",
                               (Der_Age_At_CDS_Activity_Date >= 60 & Der_Age_At_CDS_Activity_Date <80) ~  "60-79",
                               (Der_Age_At_CDS_Activity_Date >= 80 & Der_Age_At_CDS_Activity_Date <100) ~ "80-99",
                               Der_Age_At_CDS_Activity_Date >= 100 ~ "100+")) %>% 
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

# Check proportion of records with secondary snomed code input (78%)
a.ed_diabetes %>% 
  mutate(secondary_snomed = case_when(is.na(EC_Diagnosis_02) ~ "NA",
                                      !is.na(EC_Diagnosis_02) ~ "valid")) %>% 
  group_by(secondary_snomed) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)*100)

a.ed_diabetes %>% 
  select(EC_Diagnosis_02)

## B. High level summary ####
# Diabetes ED attendances - daily
b_diabetes_ed_daily <-
  a.ed_diabetes %>% 
  group_by(substr(Arrival_Date, 1, 10)) %>% 
  summarise(n_attendances = n_distinct(EC_Ident)) %>%  #sum monthly attendances
  rename(Date = 1)

# Diabetes ED attendances - daily 
b_diabetes_ed_daily_vis <-
  b_diabetes_ed_daily %>% 
  filter(Date > "2018-01-01" & 
           Date < "2020-12-31") %>%
  mutate(week_day = weekdays(as.Date(Date))) %>%
  mutate(weekend_flag = case_when(substr(week_day,1,1) == "S" ~ "Weekend",
                                  TRUE ~ "Week day")) %>% 
  
  ggplot(aes(x = as.Date(Date), y = n_attendances)) +
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.8) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 100, label = "National lockdown",size = 5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 100, label = "3 Tier lockdown",size = 5, angle = 90) +
  #geom_point(aes(colour = weekend_flag), size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15) +
  labs(title = "Emergency attendances for diabetes were temporarily disrupted by covid-19 but recovered quickly",
       subtitle = "Emergency activity: Diabetes related attendances: National",
       x = "Date",
       y = "Attendances")

## C.2. Deprivation #### 
con_ref <- dbConnect(odbc(), 
                     Driver = "SQL Server", 
                     server = "PRODNHSESQL101", 
                     Database = "NHSE_Reference")

c_lsoa_imd <- tbl(con_ref, "tbl_Ref_Other_Deprivation_By_LSOA") %>% 
  collect()

# Disconnect! # 
dbDisconnect(con_ref) 

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

## G. Diabetes attendances - Index ####
g_diabetes_snomed <-
  tribble(
    ~ID, ~Description, ~Chapter, ~diabetes_type,
    
    237620003,'Abnormal metabolic state due to diabetes mellitus','Abnormal metabolic state due to diabetes mellitus','Not stated',
    237621004,'Severe hyperglycemia due to diabetes mellitus','Hyperglycemia','Not stated',
    398140007,'Somogyi phenomenon','Hyperglycemia','Not stated',
    822995009,'Hyperglycemia due to diabetes mellitus','Hyperglycemia','Not stated',
    367991000119101,'Hyperglycemia due to type 1 diabetes mellitus','Hyperglycemia','Type 1',
    368051000119109,'Hyperglycemia due to type 2 diabetes mellitus','Hyperglycemia','Type 2',
    441656006,'Hyperglycemic crisis due to diabetes mellitus','Hyperglycemia','Not stated',
    395204000,'Hyperosmolar non-ketotic state due to type 2 diabetes mellitus','Hyperglycemia','Type 2',
    735539005,'Metabolic acidosis due to diabetes mellitus','Diabetic ketoacidosis','Not stated',
    420422005,'Diabetic ketoacidosis','Diabetic ketoacidosis','Not stated',
    721283000,'Acidosis due to type 1 diabetes mellitus','Diabetic ketoacidosis','Type 1',
    721284006,'Acidosis due to type 2 diabetes mellitus','Diabetic ketoacidosis','Type 2',
    735538002,'Lactic acidosis due to diabetes mellitus','Diabetic ketoacidosis','Not stated',
    111556005,'Diabetic ketoacidosis without coma','Diabetic ketoacidosis','Not stated',
    26298008, 'Ketoacidotic coma due to diabetes mellitus','Diabetic ketoacidosis','Not stated',
    190406000,'Malnutrition-related diabetes mellitus with ketoacidosis','Diabetic ketoacidosis','Not stated',
    420270002,'Ketoacidosis due to type 1 diabetes mellitus','Diabetic ketoacidosis','Type 1',
    421750000,'Ketoacidosis due to type 2 diabetes mellitus','Diabetic ketoacidosis','Type 2',
    421075007,'Ketoacidotic coma due to type 1 diabetes mellitus','Diabetic ketoacidosis','Type 1',
    421847006,'Ketoacidotic coma due to type 2 diabetes mellitus','Diabetic ketoacidosis','Type 2',
    1571000119104,'Mixed hyperlipidemia due to type 1 diabetes mellitus','Hyperlidemia','Type 1',
    701000119103,'Mixed hyperlipidemia due to type 2 diabetes mellitus','Hyperlidemia','Type 2',
    137941000119106,'Hyperlipidemia due to type 1 diabetes mellitus','Hyperlidemia','Type 1',
    137931000119102,'Hyperlipidemia due to type 2 diabetes mellitus','Hyperlidemia','Type 2',
    367261000119100,'Hyperosmolarity due to drug induced diabetes mellitus','Hyperosmolarity','Not stated',
    422126006,'Hyperosmolar coma due to diabetes mellitus','Hyperosmolarity','Not stated',
    368561000119102,'Hyperosmolarity due to type 1 diabetes mellitus','Hyperosmolarity','Type 1',
    428896009,'Hyperosmolality due to uncontrolled type 1 diabetes mellitus','Hyperosmolarity','Type 1',
    190330002,'Hyperosmolar coma due to type 1 diabetes mellitus','Hyperosmolarity','Type 1',
    190331003,'Hyperosmolar coma due to type 2 diabetes mellitus','Hyperosmolarity','Type 2',
    368601000119102,'Hyperosmolar coma due to secondary diabetes mellitus','Hyperosmolarity','Not stated',
    735537007,'Hyperosmolar hyperglycemic coma due to diabetes mellitus without ketoacidosis','Hyperosmolarity','Not stated',
    368551000119104,'Dyslipidemia due to type 1 diabetes mellitus','Dyslipidemia','Type 1',
    761000119102,'Dyslipidemia due to type 2 diabetes mellitus','Dyslipidemia','Type 2',
    111231000119109,'Dyslipidemia with high density lipoprotein below reference range and triglyceride above reference range due to type 2 diabetes mellitus','Dyslipidemia','Type 2',
    420662003,'Coma due to diabetes mellitus','Coma','Not stated',
    421966007,'Non-ketotic non-hyperosmolar coma due to diabetes mellitus','Coma','Not stated',
    762489000,'Acute complication due to diabetes mellitus','General','Not stated',
    193183000,'Acute painful diabetic neuropathy','General','Not stated',
    302866003,'Hypoglycemia','Hypoglycemia','Not stated',
    237630007,'Hypoglycemic disorder','Hypoglycemia','Not stated',
    237635002,'Nocturnal hypoglycemia due to diabetes mellitus','Hypoglycemia','Not stated',
    360546002,'Hypoglycemic shock','Hypoglycemia','Not stated',
    66095000,'Mixed hypoglycemia','Hypoglycemia','Not stated',
    20825002,'Ketotic hypoglycemia','Hypoglycemia','Not stated',
    68581004,'Hypoglycemia of childhood','Hypoglycemia','Not stated',
    237631006,'Neuroglycopenia','Hypoglycemia','Not stated',
    237633009,'Hypoglycemia due to diabetes mellitus','Hypoglycemia','Type 1',
    237632004,'Hypoglycemic event due to diabetes','Hypoglycemia','Not stated',
    84371000119108,'Hypoglycemia due to type 1 diabetes mellitus','Hypoglycemia','Type 1',
    421437000,'Hypoglycemic coma due to type 1 diabetes mellitus','Hypoglycemia','Type 1',
    120711000119108,'Hypoglycemic unawareness due to type 1 diabetes mellitus','Hypoglycemia','Type 1',
    421725003,'Hypoglycemic coma due to diabetes mellitus','Hypoglycemia','Not stated',
    719216001,'Hypoglycemic coma due to type 2 diabetes mellitus','Hypoglycemia','Type 2',
    170766006,'Loss of hypoglycemic warning due to diabetes mellitus','Hypoglycemia','Not stated',
    119831000119106,'Hypoglycemia unawareness due to type 2 diabetes mellitus','Hypoglycemia','Type 2',
    80394007,'Hyperglycemia (disorder)','Hyperglycemia','Not stated',
    46635009,'Diabetes mellitus type 1 (disorder)','General','Type 1',
    44054006,'Diabetes mellitus type 2 (disorder)','General','Type 2',
    14140009,'Hyperkalemia (disorder)','Hyperkalemia','Not stated',
  )

# Check all diagnoses to assign each patient to either type 1 or type 2
g_type_1_codes <- c(
  '367991000119101',
  '721283000',
  '420270002',
  '421075007',
  '1571000119104',
  '137941000119106',
  '368561000119102',
  '428896009',
  '190330002',
  '368551000119104',
  '237633009',
  '84371000119108',
  '421437000',
  '120711000119108',
  '46635009',
  '46635009'
)

g_type_2_codes <- c(
  '368051000119109',
  '395204000',
  '721284006',
  '421750000',
  '421847006',
  '701000119103',
  '137931000119102',
  '190331003',
  '761000119102',
  '111231000119109',
  '719216001',
  '119831000119106',
  '44054006'
)

# Derive diagnosis and diabetes type for each record
a.ed_diabetes <-
  a.ed_diabetes %>%  
  left_join(g_diabetes_snomed %>% 
              mutate(ID = as.character(ID)), 
            by = c("EC_Diagnosis_01" = "ID")) %>% 
  mutate(diabetes_type_flag = case_when(
    str_detect(Der_EC_Diagnosis_All, paste(g_type_1_codes, collapse = "|")) ~ "Type_1",
    str_detect(Der_EC_Diagnosis_All, paste(g_type_2_codes, collapse = "|")) ~ "Type_2",
    TRUE ~ "Unknown")) 

# Check totals
a.ed_diabetes %>% 
  group_by(diabetes_type_flag) %>% summarise(n= n())

# # A tibble: 3 x 2
# diabetes_type_flag      n
# <chr>               <int>
# 1 Type_1              28406
# 2 Type_2              34317
# 3 Unknown            182427

a.ed_diabetes %>% 
  select(Der_AEA_Diagnosis_All) %>% 
  group_by(Der_AEA_Diagnosis_All) %>% summarise(n= n())

# AEA diagnosis isn't granular enough 
# AEA_Diagnosis	AEA_Diagnosis_Desc	
# 301	301: Diabetes and other endocrinological conditions - Diabetic	
# 302	302: Diabetes and other endocrinological conditions - Other non-diabetic	

# Check whether a record has both type 1 and type 2 flag
a.ed_diabetes %>% 
  select(Der_EC_Diagnosis_All) %>% 
  mutate(diabetes_type1_flag = case_when(
    str_detect(Der_EC_Diagnosis_All, paste(g_type_1_codes, collapse = "|")) ~ "Type_1",
    TRUE ~ "NA")) %>% 
  mutate(diabetes_type2_flag = case_when(
    str_detect(Der_EC_Diagnosis_All, paste(g_type_2_codes, collapse = "|")) ~ "Type_2",
    TRUE ~ "NA",
  )) %>% 
  filter(diabetes_type1_flag == "Type_1" &
           diabetes_type2_flag == "Type_2") # 108 records

# Patient index
g_diabetes_demographic_index <-
  a.ed_diabetes %>% 
  select(
    EC_Ident,
    Der_Pseudo_NHS_Number ,
    Der_Postcode_LSOA_2011_Code,
    Age_At_Arrival,
    Der_EC_Arrival_Date_Time,
    Index_Of_Multiple_Deprivation_Decile_Description,
    Rural_Urban_Indicator,
    Sex,
    Ethnic_Category,
    EC_Diagnosis_01,
    EC_Decision_To_Admit_Date,
    Der_EC_Duration,
    Provider_Code,
    diabetes_type_flag,
    Description,
    Chapter
  ) %>%  
  # Severity
  mutate(Severity = 
           case_when(Der_EC_Duration <= 240 & is.na(EC_Decision_To_Admit_Date) ~ "<4hrs then Home", # 4 hrs then home
                     Der_EC_Duration > 240 & Der_EC_Duration <= 720 & 
                       is.na(EC_Decision_To_Admit_Date) ~ "4-12hrs then Home", # 4-12hrs then home
                     Der_EC_Duration <= 720 & is.na(EC_Decision_To_Admit_Date) ~ "12+hrs then Home",
                     !is.na(EC_Decision_To_Admit_Date) ~ "Admitted ", 
                     TRUE ~ "NA" )) %>%
  # Ethnicity
  mutate(Der_EC_Arrival_Date_Time = as.Date(Der_EC_Arrival_Date_Time)) %>% 
  mutate(
    Ethnic_Category = case_when(Ethnic_Category %in% c("99", "NA") ~ Ethnic_Category,
                                TRUE ~ substr(Ethnic_Category,1,1)
    )) %>% # Shorten Ethnic group to first character to clean data (except when coded as 99 or NA)
  mutate(
    Ethnic_Category = case_when(is.na(Ethnic_Category) ~ "99",
                                TRUE ~ Ethnic_Category)) %>% # Replace NA's with "NA" to left_join to "Not Known"
  left_join(c_ethnicity_lookup, by = c("Ethnic_Category")) %>% 
  # Age range 
  mutate(Age_range = case_when(Age_At_Arrival >= 0 & Age_At_Arrival < 20 ~ "0-19",
                               (Age_At_Arrival >= 20 & Age_At_Arrival <40) ~  "20-39",
                               (Age_At_Arrival >= 40 & Age_At_Arrival <60) ~  "40-59",
                               (Age_At_Arrival >= 60 & Age_At_Arrival <80) ~  "60-79",
                               (Age_At_Arrival >= 80 & Age_At_Arrival <100) ~ "80-99",
                               Age_At_Arrival>= 100 ~ "100+")) %>% 
  mutate(Age_range = factor(Age_range, levels = c("0-19", 
                                                  "20-39",
                                                  "40-59",
                                                  "60-79",
                                                  "80-99", 
                                                  "100+"))) %>% 
  # Geography
  left_join(a_lsoa_to_stp, by = c("Der_Postcode_LSOA_2011_Code" = "lsoa11cd")) %>% 
  left_join(a_stp_to_nhsr %>% 
              select(stp20cd, nhser20cd, nhser20nm) %>% 
              distinct(), 
            by = c("stp20cd")) %>% 
  # Group and summarise
  group_by(Der_EC_Arrival_Date_Time, 
           nhser20nm, 
           Age_range, 
           Sex,
           Ethnic_Category_Desc, 
           Ethnicity_broad, 
           Index_Of_Multiple_Deprivation_Decile_Description,
           Severity,
           EC_Diagnosis_01,
           EC_Decision_To_Admit_Date,
           Der_EC_Duration,
           diabetes_type_flag,
           Description,
           Chapter
  ) %>% 
  summarise(Admission_count = n_distinct(EC_Ident)) %>% 
  ungroup() %>% 
  rename(Arrival_Date = Der_EC_Arrival_Date_Time, 
         IMD_Decile = Index_Of_Multiple_Deprivation_Decile_Description) %>% 
  mutate(IMD_Quintile = case_when(
    IMD_Decile %in% c("Most deprived 10%","More deprived 10-20%") ~ 1,
    IMD_Decile %in% c("More deprived 20-30%", "More deprived 30-40%") ~ 2,
    IMD_Decile %in% c("More deprived 40-50%","Less deprived 40-50%%") ~ 3,
    IMD_Decile %in% c("Less deprived 30-40%","Less deprived 20-30%") ~ 4,
    IMD_Decile %in% c("Less deprived 10-20%","Least deprived 10%%") ~ 5,
  ))


## G.1.1. Compare diabetes ED attendances and emergency IP admissions ####
a.diabetes_ip_unplanned_index %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_week) %>% 
  summarise(IP_Unplanned = sum(Admission_count)) %>% 
  filter(substr(Admission_week,1,4) < 2021) %>% 
  left_join(
    a.diabetes_ip_planned_index %>% 
      mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
      group_by(Admission_week) %>% 
      summarise(IP_Planned = sum(Admission_count)) %>% 
      filter(substr(Admission_week,1,4) < 2021),
    by = c("Admission_week")
  ) %>% 
  left_join(
    g_diabetes_demographic_index %>%
      mutate(Admission_week = floor_date(as.Date(Arrival_Date), unit = "week")) %>%
      group_by(Admission_week) %>% 
      summarise(ED_Attendances = sum(Admission_count)) %>% 
      filter(substr(Admission_week,1,4) < 2021),
    by = c("Admission_week")
  ) %>% 
  pivot_longer(cols = -Admission_week, 
               names_to = "Admissions", 
               values_to = "Count") %>% 
  
  ggplot(aes(x = Admission_week, y = Count, colour = Admissions)) + 
  geom_smooth(method = 'loess', span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Week", 
       y = "Admissions/attendances",
       title = "Sharp increase in ED activity is likely fueled by increased data completeness quality",
       subtitle = "Weekly activity for Diabetes - Type 1 and 2", 
       caption = "Emergency department (ED) attendances and Inpatient (planned and unplanned) admissions, National 2018-20")


## G.1.2. Subset of providers that have more complete ED data? ####

e.con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   server = "PRODNHSESQL101", 
                   Database = "NHSE_Reference")

e.provider_site_ref <-
  as_tibble(
    dbGetQuery(e.con, '
select *
from [NHSE_Reference].[dbo].[tbl_Ref_ODS_ProviderSite]'))

# Disconnect! # 
dbDisconnect(e.con) 

g_provider_subset <- c(
  "R1F",  "RA7",  "RBD",  "RBN",  "RC9",  "RCD",  "RCU",  "RCX",  "RD3",  "RDE",  "RDZ",  "REF",
  "RFF",  "RFS",  "RGP",  "RHM",  "RHQ",  "RHU",  "RJ1",  "RJL",  "RK5",  "RK9",  "RL4",  "RLQ",
  "RM1",  "RM3",  "RN3",  "RNQ",  "RQ3",  "RR7",  "RRK",  "RTR",  "RVJ",  "RVR",  "RVW",  "RVY",
  "RW6",  "RWA",  "RWD",  "RWE",  "RX1",  "RXF",  "RXP",  "RXW",  "RYR")

# Display diabetes ED attendances by Provider (subset)
g_diabetes_demographic_index_subset <-
  g_diabetes_demographic_index %>%
  filter(Provider_Code %in% g_provider_subset)

g_diabetes_demographic_index_subset %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>%
  group_by(Admission_week, Provider_Code) %>% 
  summarise(ED_Attendances = sum(Admission_count)) %>%  
  left_join(select(e.provider_site_ref, Provider_Code, Provider_Site_Code, Provider_Name_Short),
            by = c("Provider_Code")) %>% 
  
  ggplot(aes(x = Admission_week, y = ED_Attendances)) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~Provider_Name_Short)


# Recreate for reduced dataset 

# Calculate weekly diabetes attendances
g_diabetes_week_number_subset <-
  g_diabetes_demographic_index_subset %>%
  group_by(Arrival_Date) %>% 
  summarise(n_attendances = sum(Admission_count)) %>% 
  mutate(year = year(Arrival_Date),
         month = months(Arrival_Date), 
         week_number = isoweek(Arrival_Date)) %>% 
  group_by(year, week_number) %>% 
  summarise(weekly_admissions = sum(n_attendances)) %>% 
  ungroup()

g_diabetes_week_number_wide_subset <-
  g_diabetes_week_number_subset %>% 
  filter(year == 2020) %>% 
  rename(n_2020 = weekly_admissions) %>% 
  select(-year) %>% 
  left_join(g_diabetes_week_number_subset %>% 
              filter(year == 2018) %>% 
              select(-year) %>% 
              rename(n_2018 = weekly_admissions), by = c("week_number"), keep = FALSE) %>% 
  left_join(g_diabetes_week_number_subset %>% 
              filter(year == 2019) %>% 
              select(-year) %>% 
              rename(n_2019 = weekly_admissions), by = c("week_number"), keep = FALSE) %>% 
  group_by(week_number) %>% 
  mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
  ungroup() %>% 
  mutate(def = Avg1819 - n_2020)

g_diabetes_annual_trends_subset <-
  g_diabetes_week_number_wide_subset %>%   
  ggplot(aes(x = week_number, y = n_2020)) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  geom_smooth(aes(y = Avg1819), method = "loess", span = 0.3, colour = "#5881c1") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 700, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 700, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 700, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  # Labels and arrows 
  #annotate(geom = "curve", x = 34, y = 1100, xend = 37, yend = 1300, 
  #         curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  #annotate(geom = "curve", x = 20, y = 1600, xend = 24, yend = 1500, 
  #         curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  #annotate("text", x = 31, y = 1100, label = str_wrap("2018-19 average",10), size = 3) +
  #annotate("text", x = 18, y = 1600, label = "2020", size = 3) +
  labs(x = "Week number", y = "Attendances")

# Visualise admission deficit 
g_diabetes_admission_def_vis_subset <-
  g_diabetes_week_number_wide_subset %>% 
  ggplot(aes(x = week_number, y = def)) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 200, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 200, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 200, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  # Labels and arrows 
  #annotate(geom = "curve", x = 33, y = 30, xend = 34, yend = -100, 
  #         curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  #annotate("text", x = 31, y = 30, label = "2020", size = 3) +
  labs(x = "Week number", y = "Admission deficit")

# Patch
g_diabetes_annual_trends_subset + g_diabetes_admission_def_vis_subset + 
  plot_annotation(title = "Yearly patterns in diabetes emergency attendances",
                  subtitle = "National, Weekly elective inpatient procedures, 2018-20")

# Visualise total attendances in subset 
(g_diabetes_demographic_index %>% 
    group_by(Arrival_Date) %>% 
    summarise(n_attendances = sum(Admission_count)) %>% 
    filter(Arrival_Date < "2021-01-01") %>% 
    #filter(Arrival_Date > "2019-01-01") %>% 
    
    ggplot(aes(x = Arrival_Date, y = n_attendances)) + 
    
    
    annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = -Inf, ymax = Inf, 
             fill= "#fed976", alpha = 0.6) + # Lockdown 1
    annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = -Inf, ymax = Inf, 
             fill= "#fed976", alpha = 0.6) + # 3 tier system
    annotate("text", x = as.Date("2020-05-13"), y = 150, label = "National lockdown",size = 3, angle = 90) +
    annotate("text", x = as.Date("2020-11-24"), y = 150, label = "3 Tier lockdown",size = 3, angle = 90) +
    geom_smooth(method = "loess", span = 0.3) +
    labs(title = "All providers",
         subtitle = "National",
         x = "Date", y = "ED Attendances")
) +
  (g_diabetes_demographic_index_subset %>% 
     group_by(Arrival_Date) %>% 
     summarise(n_attendances = sum(Admission_count)) %>%  
     filter(Arrival_Date < "2021-01-01") %>% 
     #filter(Arrival_Date > "2019-01-01") %>% 
     
     ggplot(aes(x = Arrival_Date, y = n_attendances)) + 
     annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = -Inf, ymax = Inf, 
              fill= "#fed976", alpha = 0.6) + # Lockdown 1
     annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = -Inf, ymax = Inf, 
              fill= "#fed976", alpha = 0.6) + # 3 tier system
     annotate("text", x = as.Date("2020-05-13"), y = 60, label = "National lockdown",size = 3, angle = 90) +
     annotate("text", x = as.Date("2020-11-24"), y = 60, label = "3 Tier lockdown",size = 3, angle = 90) +
     geom_smooth(method = "loess", span = 0.3) +
     labs(title = "Provider subset",
          subtitle = "45 selected provided",
          x = "Date", y = "ED Attendances")
  ) +
  plot_annotation(title = "Diabetes emergency attendances",
                  theme = theme(plot.title = element_text(size = 16)))



## G.2. Diabetes attendances - National and regional deficit ####
# Calculate weekly diabetes attendances
g_diabetes_week_number <-
  g_diabetes_demographic_index %>%
  group_by(Arrival_Date) %>% 
  summarise(n_attendances = sum(Admission_count)) %>% 
  mutate(year = year(Arrival_Date),
         month = months(Arrival_Date), 
         week_number = isoweek(Arrival_Date)) %>% 
  group_by(year, week_number) %>% 
  summarise(weekly_admissions = sum(n_attendances)) %>% 
  ungroup()

g_diabetes_week_number_wide <-
  g_diabetes_week_number %>% 
  filter(year == 2020) %>% 
  rename(n_2020 = weekly_admissions) %>% 
  select(-year) %>% 
  left_join(g_diabetes_week_number %>% 
              filter(year == 2018) %>% 
              select(-year) %>% 
              rename(n_2018 = weekly_admissions), by = c("week_number"), keep = FALSE) %>% 
  left_join(g_diabetes_week_number %>% 
              filter(year == 2019) %>% 
              select(-year) %>% 
              rename(n_2019 = weekly_admissions), by = c("week_number"), keep = FALSE) %>% 
  group_by(week_number) %>% 
  mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
  ungroup() %>% 
  mutate(def = n_2019 - n_2020) %>%  # Switch def from 2020-2018/19 to 2020 - 2019
  mutate(PC_def = case_when(
    week_number < 13 ~ as.numeric(0),
    week_number >= 13 ~ as.numeric(n_2019 - n_2020))) %>%  # Additional variable to start deficit from lockdown start
  ungroup()

# Plot year-on-year ED trend
g_diabetes_week_number_wide %>% 
  pivot_longer(cols = -week_number,
               names_to = "trend",
               values_to = "count") %>% 
  filter(trend != c("def", "PC_def")) %>% 
  
  ggplot(aes(x = week_number, y = count, colour = trend)) +
  geom_smooth(method = "loess", span = 0.3) + 
  scale_colour_manual(values = c("#f9bf07", "#686f73", "#5881c1", "#ec6555")) +
  labs(title = "Year on year ED attendances for diabetes", 
       subtitle = "National", 
       x = "Week", y = "Attendance count")

g_diabetes_annual_trends <-
  g_diabetes_week_number_wide %>%   
  ggplot(aes(x = week_number, y = n_2020)) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  geom_smooth(aes(y = n_2019), method = "loess", span = 0.3, colour = "#5881c1") +
  #geom_smooth(aes(y = n_2019), method = "loess", span = 0.3, colour = "red") +
  #geom_smooth(aes(y = n_2018), method = "loess", span = 0.3, colour = "green") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 1800, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 1800, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 1800, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  # Labels and arrows 
  annotate(geom = "curve", x = 29, y = 1100, xend = 20, yend = 1200, 
           curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "curve", x = 20, y = 1700, xend = 24, yend = 1605, 
           curvature = -0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 31, y = 1100, label = str_wrap("2020",10), size = 3) +
  annotate("text", x = 18, y = 1700, label = "2019", size = 3) +
  labs(x = "Week number", y = "Attendances")

# Visualise admission deficit 
g_diabetes_admission_def_vis <-
  g_diabetes_week_number_wide %>% 
  ggplot(aes(x = week_number, y = PC_def)) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 600, label = str_wrap("National lockdown", 10) ,size = 3.5) +
  annotate("text", x = 34, y = 600, label = str_wrap("Recovery period", 10) ,size = 3.5) +
  annotate("text", x = 48, y = 600, label = str_wrap("3 Tier lockdown", 10) ,size = 3.5) +
  labs(x = "Week number", y = "Admission deficit")

# Patch
g_diabetes_annual_trends + g_diabetes_admission_def_vis + 
  plot_annotation(title = "Yearly patterns in diabetes emergency attendances",
                  subtitle = "National, Weekly elective inpatient procedures, 2018-20")

# Function: get admission count and deficit by demographic 
g_get_admission_deficit_diabetes <- function(demographic_var) {
  
  g_diabetes_demographic_index <- 
    g_diabetes_demographic_index %>%
    mutate(demographic_var = {{demographic_var}})
  
  sub_data <-
    g_diabetes_demographic_index %>% 
    mutate(year = year(Arrival_Date),
           month = months(Arrival_Date), 
           week_number = isoweek(Arrival_Date)) %>% 
    group_by(year, week_number, demographic_var) %>% 
    summarise(weekly_attendances = sum(Admission_count)) %>% 
    ungroup()
  
  sub_data_wide <-
    sub_data %>% 
    filter(year == 2020) %>% 
    rename(n_2020 = weekly_attendances) %>% 
    select(-year) %>% 
    left_join(sub_data %>% 
                filter(year == 2018) %>% 
                select(-year) %>% 
                rename(n_2018 = weekly_attendances), by = c("week_number", 'demographic_var'), keep = FALSE) %>% 
    left_join(sub_data %>% 
                filter(year == 2019) %>% 
                select(-year) %>% 
                rename(n_2019 = weekly_attendances), by = c("week_number", 'demographic_var'), keep = FALSE) %>% 
    group_by(week_number, demographic_var) %>% 
    mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(def = n_2019 - n_2020) %>% 
    mutate(PC_def = case_when(
      week_number < 13 ~ as.numeric(0),
      week_number >= 13 ~ as.numeric(n_2019 - n_2020))) %>%  # Additional variable to start deficit from lockdown start
    ungroup()
  
  sub_data_wide
}

# Function: Plot 2020 vs 2018/19 average, facet by demographic
facet_plot_function_diabetes <- function(data) {
  
  data %>% 
    pivot_longer(cols = c(-week_number, - demographic_var),
                 names_to = "trend", 
                 values_to = "Attendance_count") %>% 
    filter(trend %in% c("n_2020", "n_2019")) %>% 
    mutate(trend = case_when(trend == "n_2020" ~ "2020",
                             trend == "n_2019" ~ "2019"))  %>% 
    
    ggplot(aes(x = week_number, y = Attendance_count, colour = trend, group = trend)) +
    geom_smooth(method = "loess", span = 0.3) +
    scale_colour_manual(values = c("#5881c1", "#f9bf07")) +
    facet_wrap(~ demographic_var, scales = "free") +
    # Covid timeline markers
    annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
    annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
    annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
    theme(strip.text = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 12)) +
    labs(colour = "")
}

# Region (not using the function above)
g_get_admission_deficit_diabetes(nhser20nm) %>% 
  drop_na(demographic_var) %>% 
  pivot_longer(cols = c(-week_number, - demographic_var),
               names_to = "trend", 
               values_to = "Attendance_count") %>% 
  filter(trend %in% c("n_2020", "n_2019")) %>% 
  mutate(trend = case_when(trend == "n_2020" ~ "2020",
                           trend == "n_2019" ~ "2019"))  %>% 
  
  ggplot(aes(x = week_number, y = Attendance_count, colour = trend, group = trend)) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_colour_manual(values = c("#5881c1", "#f9bf07")) +
  facet_wrap(~ demographic_var) +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 375, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 375, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 375, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.2)) +
  labs(x = "Week number", y = "Admissions", colour = "",
       title = "Most variation from the previous year's norm occurs during lockdown 1",
       subtitle = "ED attendances for diabetes in 2019 and 2020 by NHSE region")

# Regional deficit 
g_get_admission_deficit_diabetes(nhser20nm) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  
  ggplot(aes(x = week_number, y = cum_def)) +
  geom_hline(yintercept = 0, colour = "#636363") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 1000, label = str_wrap("National lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 1000, label = str_wrap("Recovery period", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 1000, label = str_wrap("3 Tier lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  facet_wrap(~demographic_var) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12)) +
  labs(x = "Week number",
       y = "Cumulative deficit",
       title = "North East and Yorkshire displays a concerning and growing attendance deficit ", 
       subtitle = "Cumulative emergency attendance deficit for diabetes complications, National",
       caption = "Deficit = difference between 2020 attendance count and expected ED attendances seen in 2019")


# Regional deficit proportion 

g_diab_ed_deficit_prop_region <-
  g_get_admission_deficit_diabetes(nhser20nm) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_2019*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  mutate(sum_def_adj = 
           case_when(sum_def >= 0 ~ sum_def,
                     sum_def < 0 ~ -sum_def )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI))  # Return CI's to negative where the def was negative

g_diab_ed_deficit_prop_region %>% 
  ggplot(aes(y = reorder(demographic_var, def_prop), x = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.25, colour = "#636363") +
  #coord_cartesian(xlim=c(20,60)) +
  labs(title = "North East and Yorkshire experienced greater disruption to ED diabetes care than other regions", 
       subtitle = "Diabetes ED attendance deficit due to covid-19 pandemic, by NHSE region",
       caption = "Deficit as a proportion of total expected admissions seen in '2019",
       x = "Admission deficit proportion", 
       y = "NHSE region")



## G.3 Demographic deficits ####

# Broad ethnicity
facet_plot_function_diabetes(g_get_admission_deficit_diabetes(Ethnicity_broad) %>% 
                               drop_na(demographic_var)) +
  labs(x = "Week number", y = "Attendances", colour = "",
       title = "White and Asian populations have the clearest attendance excesses",
       subtitle = "ED diabetes attendances in 2019 and 2020: Attendances by broad ethnicity")

# Broad ethnicity - cumulative deficit 
g_get_admission_deficit_diabetes(Ethnicity_broad) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  
  ggplot(aes(x = week_number, y = cum_def)) +
  geom_hline(yintercept = 0, colour = "#636363") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 0, label = str_wrap("National lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 0, label = str_wrap("Recovery period", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 0, label = str_wrap("3 Tier lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  facet_wrap(~demographic_var) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12)) +
  labs(x = "Week number",
       y = "Cumulative deficit",
       title = "A deficit has emerged and sustained in White and Asain ethnicities due to covid-19", 
       subtitle = "Cumulative emergency attendance deficit for diabetes complications, Deficit by ethnicity, National",
       caption = "Deficit = difference between 2020 attendance count and expected ED attendances seen in 2019")

# Ethnicity - deficit proportion 
g_diab_ed_deficit_prop_broad_ethnicity <-
  g_get_admission_deficit_diabetes(Ethnicity_broad) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_2019*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  mutate(sum_def_adj = 
           case_when(sum_def >= 0 ~ sum_def,
                     sum_def < 0 ~ -sum_def )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI))  # Return CI's to negative where the def was negative

g_diab_ed_deficit_prop_broad_ethnicity %>% 
  ggplot(aes(y = reorder(demographic_var, def_prop), x = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.25, colour = "#636363") +
  labs(title = "The deficit in White and Asian populations is largest in absolute and relative terms", 
       subtitle = "Diabetes ED attendance deficit due to covid-19 pandemic, by broad ethnicity",
       caption = "Deficit as a proportion of total expected admissions seen in '2019",
       x = "Admission deficit proportion", 
       y = "Broad ethnicity")


# Deprivation
facet_plot_function_diabetes(g_get_admission_deficit_diabetes(IMD_Quintile) %>% 
                               drop_na(demographic_var)) +
  labs(x = "Week number", y = "Admissions", colour = "",
       title = "",
       subtitle = "ED diabetes attendances in 2019 and 2020: Attendances by IMD Quintile")

# Deprivation - cumulative deficit 
g_get_admission_deficit_diabetes(IMD_Quintile) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  
  ggplot(aes(x = week_number, y = cum_def)) +
  geom_hline(yintercept = 0, colour = "#636363") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 0, label = str_wrap("National lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 0, label = str_wrap("Recovery period", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 0, label = str_wrap("3 Tier lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  facet_wrap(~demographic_var) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12)) +
  labs(x = "Week number",
       y = "Cumulative deficit",
       title = "Pre-pandemic excess are seen in all quintiles, with poor groups experiencing larger subsequent deficits", 
       subtitle = "Cumulative emergency attendance deficit for diabetes complications, Deficit by deprivation, National",
       caption = "Deficit = difference between 2020 attendance count and expected ED attendances seen in 2019")


# Deprivation - deficit proportion 
g_diab_ed_deficit_prop_broad_deprivation <-
  g_get_admission_deficit_diabetes(IMD_Quintile) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(PC_def, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_2019*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  mutate(sum_def_adj = 
           case_when(sum_def >= 0 ~ sum_def,
                     sum_def < 0 ~ -sum_def )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI))  # Return CI's to negative where the def was negative

g_diab_ed_deficit_prop_broad_deprivation %>% 
  ggplot(aes(y = reorder(demographic_var, -demographic_var), x = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.25, colour = "#636363") +
  labs(title = "Disruption in emergency diabetes attendances is skewed towards the poorest half of the population", 
       subtitle = "Diabetes ED attendance deficit due to covid-19 pandemic, by deprivation quintile",
       caption = "Deficit as a proportion of total expected admissions seen in '2019",
       x = "Admission deficit proportion", 
       y = "IMD Quintile")

# Age range 
facet_plot_function_diabetes(g_get_admission_deficit_diabetes(Age_range) %>% 
                               drop_na(demographic_var)) +
  labs(x = "Week number", y = "Admissions", colour = "",
       title = "...",
       subtitle = "ED diabetes attendances in 2019 and 2020: Attendances by Age range")

# Age range - cumulative deficit 
g_get_admission_deficit_diabetes(Age_range) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_def)) %>% 
  
  ggplot(aes(x = week_number, y = cum_def)) +
  geom_hline(yintercept = 0, colour = "#636363") +
  # Covid timeline markers
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 0, label = str_wrap("National lockdown", 10) ,size = 2.5) +
  annotate("text", x = 34, y = 0, label = str_wrap("Recovery period", 10) ,size = 2.5) +
  annotate("text", x = 48, y = 0, label = str_wrap("3 Tier lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  facet_wrap(~demographic_var) +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12)) +
  labs(x = "Week number",
       y = "Cumulative deficit",
       title = "...", 
       subtitle = "Cumulative emergency attendance deficit for diabetes complications, Deficit by Age range, National",
       caption = "Deficit = difference between 2020 attendance count and expected ED attendances seen in 2019")


# Age range - deficit proportion 
g_diab_ed_deficit_prop_Age_range <-
  g_get_admission_deficit_diabetes(Age_range) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_def = sum(def, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(def_prop = round(sum_def/sum_2019*100,2)) %>% 
  arrange(desc(def_prop)) %>% 
  mutate(sum_def_adj = 
           case_when(sum_def >= 0 ~ sum_def,
                     sum_def < 0 ~ -sum_def )) %>% # Switch out the negative def to positive to calcuate CI
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_def_adj, n = sum_2019,0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_def >= 0 ~ LCI, sum_def < 0 ~ -LCI),
         UCI = case_when(sum_def >= 0 ~ UCI, sum_def < 0 ~ -UCI))  # Return CI's to negative where the def was negative

g_diab_ed_deficit_prop_Age_range %>% 
  ggplot(aes(y = reorder(demographic_var, demographic_var), x = def_prop)) +
  geom_col(fill = "#f9bf07", colour = "grey") +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.25, colour = "#636363") +
  labs(title = "...", 
       subtitle = "Diabetes ED attendance deficit due to covid-19 pandemic, by Age range",
       caption = "Deficit as a proportion of total expected admissions seen in '2019",
       x = "Admission deficit proportion", 
       y = "Age range")

## G.4 Clinical trends ####
## G.4.1 Diabetes attendances by severity ####
g_diabetes_demographic_index %>% 
  mutate(Attendance_week = floor_date(Arrival_Date, unit = "week")) %>% 
  group_by(Attendance_week, Severity) %>% 
  summarise(n_attendances = sum(Admission_count)) %>%  #sum monthly attendances %>% 
  filter(Severity != "NA") %>% 
  
  ggplot(aes(x = Attendance_week, y = n_attendances, colour = Severity)) + 
  annotate("rect", xmin = as.Date("2020-03-23"), xmax = as.Date("2020-07-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.6) + # Lockdown 1
  annotate("rect", xmin = as.Date("2020-10-14"), xmax = as.Date("2021-01-04"), ymin = 0, ymax = Inf, 
           fill= "#fed976", alpha = 0.6) + # 3 tier system
  annotate("text", x = as.Date("2020-05-13"), y = 800, label = "National lockdown",size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 800, label = "3 Tier lockdown",size = 3, angle = 90) +
  geom_smooth(method = "loess", span = 0.3) +
  labs(x = "Week", 
       y = "Attenances",
       title = "Emergency department attendences for diabetes by attendance severity",
       subtitle = "National, 2018-20")


## G.4.2 Primary diagnosis ####

#g.con <- dbConnect(odbc(), 
#                   Driver = "SQL Server", 
#                   server = "PRODNHSESQL101", 
#                   Database = "NHSE_Reference")
#
#g.chief_complaint_ref <-
#  as_tibble(
#    dbGetQuery(g.con, '
#select *
#from [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint]'))
#
## Disconnect! # 
#dbDisconnect(g.con) 

# # Chief complaint
# g_diabetes_demographic_index %>% 
#   left_join(g.chief_complaint_ref %>% 
#               mutate(ChiefComplaintCode = as.character(ChiefComplaintCode)), 
#             by = c("EC_Chief_Complaint_SNOMED_CT" = "ChiefComplaintCode")) %>% 
#   group_by(ChiefComplaintDescription) %>% 
#   summarise(n = sum(Admission_count)) %>% 
#   arrange(desc(n))

# # EC_Diagnosis_01
# g_diabetes_demographic_index %>% 
#   left_join(g_diabetes_snomed %>% 
#               mutate(ID = as.character(ID)), 
#             by = c("EC_Diagnosis_01" = "ID")) %>% 
#   group_by(Description, diabetes_type) %>% 
#   summarise(n = sum(Admission_count)) %>% 
#   arrange(desc(n))
# 
# # Result: Use EC_Diagnosis_01 as primary diagnosis, Chief_complaint is limitted to 230ish snomed-ct codes - limited diabetes to hypo or hyperglycaemia
# 
# g_diabetes_demographic_index_diag <-
#   g_diabetes_demographic_index %>% 
#   left_join(g_diabetes_snomed %>% 
#               mutate(ID = as.character(ID)), 
#             by = c("EC_Diagnosis_01" = "ID"))
# 
# g_diabetes_demographic_index_diag %>% 
#   group_by(diabetes_type) %>% 
#   summarise(n = sum(Admission_count))


# Diabetes type flag 
g_diabetes_demographic_index %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type_flag) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = diabetes_type_flag)) +
  geom_smooth(method = "loess", span = 0.3)

g_diabetes_demographic_index %>% 
  group_by(substr(Arrival_Date,1,4)) %>% 
  summarise(n = sum(Admission_count))

# Primary diagnosis (Description) 
g_diabetes_demographic_index %>% 
  group_by(Description) %>% summarise(n = n())

# # A tibble: 7 x 2
# Description                             n
# <chr>                               <int>
# 1 Diabetes mellitus type 1 (disorder) 24990
# 2 Diabetes mellitus type 2 (disorder) 28156
# 3 Diabetic ketoacidosis               50900
# 4 Hyperglycemia (disorder)            24360
# 5 Hyperkalemia (disorder)             30620
# 6 Hypoglycemia                        57662
# 7 NA                                  28415

# Changing rate of diabetes admissions by diagnosis 
g_diabetes_demographic_index %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Description) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  drop_na(Description) %>% 
  filter(Admission_week > "2019-01-01") %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = Description)) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y =  "Attendances", 
       title = "Coding of SNOMED-CT ID's is limitted to 6 diabetes diagnoses", 
       subtitle = "Weekly admission count by primary diagnosis, National 2019-20")

## Type 1: Changing rates of attendances  ####
g_type1_weekly_count <- 
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") 

g_type1_weekly_count_vis <-
g_type1_weekly_count %>%
  ggplot(aes(x = Admission_week, y = Activity)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 160, label = str_wrap("Lockdown", 10) ,size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 160, label = str_wrap("Recovery", 10) ,size = 3, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 160, label = str_wrap("Lockdown", 10) ,size = 3, angle = 90) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  labs(x = "Date", y = "Count",
       title = "ED attendances for Type 1 diabetes have not yet returned to pre-pandemic levels",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, National 2019-20")
  
# Break this down by demographic
# Region
g_type1_weekly_count_region <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, nhser20nm) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(nhser20nm) %>% 
  ungroup() 

g_type1_weekly_count_region_vis <-
g_type1_weekly_count_region %>%
  ggplot(aes(x = Admission_week, y = Activity, colour = nhser20nm)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "NHSE Region",
       title = "ED attendances appear to have been reducing prior to lockdown",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, by Region, National 2019-20")

# Age
g_type1_weekly_count_age <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Age_range) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(Age_range) 

g_type1_weekly_count_age_vis <-
g_type1_weekly_count_age %>% 
  ggplot(aes(x = Admission_week, y = Activity, colour = Age_range, group = Age_range)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Age range",
       title = "Attendance rates for older patients were less affected by the pandemic",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, by Age_range, National 2019-20")

# Deprivation 
g_type1_weekly_count_deprivation <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, IMD_Quintile) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(IMD_Quintile) %>% 
  mutate(IMD_Quintile = factor(IMD_Quintile, levels = c(1,2,3,4,5))) 


g_type1_weekly_count_deprivation_vis <-
g_type1_weekly_count_deprivation %>%
  ggplot(aes(x = Admission_week, y = Activity, colour = IMD_Quintile, group = IMD_Quintile)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "IMD Quintile",
       title = "ED attendances are more frequent in deprived patients",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, by IMD Quintile, National 2019-20")

# Broad ethnicity
g_type1_weekly_count_ethnicity_broad <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Ethnicity_broad) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(Ethnicity_broad) 

g_type1_weekly_count_ethnicity_broad_vis <-
g_type1_weekly_count_ethnicity_broad %>%
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad, group = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Broad ethnicity",
       title = "...",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, by broad ethnicity, National 2019-20")

# Broad ethnicity - facet 
g_type1_weekly_count_ethnicity_broad_facet_vis <-
g_type1_weekly_count_ethnicity_broad %>%  
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad, group = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 3) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 3) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 3) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  facet_wrap(~Ethnicity_broad, scales = "free") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 8)) +
  labs(x = "Date", y = "Count", color = "Broad ethnicity",
       title = "Asian, Black and White patients have similar trends in ED attendance",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, by broad ethnicity, National 2019-20",
       caption = "L: Lockdown, R: Recovery")

# Severity
g_type1_weekly_count_severity <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Severity) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" & Admission_week < "2020-12-31") %>% 
  filter(Severity != "NA") 

g_type1_weekly_count_severity_vis <-
g_type1_weekly_count_severity %>%  
  ggplot(aes(x = Admission_week, y = Activity, colour = Severity, group = Severity)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Severity",
       title = "Attendances for type 1 diabetes that are treated in and disharged from ED are increasing post-pandemic",
       subtitle = "Weekly count of ED attenance for Type 1 Diabetes, by attendance severity, National 2019-20")


## Type 1: Proportional change in attendances ####
e_get_attendance_diff_type1 <- function(demographic_var) {
  
  g_diabetes_demographic_index <- 
    g_diabetes_demographic_index %>%
    mutate(demographic_var = {{demographic_var}})
  
  sub_data <-
    g_diabetes_demographic_index %>% 
    filter(Description == "Diabetes mellitus type 1 (disorder)") %>% 
    mutate(year = year(Arrival_Date),
           month = months(Arrival_Date), 
           week_number = isoweek(Arrival_Date)) %>% 
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
    #mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(diff = n_2020 - n_2019) %>% 
    mutate(PC_diff = case_when(
      week_number < 13 ~ as.numeric(0),
      week_number >= 13 ~ as.numeric(n_2020 - n_2019))) %>%  # Additional variable to start deficit from lockdown start
    ungroup() 
  
  sub_data_wide
}

# Region
e_type1_cum_diff_region <-
  e_get_attendance_diff_type1(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  drop_na(demographic_var) 

e_type1_cum_diff_region_vis <-
e_type1_cum_diff_region %>%
  ggplot(aes(x = week_number, y = count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "Reductions in type 1 diabetes attendance continued between lockdowns", 
       subtitle = "Cumulative change in weekly attendance count for type 1 diabetes between 2019 and 2020, by NHSE region",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Region - proportional difference between 2019-2020
e_type1_prop_diff_region <-
  e_get_attendance_diff_type1(nhser20nm) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) # Return CI's to negative where the def was negative

e_type1_prop_diff_region_vis <-
e_type1_prop_diff_region %>% 
  ggplot(aes(y = reorder(demographic_var, diff_prop), x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "NHSE Region",
       title = "The Midlands has the largest absolute and proportional reduction in attendances",
       subtitle = "Proportional change in ED attendance count for type 1 diabetes between 2019-20 by NHSE Region",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Age
e_type1_cum_diff_age_range <-
  e_get_attendance_diff_type1(Age_range) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup()

e_type1_cum_diff_age_range_vis <-
e_type1_cum_diff_age_range %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "Reductions in attendances grew in the under 20's during the second lockdown", 
       subtitle = "Cumulative change in weekly attendance count for type 1 diabetes between 2019 and 2020, by Age range",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Age range - proportional difference between 2019-2020
e_type1_prop_diff_age <-
  e_get_attendance_diff_type1(Age_range) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI))  # Return CI's to negative where the def was negative

e_type1_prop_diff_age_vis <-  
e_type1_prop_diff_age %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Age range",
       title = "Proportional change was similar for all over 20's",
       subtitle = "Proportional change in ED attendance count for type 1 diabetes between 2019-20 by age range",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Deprivation
e_type1_cum_diff_deprivation <-
  e_get_attendance_diff_type1(IMD_Quintile) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() %>% 
  mutate(demographic_var = factor(demographic_var, levels = c(1,2,3,4,5)))

e_type1_cum_diff_deprivation_vis <-
e_type1_cum_diff_deprivation %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "The cumulative reduction in ED attendances increases with deprivation", 
       subtitle = "Cumulative change in weekly attendance count for type 1 diabetes between 2019 and 2020, by IMD Quintile ",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Deprivation - proportional difference between 2019-2020
e_type1_prop_diff_deprivation <-
  e_get_attendance_diff_type1(IMD_Quintile) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) %>%   # Return CI's to negative where the def was negative
  mutate(demographic_var = factor(demographic_var, levels = c(1,2,3,4,5)))

e_type1_prop_diff_deprivation_vis <-
e_type1_prop_diff_deprivation %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "IMD Quintile",
       title = "The reduction in attendances during the pandemic was lowest in the most affluent 20%",
       subtitle = "Proportional change in ED attendance count for type 1 diabetes between 2019-20 by IMD Quintile",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Ethnicity
# Ethnic_Category_Desc
e_type1_cum_diff_ethnicity <-
  e_get_attendance_diff_type1(Ethnicity_broad) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() 

e_type1_cum_diff_ethnicity_vis <-
e_type1_cum_diff_ethnicity %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "...", 
       subtitle = "Cumulative change in weekly attendance count for type 1 diabetes between 2019 and 2020, by broad ethnicity",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Ethnicity - proportional difference between 2019-2020
e_type1_prop_diff_ethnicity <-
  e_get_attendance_diff_type1(Ethnic_Category_Desc) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI))

e_type1_prop_diff_ethnicity_vis <-
e_type1_prop_diff_ethnicity %>% 
  ggplot(aes(y = reorder(demographic_var, diff_prop), x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Ethnicity",
       title = "Proportional changes do not appear to be driven by ethnicity",
       subtitle = "Proportional change in ED attendance count for type 1 diabetes between 2019-20 by ethnicity",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Serverity 
e_type1_cum_diff_severity <-
  e_get_attendance_diff_type1(Severity) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() 

e_type1_cum_diff_severity_vis <-
  e_type1_cum_diff_severity %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "...", 
       subtitle = "Cumulative change in weekly attendance count for type 1 diabetes between 2019 and 2020, by attendance severity",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Severity - proportional difference between 2019-2020
e_type1_prop_diff_severity <-
  e_get_attendance_diff_type1(Severity) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) 

e_type1_prop_diff_severity_vis <-
  e_type1_prop_diff_severity %>% 
  filter(demographic_var != "NA") %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Attendance severity",
       title = "...",
       subtitle = "Proportional change in ED attendance count for type 1 diabetes between 2019-20 by attendance severity",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")


## Type 2: Changing rates of attendances  ####
g_type2_weekly_count <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") 

g_type2_weekly_count_vis <-
g_type2_weekly_count %>%
  ggplot(aes(x = Admission_week, y = Activity)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 180, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 180, label = str_wrap("Recovery", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 180, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  labs(x = "Date", y = "Count",
       title = "ED attendances for Type 2 diabetes have returned to pre-pandemic levels",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, National 2019-20")

# Break this down by demographic
# Region
g_type2_weekly_count_region <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, nhser20nm) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(nhser20nm) %>% 
  ungroup() 

g_type2_weekly_count_region_vis <-
g_type2_weekly_count_region %>% 
  ggplot(aes(x = Admission_week, y = Activity, colour = nhser20nm)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "NHSE Region",
       title = "Attendances in the South East have continued to rise after the first lockdown",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, by Region, National 2019-20")

# Age
g_type2_weekly_count_age <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Age_range) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(Age_range) 

g_type2_weekly_count_age_vis <-
g_type2_weekly_count_age %>%
  filter(Age_range != "100+") %>% 
  ggplot(aes(x = Admission_week, y = Activity, colour = Age_range, group = Age_range)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Age range",
       title = "Attendances in 40-80  year olds were particularly impacted by lockdowns",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, by Age range, National 2019-20")

# Deprivation 
g_type2_weekly_count_deprivation <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, IMD_Quintile) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(IMD_Quintile) %>% 
  mutate(IMD_Quintile = factor(IMD_Quintile, levels = c(1,2,3,4,5))) 

g_type2_weekly_count_deprivation_vis <-
g_type2_weekly_count_deprivation %>% 
  ggplot(aes(x = Admission_week, y = Activity, colour = IMD_Quintile, group = IMD_Quintile)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "IMD Quintile",
       title = "ED attendances were more frequent in deprived groups and more impacted by the lockdowns",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, by IMD Quintile, National 2019-20")

# Broad ethnicity
g_type2_weekly_count_ethnicity_broad <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Ethnicity_broad) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" &
           Admission_week < "2020-12-31") %>% 
  drop_na(Ethnicity_broad) 

g_type2_weekly_count_ethnicity_broad_vis <-
g_type2_weekly_count_ethnicity_broad %>%  
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad, group = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Broad ethnicity",
       title = "...",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, by broad ethnicity, National 2019-20")

# Broad ethnicity - facet 
g_type2_weekly_count_ethnicity_broad_facet <-
  g_type2_weekly_count_ethnicity_broad %>%  
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad, group = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  #annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 3) +
  #annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 3) +
  #annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 3) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  facet_wrap(~Ethnicity_broad, scales = "free") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, size = 8)) +
  labs(x = "Date", y = "Count", color = "Broad ethnicity",
       title = "Asian, Black and White patients have similar trends in ED attendance",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, by broad ethnicity, National 2019-20"
       #caption = "L: Lockdown, R: Recovery"
       )

# Severity
g_type2_weekly_count_severity <-
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, Severity) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week > "2019-01-01" & Admission_week < "2020-12-31") %>% 
  filter(Severity != "NA") 

g_type2_weekly_count_severity_vis <-
g_type2_weekly_count_severity %>%  
  ggplot(aes(x = Admission_week, y = Activity, colour = Severity, group = Severity)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("Recovery", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("Lockdown", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Severity",
       title = "Attendances that are treated in and disharged from ED are increasing post-pandemic",
       subtitle = "Weekly count of ED attenance for Type 2 Diabetes, by attendance severity, National 2019-20")

## Type 2: Proportional change in attendances ####
e_get_attendance_diff_type2 <- function(demographic_var) {
  
  g_diabetes_demographic_index <- 
    g_diabetes_demographic_index %>%
    mutate(demographic_var = {{demographic_var}})
  
  sub_data <-
    g_diabetes_demographic_index %>% 
    filter(Description == "Diabetes mellitus type 2 (disorder)") %>% 
    mutate(year = year(Arrival_Date),
           month = months(Arrival_Date), 
           week_number = isoweek(Arrival_Date)) %>% 
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
    #mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(diff = n_2020 - n_2019) %>% 
    mutate(PC_diff = case_when(
      week_number < 13 ~ as.numeric(0),
      week_number >= 13 ~ as.numeric(n_2020 - n_2019))) %>%  # Additional variable to start deficit from lockdown start
    ungroup() 
  
  sub_data_wide
}

# Region
e_type2_cum_diff_region <-
  e_get_attendance_diff_type2(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  drop_na(demographic_var) 

e_type2_cum_diff_region_vis <-
e_type2_cum_diff_region %>%
  ggplot(aes(x = week_number, y = count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "Sustained reductions in attendances are seen in the Midlands and London", 
       subtitle = "Cumulative change in weekly attendance count for type 2 diabetes between 2019 and 2020, by NHSE region",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Region - proportional difference between 2019-2020
e_type2_prop_diff_region <-
  e_get_attendance_diff_type2(nhser20nm) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) # Return CI's to negative where the def was negative

e_type2_prop_diff_region_vis <-
e_type2_prop_diff_region %>% 
  ggplot(aes(y = reorder(demographic_var, diff_prop), x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "NHSE Region",
       title = "The South of England saw increases in ED attendances",
       subtitle = "Proportional change in ED attendance count for type 2 diabetes between 2019-20 by NHSE Region",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Age
e_type2_cum_diff_age_range <-
  e_get_attendance_diff_type2(Age_range) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup()

e_type2_cum_diff_age_range_vis <-
e_type2_cum_diff_age_range %>%
  filter(demographic_var != "100+") %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "ED attendances for elderly patients were more disrupted by the pandemic than 20-40 year olds", 
       subtitle = "Cumulative change in weekly attendance count for type 2 diabetes between 2019 and 2020, by Age range",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Age range - proportional difference between 2019-2020
e_type2_prop_diff_age <-
  e_get_attendance_diff_type2(Age_range) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI))  # Return CI's to negative where the def was negative

e_type2_prop_diff_age_vis <-
e_type2_prop_diff_age %>% 
  filter(demographic_var != "100+") %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Age range",
       title = "1 in 5 attendances for 80+ year olds were avoided due to the pandemic",
       subtitle = "Proportional change in ED attendance count for type 2 diabetes between 2019-20 by age range",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Deprivation
e_type2_cum_diff_deprivation <-
  e_get_attendance_diff_type2(IMD_Quintile) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() %>% 
  mutate(demographic_var = factor(demographic_var, levels = c(1,2,3,4,5)))

e_type2_cum_diff_deprivation_vis <-
e_type2_cum_diff_deprivation %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "The cumulative reduction in ED attendances increases with deprivation", 
       subtitle = "Cumulative change in weekly attendance count for type 2 diabetes between 2019 and 2020, by IMD Quintile ",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Deprivation - proportional difference between 2019-2020
e_type2_prop_diff_deprivation <-
  e_get_attendance_diff_type2(IMD_Quintile) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) %>%   # Return CI's to negative where the def was negative
  mutate(demographic_var = factor(demographic_var, levels = c(1,2,3,4,5)))

e_type2_prop_diff_deprivation_vis <-
e_type2_prop_diff_deprivation %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "IMD Quintile",
       title = "Change in attendance varied across deprivation quintiles",
       subtitle = "Proportional change in ED attendance count for type 2 diabetes between 2019-20 by IMD Quintile",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Ethnicity
# Ethnic_Category_Desc
e_type2_cum_diff_ethnicity <-
  e_get_attendance_diff_type2(Ethnicity_broad) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() 

e_type2_cum_diff_ethnicity_vis <-
e_type2_cum_diff_ethnicity %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "...", 
       subtitle = "Change in weekly attendance count for type 2 diabetes between 2019 and 2020, by broad ethnicity",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Ethnicity - proportional difference between 2019-2020
e_type2_prop_diff_ethnicity <-
  e_get_attendance_diff_type2(Ethnic_Category_Desc) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI))

e_type2_prop_diff_ethnicity_vis <-
e_type2_prop_diff_ethnicity %>% 
  ggplot(aes(y = reorder(demographic_var, diff_prop), x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Ethnicity",
       title = "Attendances in Indian and Pakistani patients were more disrupted than in White British",
       subtitle = "Proportional change in ED attendance count for type 2 diabetes between 2019-20 by ethnicity",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")

# Severity 
e_type2_cum_diff_severity <-
  e_get_attendance_diff_type2(Severity) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() 

e_type2_cum_diff_severity_vis <-
  e_type2_cum_diff_severity %>%
  filter(demographic_var != "NA") %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "The pandemic has seen a rise in type 2 diabetics discharged from ED in less than 4hrs", 
       subtitle = "Cumulative change in weekly attendance count for type 2 diabetes between 2019 and 2020, by attendance severity",
       caption = "Attendance difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative attendance difference")

# Severity - proportional difference between 2019-2020
e_type2_prop_diff_severity <-
  e_get_attendance_diff_type2(Severity) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) 

e_type2_prop_diff_severity_vis <-
  e_type2_prop_diff_severity %>% 
  filter(demographic_var != "NA") %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Attendance severity",
       title = "Attedances treated in 4-12hrs then discharged from ED have greatly reduced",
       subtitle = "Proportional change in ED attendance count for type 2 diabetes between 2019-20 by attendance severity",
       caption = "Proportional change = change between 2019-20 / total attendance count for 2019")


## Changing rates of DKA ####
# Identification of diabetes type in DKA attendances - Using secondary diagnosis codes in EC record
g_diabetes_demographic_index %>% 
  filter(Description == "Diabetic ketoacidosis") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type_flag) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  #filter(Admission_week > "2019-01-01") %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = diabetes_type_flag)) +
  geom_smooth(method = "loess", span = 0.3)

g_diabetes_demographic_index %>% 
  filter(Description == "Diabetic ketoacidosis") %>% 
  group_by(diabetes_type_flag) %>% 
  summarise(n = n())

# # A tibble: 3 x 2
# diabetes_type_flag     n
# <chr>              <int>
# 1 Type_1               552
# 2 Type_2                74
# 3 Unknown            50274

# Time series for DKA attendances
# Problematic time series for DKA - Unsuitable to compare to previous years
g_DKA_weekly_count <-
  g_diabetes_demographic_index %>% 
  filter(Description == "Diabetic ketoacidosis") %>% 
  mutate(Admission_week = floor_date(Arrival_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week) %>% 
  summarise(Activity = sum(Admission_count)) %>% 
  filter(Admission_week < "2020-12-31") 

g_DKA_weekly_count_vis <-
  g_DKA_weekly_count %>%
  ggplot(aes(x = Admission_week, y = Activity)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 225, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 225, label = str_wrap("Recovery", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 225, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  labs(x = "Date", y = "Count",
       title = "Data quality and variability in ED attendances are problematic",
       subtitle = "Weekly count of ED attenance for DKA, National 2019-20",
       caption = "Note: Type 1 & 2 diabetics included")


## DKA - Inpatient admissions ####
g_diabetes_ICD10 <-
read_csv("Diabetes_ICD10_codes.csv") %>% 
  clean_names() %>% 
  select(full_icd10_code, title)

g_ip_diabetes_diag <-
a.ip_diabetes %>% 
  left_join(g_diabetes_ICD10, by = c("Der_Primary_Diagnosis_Code" = "full_icd10_code")) %>% 
  left_join(select(c_lsoa_imd, LSOA_Code, IMD_Decile), by = c("Der_Postcode_LSOA_2011_Code" = "LSOA_Code")) %>%
  mutate(IMD_Quintile = case_when(IMD_Decile %in% c(1,2) ~ 1,
                                  IMD_Decile %in% c(3,4) ~ 2,
                                  IMD_Decile %in% c(5,6) ~ 3,
                                  IMD_Decile %in% c(7,8) ~ 4,
                                  IMD_Decile %in% c(9,10) ~ 5,
                                  TRUE ~ 99)) %>% 
  mutate(
    Ethnic_Group = case_when(Ethnic_Group %in% c("99", "NA") ~ Ethnic_Group,
                             TRUE ~ substr(Ethnic_Group,1,1)
                             )) %>% # Shorten Ethnic group to first character to clean data (except when coded as 99 or NA)
  mutate(
    Ethnic_Group = case_when(is.na(Ethnic_Group) ~ "99",
                             TRUE ~ Ethnic_Group)) %>% # Replace NA's with "NA" to left_join to "Not Known"
  left_join(c_ethnicity_lookup, by = c("Ethnic_Group" = "Ethnic_Category"))


# DKA in all patients
g_DKA_ip_weekly_count <-
  g_ip_diabetes_diag %>% 
  filter(str_detect(title, "ketoacidosis") & 
           substr(Admission_Method,1,1) == "2") %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  #filter(Admission_week > "2019-01-01") %>%
  filter(Admission_week < "2020-12-31")   

g_DKA_ip_weekly_count_vis <-
  g_DKA_ip_weekly_count %>%
  ggplot(aes(x = Admission_week, y = Activity)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 500, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 500, label = str_wrap("Recovery", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 500, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  geom_point(size = 2, alpha = 0.2, colour = "#2c2825") +
  geom_smooth(method = "loess", span = 0.3, colour = "#f9bf07") +
  labs(x = "Date", y = "Count",
       title = "The seasonal nature in DKA admissions suggest behavioural influences",
       subtitle = "Weekly count of inpatient admissions for diabetic ketoacidosis (DKA), National 2019-20",
       caption = "Note: Type 1 & 2 diabetics included")

# DKA in Type 1 and Type 2 diabetes - separately
g_diabetes_ICD10 %>% 
  filter(str_detect(title, "Insulin-dependent diabetes")) %>% 
  select(full_icd10_code)

g_icd10_type_1 <- c("E10","E100","E101","E102","E103","E104","E105","E106","E107","E108","E109")

g_diabetes_ICD10 %>% 
  filter(str_detect(title, "Non-insulin-dependent diabetes mellitus")) %>% 
  select(full_icd10_code)

g_icd10_type_2 <- c("E11","E110","E111","E112","E113","E114","E115","E116","E117","E118","E119")

# Derive diabetes type 
g_ip_diabetes_diag_DKA <-
  g_ip_diabetes_diag %>% 
  filter(str_detect(title, "ketoacidosis") & 
           substr(Admission_Method,1,1) == "2") %>% 
  mutate(diabetes_type = case_when(
    str_detect(Der_Diagnosis_All, paste(g_icd10_type_1, collapse = "|")) ~ "Type_1",
    str_detect(Der_Diagnosis_All, paste(g_icd10_type_2, collapse = "|")) ~ "Type_2",
    TRUE ~ "Unknown")) %>% 
  mutate(Admission_Date = as.Date(Admission_Date))

# Seasonal spikes in DKA? 
g_DKA_ip_weekly_count_by_type <-
  g_ip_diabetes_diag_DKA %>%  
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  #filter(Admission_week > "2019-01-01") %>%
  filter(Admission_week < "2020-12-31")   

g_DKA_ip_weekly_count_by_type_vis <-
  g_DKA_ip_weekly_count_by_type %>%
  ggplot(aes(x = Admission_week, y = Activity, colour = diabetes_type, group = diabetes_type)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 200, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 200, label = str_wrap("Recovery", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 200, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  geom_point(size = 2, alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", colour = "Diabetes Type",
       title = "Seasonality features in DKA admissions for type 1 & 2 diabetes",
       subtitle = "Weekly count of inpatient admissions for diabetic ketoacidosis (DKA), National 2019-20",
       caption = "Note: 'Unknown' includes Malnutrition-related, Unspecified and Other diabetes")
  
# Trends in DKA by region and demographic

# Region 
g_DKA_IP_weekly_count_region <-
  g_ip_diabetes_diag_DKA %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type,  nhser20nm) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  #filter(Admission_week > "2019-01-01") %>% 
  filter(Admission_week < "2020-12-31") %>% 
  drop_na(nhser20nm) %>% 
  ungroup() 

g_DKA_IP_weekly_count_region_vis <-
  g_DKA_IP_weekly_count_region %>% 
  filter(diabetes_type != "Unknown") %>% 
  ggplot(aes(x = Admission_week, y = Activity, colour = diabetes_type, group = diabetes_type)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~nhser20nm) +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "NHSE Region",
       title = "No interesting finding ... ",
       subtitle = "Weekly count of IP admission for DKA, by Region, National 2019-20",
       caption = "L: Lockdown, R: Recovery")

# Age 
g_DKA_IP_weekly_count_age <-
  g_ip_diabetes_diag_DKA %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type,  Age_range) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  #filter(Admission_week > "2019-01-01") %>% 
  filter(Admission_week < "2020-12-31") %>% 
  drop_na(Age_range) %>% 
  ungroup()

g_DKA_IP_weekly_count_age_vis <-
  g_DKA_IP_weekly_count_age %>% 
  filter(diabetes_type != "Unknown") %>% 
  filter(Age_range != "100+") %>%
  #filter(Age_range %in% c("0-19", "20-39", "40-59", "60-79", "80-99")) %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = Age_range)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #geom_point(size = 1.5, alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~diabetes_type, scales = "free") +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Age range",
       title = "DKA is more common in under 40 type 1 and 40-80 year old type 2 diabetics ",
       subtitle = "Weekly count of IP admission for DKA, by Age range and diabetes type, National 2019-20",
       caption = "L: Lockdown, R: Recovery")


# Deprivation 
g_DKA_IP_weekly_count_deprivation  <-
  g_ip_diabetes_diag_DKA %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type,  IMD_Quintile) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  #filter(Admission_week > "2019-01-01") %>% 
  filter(Admission_week < "2020-12-31") %>% 
  drop_na(IMD_Quintile) %>% 
  ungroup() %>% 
  mutate(IMD_Quintile = factor(IMD_Quintile, levels = c(1,2,3,4,5)))

g_DKA_IP_weekly_count_deprivation_vis <-
  g_DKA_IP_weekly_count_deprivation %>%  
  filter(diabetes_type != "Unknown",
         IMD_Quintile != "NA") %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = IMD_Quintile, group = IMD_Quintile)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #geom_point(size = 1.5, alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~diabetes_type, scales = "free") +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "IMD Quintile",
       title = "Deprivatin gradient seen in attendances for DKA in both types of diabetes",
       subtitle = "Weekly count of IP admission for DKA, by IMD Quintile and diabetes type, National 2019-20",
       caption = "L: Lockdown, R: Recovery")

g_DKA_IP_weekly_count_deprivation %>%  
  filter(diabetes_type != "Unknown",
         IMD_Quintile != "NA") %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = IMD_Quintile, group = IMD_Quintile)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #geom_point(size = 1.5, alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~diabetes_type, scales = "free") +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "IMD Quintile",
       title = "Deprivatin gradient seen in attendances for DKA in both types of diabetes",
       subtitle = "Weekly count of IP admission for DKA, by IMD Quintile and diabetes type, National 2019-20",
       caption = "L: Lockdown, R: Recovery")

# Ethnicity 
g_DKA_IP_weekly_count_ethnicity  <-
  g_ip_diabetes_diag_DKA %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type,  Ethnicity_broad) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  #filter(Admission_week > "2019-01-01") %>% 
  filter(Admission_week < "2020-12-31") %>% 
  drop_na(Ethnicity_broad) %>% 
  ungroup() 

g_DKA_IP_weekly_count_ethnicity_vis <-
  g_DKA_IP_weekly_count_ethnicity %>%  
  filter(diabetes_type != "Unknown",
         Ethnicity_broad == "White") %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad, group = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #geom_point(size = 1.5, alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~diabetes_type, scales = "free") +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_manual(values = c("#5881c1")) +
  labs(x = "Date", y = "Count", color = "Broad ethnicity",
       title = "Seasonality trends in the White population are similar acrss diabetes types",
       subtitle = "Weekly count of IP admission for DKA, by ethnicity and diabetes type, National 2019-20")

# Excluding white group
g_DKA_IP_weekly_count_ethnicity_nonWhite_vis <-
g_DKA_IP_weekly_count_ethnicity %>%  
  filter(diabetes_type != "Unknown",
         Ethnicity_broad != "White") %>% 
  
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad, group = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #geom_point(size = 1.5, alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~diabetes_type, scales = "free") +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "",
       caption = "L: Lockdown, R: Recovery")

 library(patchwork)
g_DKA_IP_weekly_count_ethnicity_vis /
  g_DKA_IP_weekly_count_ethnicity_nonWhite_vis

g_DKA_IP_weekly_count_ethnicity %>%  
  filter(diabetes_type != "Unknown") %>% 
  ggplot(aes(x = Admission_week, y = Activity, colour = Ethnicity_broad)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  #annotate("text", x = as.Date("2020-05-13"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #annotate("text", x = as.Date("2020-08-24"), y = 0, label = str_wrap("R", 10) ,size = 2.5) +
  #annotate("text", x = as.Date("2020-11-30"), y = 0, label = str_wrap("L", 10) ,size = 2.5) +
  #geom_point(size = 1.5, alpha = 0.75) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_grid(Ethnicity_broad ~ diabetes_type, scales = "free") +
  theme(strip.text = element_text(face = "bold")) +
  scale_color_SU(palette = "main") +
  labs(x = "Date", y = "Count", color = "Broad ethnicity",
       title = "...",
       subtitle = "Weekly count of IP admission for DKA, by ethnicity and diabetes type, National 2019-20",
       caption = "L: Lockdown, R: Recovery")

## Proportional changes in DKA admissions ####
e_get_attendance_diff_DKA <- function(demographic_var) {
  
  g_ip_diabetes_diag_DKA <- 
    g_ip_diabetes_diag_DKA %>%
    mutate(demographic_var = {{demographic_var}})
  
  sub_data <-
    g_ip_diabetes_diag_DKA %>%  
    mutate(year = year(Admission_Date),
           month = months(Admission_Date), 
           week_number = isoweek(Admission_Date)) %>% 
    group_by(year, week_number, demographic_var) %>% 
    summarise(weekly_admissions = n_distinct(APCS_Ident)) %>% 
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
    #mutate(Avg1819 =  round(mean(c(n_2018, n_2019), na.rm=T))) %>% 
    mutate(diff = n_2020 - n_2019) %>% 
    mutate(PC_diff = case_when(
      week_number < 13 ~ as.numeric(0),
      week_number >= 13 ~ as.numeric(n_2020 - n_2019))) %>%  # Additional variable to start deficit from lockdown start
    ungroup() 
  
  sub_data_wide
}

# nhser20nm
# Age_range
# IMD_Quintile
# Ethnicity_broad
# Ethnic_Category_Desc

# Region
e_DKA_cum_diff_region <-
  e_get_attendance_diff_DKA(nhser20nm) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  drop_na(demographic_var) 

e_DKA_cum_diff_region_vis <-
  e_DKA_cum_diff_region %>%
  ggplot(aes(x = week_number, y = count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "...", 
       subtitle = "Cumulative change in weekly admission count for DKA between 2019 and 2020, by NHSE region",
       caption = "Admission difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative admission difference")

# Region - proportional difference between 2019-2020
e_DKA_prop_diff_region <-
  e_get_attendance_diff_DKA(nhser20nm) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) # Return CI's to negative where the def was negative

e_DKA_prop_diff_region_vis <-
  e_DKA_prop_diff_region %>% 
  ggplot(aes(y = reorder(demographic_var, diff_prop), x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "NHSE Region",
       title = "...",
       subtitle = "Proportional change in inpatient admission count for DKA between 2019-20 by NHSE Region",
       caption = "Proportional change = change between 2019-20 / total admission count for 2019")

# Age
e_DKA_cum_diff_age_range <-
  e_get_attendance_diff_DKA(Age_range) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup()

e_DKA_cum_diff_age_range_vis <-
  e_DKA_cum_diff_age_range %>%
  filter(demographic_var != "100+") %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "...", 
       subtitle = "Cumulative change in weekly admission count for DKA between 2019 and 2020, by Age range",
       caption = "Admission difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative admission difference")

# Age range - proportional difference between 2019-2020
e_DKA_prop_diff_age <-
  e_get_attendance_diff_DKA(Age_range) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI))  # Return CI's to negative where the def was negative

e_DKA_prop_diff_age_vis <-
  e_DKA_prop_diff_age %>% 
  filter(demographic_var != "100+") %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Age range",
       title = "...",
       subtitle = "Proportional change in inpatient admission count for DKA between 2019-20 by age range",
       caption = "Proportional change = change between 2019-20 / total admission count for 2019")

# Deprivation
e_DKA_cum_diff_deprivation <-
  e_get_attendance_diff_DKA(IMD_Quintile) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() %>% 
  mutate(demographic_var = factor(demographic_var, levels = c(1,2,3,4,5)))

e_DKA_cum_diff_deprivation_vis <-
  e_DKA_cum_diff_deprivation %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "Deprivation quintiles diverge at the onset of the second lockdown", 
       subtitle = "Cumulative change in weekly admission count for DKA between 2019 and 2020, by IMD Quintile ",
       caption = "Admission difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative admission difference")

# Deprivation - proportional difference between 2019-2020
e_DKA_prop_diff_deprivation <-
  e_get_attendance_diff_DKA(IMD_Quintile) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI)) %>%   # Return CI's to negative where the def was negative
  mutate(demographic_var = factor(demographic_var, levels = c(1,2,3,4,5)))

e_DKA_prop_diff_deprivation_vis <-
  e_DKA_prop_diff_deprivation %>% 
  drop_na(demographic_var) %>% 
  ggplot(aes(y = demographic_var, x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "IMD Quintile",
       title = "...",
       subtitle = "Proportional change in inpatient admission count for DKA between 2019-20 by IMD Quintile",
       caption = "Proportional change = change between 2019-20 / total admission count for 2019")

# Ethnicity
e_DKA_cum_diff_ethnicity <-
  e_get_attendance_diff_DKA(Ethnicity_broad) %>% 
  group_by(demographic_var) %>% 
  mutate(cum_def = cumsum(PC_diff)) %>% 
  pivot_longer(cols = c(-week_number, -demographic_var),
               names_to = "trend", 
               values_to = "count") %>% 
  filter(trend == "cum_def") %>% 
  filter(week_number < 53) %>% 
  drop_na(demographic_var) %>% 
  ungroup() 

e_DKA_cum_diff_ethnicity_vis <-
  e_DKA_cum_diff_ethnicity %>%
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(title = "...", 
       subtitle = "Change in weekly admission count for DKA between 2019 and 2020, by broad ethnicity",
       caption = "Admission difference = 2020 count - 2019 count",
       x = "Week number",
       y = "Cumulative admission difference")

(
e_DKA_cum_diff_ethnicity %>%
  filter(demographic_var == "White") %>% 
  mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
  filter(demographic_var != "100+") %>% 
  ggplot(aes(week_number, count, colour = demographic_var)) +
  geom_hline(yintercept = 0, colour = "#636363") + 
  annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = 20, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  annotate("text", x = 34, y = -100, label = str_wrap("Recovery", 10) ,size = 3) +
  annotate("text", x = 48, y = -100, label = str_wrap("Lockdown", 10) ,size = 3) +
  geom_smooth(method = 'loess', span = 0.3) +
  geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
  scale_color_manual(values = c("#5881c1")) +
  theme(legend.position = "none") +
  labs(title = "There was a sharp rise in DKA admissions in Black people during lockdown 1", 
       subtitle = "Cumulative change in weekly admission count for DKA between 2019 and 2020, by broad ethnicity",
       x = "",
       y = "Cumulative difference")
) /
  (
    # Excluding white group - Interesting
    e_DKA_cum_diff_ethnicity %>%
      filter(demographic_var != "White") %>% 
      mutate(label = if_else(week_number == max(week_number), as.character(demographic_var), NA_character_)) %>%
      filter(demographic_var != "100+") %>% 
      ggplot(aes(week_number, count, colour = demographic_var)) +
      geom_hline(yintercept = 0, colour = "#636363") + 
      annotate("segment", x = 13, xend = 13, y = -Inf, yend = Inf, lty = 2) +
      annotate("segment", x = 27, xend = 27, y = -Inf, yend = Inf, lty = 2) +
      annotate("segment", x = 42, xend = 42, y = -Inf, yend = Inf, lty = 2) +
      annotate("text", x = 20, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
      annotate("text", x = 34, y = 25, label = str_wrap("Recovery", 10) ,size = 3) +
      annotate("text", x = 48, y = 25, label = str_wrap("Lockdown", 10) ,size = 3) +
      geom_smooth(method = 'loess', span = 0.3) +
      geom_label_repel(aes(label = label), nudge_x = 1, size = 3.5,  na.rm = TRUE) +
      scale_color_SU(palette = "main") +
      theme(legend.position = "none") +
      labs( caption = "Admission difference = 2020 count - 2019 count",
            x = "Week Number",
            y = "Cumulative difference")
  )


# Ethnicity - proportional difference between 2019-2020
e_DKA_prop_diff_ethnicity <-
  e_get_attendance_diff_DKA(Ethnic_Category_Desc) %>% 
  drop_na(demographic_var) %>% 
  group_by(demographic_var) %>% 
  summarise(sum_diff = sum(PC_diff, na.rm = TRUE),
            sum_2019 = sum(n_2019, na.rm = TRUE)) %>% 
  mutate(diff_prop = sum_diff/sum_2019*100) %>% 
  mutate(sum_diff_adj = case_when(sum_diff >= 0 ~ sum_diff,
                                  sum_diff < 0 ~ -sum_diff )) %>% 
  group_by(demographic_var) %>% 
  mutate(LCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[1]*100,
         UCI = add4ci(x = sum_diff_adj, n = sum_2019, 0.95)$conf.int[2]*100) %>%
  mutate(LCI = case_when(sum_diff >= 0 ~ LCI, sum_diff < 0 ~ -LCI),
         UCI = case_when(sum_diff >= 0 ~ UCI, sum_diff < 0 ~ -UCI))

e_DKA_prop_diff_ethnicity_vis <-
  e_DKA_prop_diff_ethnicity %>% 
  ggplot(aes(y = reorder(demographic_var, diff_prop), x = diff_prop)) +
  geom_col(fill = "#f9bf07", colour = "#636363") +
  geom_errorbar(aes(xmin=LCI, xmax=UCI), width = 0.25, colour = "#636363") +
  labs(x = "Proportional change", 
       y = "Ethnicity",
       title = "Large proportional increase seen in Black subgroups",
       subtitle = "Proportional change in inpatient admission count for DKA between 2019-20 by ethnicity",
       caption = "Proportional change = change between 2019-20 / total admission count for 2019")

## Proportional changes in DKA and Type 1/2  ####

# Region 
e_scatter_region_DKAvsType1 <-
e_DKA_prop_diff_region %>% 
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type1_prop_diff_region %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, diff_prop, conf) %>% 
              rename(Type1_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) 

e_scatter_region_DKAvsType2 <-
  e_DKA_prop_diff_region %>% 
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type2_prop_diff_region %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, diff_prop, conf) %>% 
              rename(Type2_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) 

e_DKAvsType1_2_patch_region <-
(
# Type 1
  e_scatter_region_DKAvsType1 %>% 
  ggplot(aes(x = DKA_prop_change, y = Type1_prop_change, label = str_wrap(demographic_var, 12))) + 
  geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
  geom_hline(aes(yintercept = mean(Type1_prop_change)), linetype = "dashed", colour = "#636363") +
  geom_point(
    #aes(alpha = conf_tertile), 
    fill = "#f9bf07", colour = "black", pch = 21, size = 7) +
  geom_text_repel(box.padding = 1, size = 3) +
    theme(legend.position = "none") +
  labs(title = "Proportional disruption in DKA and Type 1 ED attendance was heightening in East of Engalnd",
       subtitle = "Proportional change in diabetes activity by NHSE Region",
       caption = "Dashed line: Axis mean",
       x = "Change in DKA admissions",
       y = "Change in type 1 diabetes ED attendances",
       alpha = "Confidence")
) +
  (
    # Type 2
    e_scatter_region_DKAvsType2 %>% 
      ggplot(aes(x = DKA_prop_change, y = Type2_prop_change, label = str_wrap(demographic_var, 12))) + 
      geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_hline(aes(yintercept = mean(Type2_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_point(
        #aes(alpha = conf_tertile), 
        fill = "#f9bf07", colour = "black", pch = 21, size = 7) +
      geom_text_repel(box.padding = 1, size = 3) +
      labs(x = "Change in DKA admissions",
           y = "Change in type 2 diabetes ED attendances",
           alpha = "Confidence")
  )

# Age range
e_scatter_age_DKAvsType1 <-
  e_DKA_prop_diff_age %>%
  filter(demographic_var != "100+") %>%
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type1_prop_diff_age %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, diff_prop, conf) %>% 
              rename(Type1_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) 

e_scatter_age_DKAvsType2 <-
  e_DKA_prop_diff_age %>% 
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type2_prop_diff_age %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, diff_prop, conf) %>% 
              rename(Type2_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) 

e_DKAvsType1_2_patch_age <-
(
  # Type 1
  e_scatter_age_DKAvsType1 %>%
    filter(demographic_var != "100+") %>% 
    ggplot(aes(x = DKA_prop_change, y = Type1_prop_change, label = str_wrap(demographic_var, 12))) + 
    geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
    geom_hline(aes(yintercept = mean(Type1_prop_change)), linetype = "dashed", colour = "#636363") +
    geom_point(
      #aes(alpha = conf_tertile), 
      fill = "#f9bf07", colour = "black", pch = 21, size = 7) +
    geom_text_repel(box.padding = 1, size = 3) +
    theme(legend.position = "none") +
    labs(title = "...",
         subtitle = "Proportional change in diabetes activity by Age range",
         caption = "Dashed line: Axis mean",
         x = "Change in DKA admissions",
         y = "Change in type 1 diabetes ED attendances",
         alpha = "Confidence")
) +
  (
    # Type 2
    e_scatter_age_DKAvsType2 %>% 
      filter(demographic_var != "100+") %>% 
      ggplot(aes(x = DKA_prop_change, y = Type2_prop_change, label = str_wrap(demographic_var, 12))) + 
      geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_hline(aes(yintercept = mean(Type2_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_point(
        #aes(alpha = conf_tertile), 
        fill = "#f9bf07", colour = "black", pch = 21, size = 7) +
      geom_text_repel(box.padding = 1, size = 3) +
      labs(x = "Change in DKA admissions",
           y = "Change in type 2 diabetes ED attendances",
           alpha = "Confidence")
  )

# Deprivation
e_scatter_deprivation_DKAvsType1 <-
  e_DKA_prop_diff_deprivation %>%
  filter(demographic_var != "100+") %>%
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type1_prop_diff_deprivation %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, diff_prop, conf) %>% 
              rename(Type1_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) 

e_scatter_deprivation_DKAvsType2 <-
  e_DKA_prop_diff_deprivation %>% 
  drop_na(demographic_var) %>% 
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type2_prop_diff_deprivation %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, diff_prop, conf) %>% 
              rename(Type2_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  mutate(conf_tertile = 
           case_when(
             confidence < quantile(confidence, probs = 0.33) ~ "High",
             confidence > quantile(confidence, probs = 0.33) & confidence < quantile(confidence, probs = 0.66) ~ "Medium",
             confidence > quantile(confidence, probs = 0.66) ~ "Low"
           ))  %>% 
  mutate(conf_tertile = factor(conf_tertile, levels = c("Low", "Medium", "High"))) 

e_DKAvsType1_2_patch_deprivation <-
(
  # Type 1
  e_scatter_deprivation_DKAvsType1 %>%
    filter(demographic_var != "100+") %>% 
    ggplot(aes(x = DKA_prop_change, y = Type1_prop_change, label = str_wrap(demographic_var, 12))) + 
    geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
    geom_hline(aes(yintercept = mean(Type1_prop_change)), linetype = "dashed", colour = "#636363") +
    geom_point(
      #aes(alpha = conf_tertile), 
      fill = "#f9bf07", colour = "black", pch = 21, size = 7) +
    geom_text_repel(box.padding = 1, size = 3) +
    theme(legend.position = "none") +
    labs(title = "...",
         subtitle = "Proportional change in diabetes activity by IMD Quintile ",
         caption = "Dashed line: Axis mean",
         x = "Change in DKA admissions",
         y = "Change in type 1 diabetes ED attendances",
         alpha = "Confidence")
) +
  (
    # Type 2
    e_scatter_deprivation_DKAvsType2 %>% 
      filter(demographic_var != "100+") %>% 
      ggplot(aes(x = DKA_prop_change, y = Type2_prop_change, label = str_wrap(demographic_var, 12))) + 
      geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_hline(aes(yintercept = mean(Type2_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_point(
        #aes(alpha = conf_tertile), 
        fill = "#f9bf07", colour = "black", pch = 21, size = 7) +
      geom_text_repel(box.padding = 1, size = 3) +
      labs(x = "Change in DKA admissions",
           y = "Change in type 2 diabetes ED attendances",
           alpha = "Confidence")
  )

# Ethnicity
e_scatter_ethnicity_DKAvsType1 <-
  e_DKA_prop_diff_ethnicity %>%
  filter(demographic_var != "100+") %>%
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, sum_2019, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type1_prop_diff_ethnicity %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, sum_2019, diff_prop, conf) %>% 
              rename(Type1_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  group_by(demographic_var) %>% 
  mutate(Frequency = sum(sum_2019.x, sum_2019.y)) %>% 
  ungroup() %>%
  mutate(Frequency_tertile = 
           case_when(
             Frequency <= quantile(Frequency, probs = 0.33) ~ "Low",
             Frequency > quantile(Frequency, probs = 0.33) & Frequency < quantile(Frequency, probs = 0.66) ~ "Medium",
             Frequency >= quantile(Frequency, probs = 0.66) ~ "High"
           ))  %>% 
  mutate(Frequency_tertile = factor(Frequency_tertile, levels = c("Low", "Medium", "High"))) 

e_scatter_ethnicity_DKAvsType2 <-
  e_DKA_prop_diff_ethnicity %>% 
  drop_na(demographic_var) %>% 
  mutate(conf = UCI - LCI) %>% 
  select(demographic_var, sum_2019, sum_2019, diff_prop, conf) %>% 
  rename(DKA_prop_change = diff_prop) %>% 
  left_join(e_type2_prop_diff_ethnicity %>% 
              mutate(conf = UCI - LCI) %>% 
              select(demographic_var, sum_2019, diff_prop, conf) %>% 
              rename(Type2_prop_change = diff_prop), 
            by = c("demographic_var")) %>%
  mutate(confidence = mean(conf.x, conf.y)) %>% 
  ungroup() %>%
  group_by(demographic_var) %>% 
  mutate(Frequency = sum(sum_2019.x, sum_2019.y)) %>% 
  ungroup() %>%
  mutate(Frequency_tertile = 
           case_when(
             Frequency <= quantile(Frequency, probs = 0.33) ~ "Low",
             Frequency > quantile(Frequency, probs = 0.33) & Frequency < quantile(Frequency, probs = 0.66) ~ "Medium",
             Frequency >= quantile(Frequency, probs = 0.66) ~ "High"
           ))  %>% 
  mutate(Frequency_tertile = factor(Frequency_tertile, levels = c("Low", "Medium", "High")))

e_DKAvsType1_2_patch_ethnicity <-
(
  # Type 1
  e_scatter_ethnicity_DKAvsType1 %>%
    filter(demographic_var != "Chinese") %>%  ## NaN value removed
    ggplot(aes(x = DKA_prop_change, y = Type1_prop_change, label = str_wrap(demographic_var, 12))) + 
    geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
    geom_hline(aes(yintercept = mean(Type1_prop_change)), linetype = "dashed", colour = "#636363") +
    geom_point(
      aes(size  = Frequency_tertile), 
      fill = "#f9bf07", colour = "black", pch = 21) +
    geom_text_repel(box.padding = 1, size = 3, max.overlaps = 100) +
    theme(legend.position = "none") +
    labs(title = "...",
         subtitle = "Proportional change in diabetes activity by ethnicity ",
         caption = "Dashed line: Axis mean",
         x = "Change in DKA admissions",
         y = "Change in type 1 diabetes ED attendances",
        size = "Frequency")
) +
  (
    # Type 2
    e_scatter_ethnicity_DKAvsType2 %>% 
      filter(demographic_var != "White and asian") %>%  ## NaN value removed
      ggplot(aes(x = DKA_prop_change, y = Type2_prop_change, label = str_wrap(demographic_var, 12))) + 
      geom_vline(aes(xintercept = mean(DKA_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_hline(aes(yintercept = mean(Type2_prop_change)), linetype = "dashed", colour = "#636363") +
      geom_point(
        aes(size  = Frequency_tertile), 
        fill = "#f9bf07", colour = "black", pch = 21) +
      geom_text_repel(box.padding = 1, size = 3,max.overlaps = 100) +
      labs(x = "Change in DKA admissions",
           y = "Change in type 2 diabetes ED attendances",
           size = "Frequency")
  )






## Severity - LoS ####
# LoS range

# Critical care days - Der_SEM_CCP_Total_Days

g_ip_diabetes_diag_DKA %>% 
  select(contains("LOS")) %>% 
  mutate(Der_CC_days = Der_Episode_LoS - Der_Episode_Adj_LoS)

# LoS time series 
g_ip_diabetes_diag_DKA_LoS_ts <-
  g_ip_diabetes_diag_DKA %>% 
  mutate(Der_CC_days = Der_Episode_LoS - Der_Episode_Adj_LoS) %>% 
  group_by(APCE_Ident) %>% 
  mutate(Spell_LoS = sum(Der_Episode_LoS),
         Spell_CC_days = sum(Der_CC_days)) %>% 
  ungroup() %>% 
  mutate(Admission_week = floor_date(Admission_Date, unit = "week")) %>% 
  ungroup() %>% 
  group_by(Admission_week, diabetes_type) %>% 
  summarise(Activity = n_distinct(APCS_Ident),
            Mean_LoS_wk = mean(Spell_LoS),
            Mean_CC_days_per_spell = mean(Spell_CC_days)) %>% 
  #filter(Admission_week >= "2020-01-01") %>% 
  filter(Admission_week < "2020-12-31") %>% 
  ungroup() 

g_ip_diabetes_diag_DKA_LoS_ts_vis <-
g_ip_diabetes_diag_DKA_LoS_ts %>% 
  filter(diabetes_type != "Unknown") %>% 
  ggplot(aes(x = Admission_week, y = Mean_LoS_wk, colour = diabetes_type, group = diabetes_type)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 8, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 8, label = str_wrap("Recovery", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 8, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date",
       y = "Average admission length (Days)",
       title = "DKA associated LoS was most influenced by COVID-19 in type 2 diabetics",
       subtitle = "Weekly mean length of stay for inpatient DKA admissions by diabetes type, National 2018-20",
       color = "Diabetes type")

g_ip_diabetes_diag_DKA_LoS_ts_critical_care <-
g_ip_diabetes_diag_DKA_LoS_ts %>% 
  filter(diabetes_type != "Unknown") %>% 
  ggplot(aes(x = Admission_week, y = Mean_CC_days_per_spell, colour = diabetes_type, group = diabetes_type)) +
  annotate("segment", x = as.Date("2020-03-23"), xend = as.Date("2020-03-23"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-07-04"), xend = as.Date("2020-07-04"), y = -Inf, yend = Inf, lty = 2) +
  annotate("segment", x = as.Date("2020-10-14"), xend = as.Date("2020-10-14"), y = -Inf, yend = Inf, lty = 2) +
  annotate("text", x = as.Date("2020-05-13"), y = 1, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-08-24"), y = 1, label = str_wrap("Recovery", 10) ,size = 2.5, angle = 90) +
  annotate("text", x = as.Date("2020-11-24"), y = 1, label = str_wrap("Lockdown", 10) ,size = 2.5, angle = 90) +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_SU(palette = "main") +
  labs(x = "Date",
       y = "Average number critical care days per admission",
       title = "Critical care use was more impacted by COVID-19 in type 2 diabetes DKA admissions",
       subtitle = "Weekly mean number of critical care days per inpatient DKA admissions by diabetes type, National 2018-20",
       color = "Diabetes type")

g_ip_diabetes_diag_DKA_LoS_dist <-
g_ip_diabetes_diag_DKA %>% 
  group_by(APCE_Ident) %>% 
  mutate(Spell_LoS = sum(Der_Episode_LoS)) %>% 
  ungroup() %>% 
  mutate(Admission_month = floor_date(Admission_Date, unit = "month")) %>% 
  ungroup() %>% 
  group_by(Admission_month, diabetes_type,  Spell_LoS) %>% 
  summarise(Activity = n_distinct(APCS_Ident)) %>% 
  mutate(mean = mean(Spell_LoS)) %>% 
  #filter(Admission_month >= "2020-01-01") %>% 
  filter(Admission_month < "2020-12-31") %>% 
  drop_na(Spell_LoS) %>% 
  ungroup() %>%
  mutate(month = months(Admission_month)) %>% 
  mutate(month = fct_reorder(month, Admission_month))

# Test density graphs
g_ip_diabetes_diag_DKA_LoS_dist %>% 
  filter(Admission_month >= "2020-01-01") %>% 
  filter(Activity > 10) %>% 
  
  ggplot(aes(x = Spell_LoS, fill = month)) +
  geom_density(alpha = 0.8) +
  facet_wrap(~month) +
  scale_fill_SU(palette = "main") +
  theme(legend.position = "none") +
  labs(x = "Addmission LoS", y = "Proportion",
       title = "...",
       subtitle = "Distribution of DKA attendances by length of stay (LoS) by month 2020")
  
  
  
# Plot 2019 monthly distributions over 2020

ggplot(g_ip_diabetes_diag_DKA_LoS_dist %>%  
         filter(substr(Admission_month,1,4) == 2019) %>% 
         select(-month) , 
       aes(x = Spell_LoS)) +
  geom_density(data = g_ip_diabetes_diag_DKA_LoS_dist %>% 
                 filter(substr(Admission_month,1,4) == 2019), 
               aes(x = Spell_LoS, fill = month), alpha = 0.8) +
  geom_density() +
  #geom_vline(data = MLU_dist_scenarios, aes(xintercept = mean), linetype = "dashed") + # dashed line = scenario mean
  geom_vline(aes(xintercept = mean)) + # solid line = baseline mean
  facet_wrap(~ month) +
  scale_fill_SU(guide = FALSE) +
  xlim(0,50) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour="#bdbdbd", fill="#f0f0f0")) +
  labs(title = "...",
       subtitle = "Distribution of DKA attendances by length of stay (LoS) by month 2020",
       x = "Admission LoS (Days)",
       y = "Density (%)") 

## Seasonality in DKA ####

h.con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   server = "PRODNHSESQL101", 
                   Database = "NHSE_Reference")

h_ICD_ref <-
  as_tibble(
    dbGetQuery(h.con, '
select *
from [NHSE_Reference].[DBO].[tbl_Ref_ClinCode_ICD10]'))

# Disconnect! # 
dbDisconnect(h.con) 
  
h_ICD_ref_chapter <-
h_ICD_ref %>% 
  mutate(chapter = substr(ICD10_L4_Code,1,1)) %>% 
  select(chapter, ICD10_Chapter_Code, ICD10_Chapter_Desc) %>% 
  distinct()

h_all_other_diagnoses <-
g_ip_diabetes_diag_DKA %>% #634,253
  select(APCS_Ident, contains("Diagnosis_Code")) %>% 
  pivot_longer(cols = -APCS_Ident,
               names_to = "Diagnosis_number",
               values_to = "Diagnosis_code") %>% 
  left_join(h_ICD_ref, by = c("Diagnosis_code" = "ICD10_L4_Code")) %>% # bring in diagnosis descriptions
  select(1:4, ICD10_Chapter_Desc) %>% 
  drop_na(Diagnosis_code) %>% 
  distinct(ICD10_L4_Desc)

h_all_other_diagnoses %>% 
  mutate(ICD10_L4_Desc = as.character(ICD10_L4_Desc)) %>% 
  mutate(two_dig_diag = substr(ICD10_L4_Desc,1,2))
  group_by(substr(ICD10_L4_Desc,1,2))

g_ip_diabetes_diag_DKA %>% 
  mutate(two_dig_secondary_diag = substr(Der_Secondary_Diagnosis_Code_1, 1, 2)) %>% 
  left_join(h_ICD_ref %>%
              mutate(two_dig_diag = substr(ICD10_L4_Code, 1, 2)) %>% 
              select(two_dig_diag, ICD10_Chapter_Desc) %>% 
              distinct()
              )

# Link to admissions with alcohol included
# Alcohol vs non-alcohol related

h_dka_alcohol_related_icd10_codes <-
g_ip_diabetes_diag_DKA %>% #634,253
  select(APCS_Ident, contains("Diagnosis_Code")) %>% 
  pivot_longer(cols = -APCS_Ident,
               names_to = "Diagnosis_number",
               values_to = "Diagnosis_code") %>% 
  left_join(h_ICD_ref, by = c("Diagnosis_code" = "ICD10_L4_Code")) %>% # bring in diagnosis descriptions
  select(1:4) %>% 
  drop_na(Diagnosis_code) %>%  # remove NA diagnosis codes
  filter(str_detect(ICD10_L4_Desc, "Alcohol")) %>% 
  select(Diagnosis_code) %>%
  distinct() %>%  
  as.list()

h_dka_alcohol_related_icd10 <- 
  c("Z502", "K860", "Z721", "K703", "K709", "Z714", "Y919", "K852", "G621", "K700", "K292", "I426", 
    "K701", "K704", "T519", "K702", "Y573", "E244")

g_ip_diabetes_diag_DKA_alchol <-
g_ip_diabetes_diag_DKA %>% 
  mutate(alcohol_diag_flag = case_when(
    str_detect(Der_Diagnosis_All, paste(h_dka_alcohol_related_icd10, collapse = "|")) ~ "alcohol_related",
    TRUE ~ "not_alcohol_related"))

g_ip_diabetes_diag_DKA_alchol %>% 
  select(Der_Diagnosis_All, alcohol_diag_flag) %>% 
  group_by(alcohol_diag_flag) %>% summarise(n = n())

g_ip_diabetes_diag_DKA_alchol %>% 
  #mutate(Admission_day = floor_date(as.Date(Admission_Date), unit = "week")) %>% 
  group_by(Admission_Date, alcohol_diag_flag) %>% 
  summarise(Admission_count = n_distinct(APCS_Ident)) %>% 
  
  ggplot(aes(x = Admission_Date, y = Admission_count, 
             colour = alcohol_diag_flag, fill = alcohol_diag_flag)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'loess', span = 0.3) +
  facet_wrap(~alcohol_diag_flag, scales = "free")

# ICD10 chapter of secondary diagnosis
#ICD10 Chapters 
h_ICD10_Chapters <-
  tribble (
    ~ICD10_Chapter_Number, ~ICD10_Chapter, ~ICD10_Chapter_Short,
    #|--|--|--
    1,	"chapter_1",	"Infectious and parasitic diseases",
    2,	"chapter_2",	"Neoplasms",
    3,	"chapter_3",	"Blood and immune system",
    4,  "chapter_4",  "Endocrine, nutritional and metabolic diseases",
    5,	"chapter_5",	"Mental and behavioural disorders",
    6,	"chapter_6",  "Nervous system",
    7,	"chapter_7",	"Eye and adnexa",
    8,	"chapter_8",	"Ear and mastoid process",
    9,	"chapter_9",	"Circulatory system",
    10,	"chapter_10",	"Respiratory system",
    11,	"chapter_11",	"Digestive system",
    12,	"chapter_12",	"Skin and subcutaneous tissue",
    13,	"chapter_13",	"MSK and connective tissue",
    14,	"chapter_14",	"Genitourinary system",
    15,	"chapter_15",	"Pregnancy and childbirth",
    16,	"chapter_16",	"Perinatal Conditions",
    17,	"chapter_17",	"Congenital malformations",
    18,	"chapter_18",	"Signs & Symptoms not elsewhere classified",
    19,	"chapter_19",	"Injury, poisoning and other external causes",
    20,	"chapter_20",	"External causes of morbidity and mortality",
    21,	"chapter_21",	"Factors influencing health status and contact with health services",
    22,	"chapter_22",	"Codes for special purposes",
    99,	"chapter_NA",	"NA",
    98, "chapter_98", "Other"
  ) %>%   
  clean_names()


g_ip_diabetes_diag_DKA %>% 
  mutate(icd_chapter = case_when(Der_Secondary_Diagnosis_Code_1 == "NUL" ~ 99,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^[AB]") ~ 1,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^(C|D[0-4])") ~ 2,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^D[5-9]") ~ 3,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^E") ~ 4,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^F") ~ 5,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^G") ~ 6,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^H[0-5]") ~ 7,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^H[6-9]") ~ 8,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^I") ~ 9 ,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^J") ~ 10, 
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^K") ~ 11,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^L") ~ 12, 
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^M") ~ 13,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^N") ~ 14,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^O") ~ 15,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^P") ~ 16,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^Q") ~ 17,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^R") ~ 18,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^[ST]") ~ 19,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^[VWXY]") ~ 20,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^Z") ~ 21,
                               str_detect(Der_Secondary_Diagnosis_Code_1, "^U") ~ 22)) %>% 
  left_join(h_ICD10_Chapters, by = c("icd_chapter" = "icd10_chapter_number")) %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>%
  #left_join(h_ICD_ref, by = c("Der_Secondary_Diagnosis_Code_1" = "ICD10_L4_Code")) %>% # bring in diagnosis descriptions
  group_by(Admission_week, icd10_chapter_short) %>% 
  summarise(Admission_count = n_distinct(APCS_Ident)) %>% 
  drop_na(icd10_chapter_short) %>% 
  
  ggplot(aes(x = Admission_week, y = Admission_count, 
             colour = icd10_chapter_short)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'loess', span = 0.3) +
  facet_wrap(~icd10_chapter_short, scales = "free_y") +
  theme(legend.position = "none")

###
g_ip_diabetes_diag_DKA %>% 
  mutate(Admission_week = floor_date(as.Date(Admission_Date), unit = "week")) %>%
  left_join(h_ICD_ref, by = c("Der_Secondary_Diagnosis_Code_1" = "ICD10_L4_Code")) %>% # bring in diagnosis descriptions
  group_by(Admission_week, ICD10_Chapter_Desc) %>% 
  summarise(Admission_count = n_distinct(APCS_Ident)) %>% 
  
  ggplot(aes(x = Admission_week, y = Admission_count, 
             colour = as.character(ICD10_Chapter_Desc))) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'loess', span = 0.3) +
  facet_wrap(~ICD10_Chapter_Desc) +
  theme(legend.position = "none")


######
g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_1) %>% 
  group_by(Der_Secondary_Diagnosis_Code_1) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)

g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_2) %>% 
  group_by(Der_Secondary_Diagnosis_Code_2) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)

g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_3) %>% 
  group_by(Der_Secondary_Diagnosis_Code_3) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)

g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_4) %>% 
  group_by(Der_Secondary_Diagnosis_Code_4) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)  

g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_5) %>% 
  group_by(Der_Secondary_Diagnosis_Code_5) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)

g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_6) %>% 
  group_by(Der_Secondary_Diagnosis_Code_6) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)

g_ip_diabetes_diag_DKA %>% #233,171
  select(Der_Secondary_Diagnosis_Code_7) %>% 
  group_by(Der_Secondary_Diagnosis_Code_7) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n)*100)




