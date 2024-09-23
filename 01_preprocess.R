library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(crosstalk)
#constants------------------------------------
lmo_colours <- c("#8abbe8","#2a2d64")
df_list <- list()
#functions---------------------------
get_current <- function(tbbl){
  tbbl$value[tbbl$name==min(tbbl$name)]
}
get_cagr <- function(tbbl){
  start <- tbbl$value[tbbl$name==min(tbbl$name)]
  end <- tbbl$value[tbbl$name==max(tbbl$name)]
  (end/start)^(1/(max(tbbl$name-min(tbbl$name))))-1
}
#read in the mapping data--------------------------
noc_broad <- read_csv(here("data","noc_broad.csv"), col_types = "c")
noc_teer <- read_csv(here("data","noc_teer.csv"))|>
  mutate(teer_num=as.character(teer_num))
industry_mapping <- read_csv(here("data","lmo64_agg_mapping.csv"))|>
  select(industry=lmo_industry_name, aggregate_industry)
#read in LMO data------------------------
demand_occupation <- read_excel(here("data", "demand_occupation.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  clean_names()|>
  filter(variable %in% c("Expansion Demand", "Replacement Demand", "Job Openings"))|>
  group_by(noc, description, variable, geographic_area)|>
  summarize(value=sum(value))|>
  mutate(broad_num=str_sub(noc,2,2),
         teer_num=str_sub(noc,3,3))|>
  full_join(noc_broad)|>
  full_join(noc_teer)|>
  ungroup()|>
  select(-broad_num, -teer_num, -noc)|>
  pivot_wider(names_from = variable, values_from = value)|>
  group_by(broad, teer)|>
  arrange(`Job Openings`, .by_group = TRUE)|>
  pivot_longer(cols=contains("Demand"))|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

demand_industry <- read_excel(here("data","demand_industry.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  clean_names()|>
  filter(variable %in% c("Expansion Demand", "Replacement Demand", "Job Openings"))|>
  group_by(industry, variable, geographic_area)|>
  summarize(value=sum(value))|>
  fuzzyjoin::stringdist_full_join(industry_mapping)|> #names do not match exactly
  ungroup()|>
  select(industry=industry.y, aggregate_industry, geographic_area, variable, value)|>
  pivot_wider(names_from = variable, values_from = value)|>
  group_by(geographic_area)|>
  arrange(`Job Openings`, .by_group = TRUE)|>
  pivot_longer(cols=contains("Demand"))|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

employment_occupation <- read_excel(here("data","employment_occupation.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  mutate(name=as.numeric(name))|>
  clean_names()|>
  mutate(broad_num=str_sub(noc,2,2),
         teer_num=str_sub(noc,3,3))|>
  full_join(noc_broad)|>
  full_join(noc_teer)|>
  ungroup()|>
  select(-broad_num, -teer_num, -noc)|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

employment_industry <- read_excel(here("data","employment_industry.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  mutate(name=as.numeric(name))|>
  clean_names()|>
  fuzzyjoin::stringdist_full_join(industry_mapping)|> #names do not match
  ungroup()|>
  select(industry=industry.y, aggregate_industry, geographic_area, name, value)|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

#pie charts---------------------

df_list[["job_openings_pie"]] <- demand_occupation|>
  group_by(name, geographic_area)|>
  summarize(value=sum(value, na.rm = TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm=TRUE))|>
  filter(`Job Openings`>0)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_pie"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","jo_pie.rds"))

region_emp <- employment_occupation|>
  filter(name==min(name))|>
  group_by(geographic_area)|>
  summarize(value=sum(value))|>
  mutate(name=paste0("Employment: ", geographic_area),
         colour=lmo_colours[1])

residual_emp <- region_emp|>
  select(geographic_area, value)|>
  mutate(value=max(value)-value,
         name="Employment: All other regions",
         colour=lmo_colours[2])

df_list[["employment_pie"]] <- bind_rows(region_emp, residual_emp)|>
  arrange(geographic_area)

df_list[["employment_pie"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","emp_pie.rds"))

#by broad occupational groups----------------------------------

df_list[["job_openings_broad"]] <- demand_occupation|>
  group_by(broad, geographic_area, name)|>
  summarize(value=sum(value, na.rm=TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_broad"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","jo_broad.rds"))

df_list[["employment_broad"]] <- employment_occupation|>
  filter(name==min(name))|>
  group_by(broad, geographic_area)|>
  summarize(value=sum(value, na.rm=TRUE))|>
  arrange(value)|>
  mutate(colour=lmo_colours[1])

df_list[["employment_broad"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","emp_broad.rds"))

df_list[["cagr_broad"]] <- employment_occupation|>
  group_by(broad, geographic_area, name)|>
  summarize(value=sum(value, na.rm=TRUE))|>
  nest()|>
  mutate(value=map_dbl(data, get_cagr),
         colour=lmo_colours[1])|>
  select(-data)|>
  arrange(value)

df_list[["cagr_broad"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","cagr_broad.rds"))

#by TEER----------------------------------------

df_list[["job_openings_teer"]] <- demand_occupation|>
  group_by(teer, geographic_area, name)|>
  summarize(value=sum(value, na.rm=TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_teer"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","jo_teer.rds"))

df_list[["employment_teer"]] <-employment_occupation|>
  filter(name==min(name))|>
  group_by(teer, geographic_area)|>
  summarize(value=sum(value, na.rm=TRUE))|>
  arrange(value)|>
  mutate(colour=lmo_colours[1])

df_list[["employment_teer"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","emp_teer.rds"))

df_list[["cagr_teer"]] <- employment_occupation|>
  group_by(teer, geographic_area, name)|>
  summarize(value=sum(value, na.rm=TRUE))|>
  nest()|>
  mutate(value=map_dbl(data, get_cagr),
         colour=lmo_colours[1])|>
  select(-data)|>
  arrange(value)

df_list[["cagr_teer"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","cagr_teer.rds"))

# by 2 digit NOC-------------------------------------------

df_list[["job_openings_two_digit"]] <- demand_occupation|>
  group_by(geographic_area, teer, broad, name)|>
  summarize(value=sum(value, na.rm=TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, desc(teer), `Job Openings`)|>
  unite(two_digit, broad, teer, sep=": ")|>
  filter(`Job Openings`>0)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_two_digit"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","jo_two.rds"))

df_list[["employment_two_digit"]] <- employment_occupation|>
  filter(name==min(name))|>
  group_by(geographic_area, teer, broad, name)|>
  summarize(value=sum(value, na.rm=TRUE))|>
  arrange(geographic_area, desc(teer), value)|>
  unite(two_digit, broad, teer, sep=": ")|>
  mutate(colour=lmo_colours[1])

df_list[["employment_two_digit"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","emp_two.rds"))

df_list[["cagr_two_digit"]] <- employment_occupation|>
  group_by(geographic_area, teer, broad, name)|>
  summarize(value=sum(value, na.rm=TRUE))|>
  nest()|>
  mutate(value=map_dbl(data, get_cagr),
         colour=lmo_colours[1])|>
  select(-data)|>
  arrange(geographic_area, desc(teer), value)|>
  unite(two_digit, broad, teer, sep=": ")

df_list[["cagr_two_digit"]]|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","cagr_two.rds"))

#drill down to occupation------------------------------------
df_list[["job_openings_all"]] <- demand_occupation|>
  ungroup()|>
  arrange(geographic_area, broad, teer, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_all"]]|>
  SharedData$new(~interaction(geographic_area, broad, teer), group="drilldown")|>
  write_rds(here("out","jo_occ.rds"))

df_list[["employment_all"]] <-employment_occupation|>
  filter(name==min(name),
         value>0)|>
  ungroup()|>
  arrange(geographic_area, broad, teer, value)|>
  mutate(colour=lmo_colours[1])

df_list[["employment_all"]]|>
  SharedData$new(~interaction(geographic_area, broad, teer), group="drilldown")|>
  write_rds(here("out","emp_occ.rds"))

df_list[["cagr_all"]] <- employment_occupation|>
  group_by(description, geographic_area, teer, broad)|>
  nest()|>
  mutate(value=map_dbl(data, get_cagr),
         colour=lmo_colours[1])|>
  select(-data)|>
  arrange(geographic_area, broad, teer, value)

df_list[["cagr_all"]]|>
  SharedData$new(~interaction(geographic_area, broad, teer), group="drilldown")|>
  write_rds(here("out","cagr_occ.rds"))

#By aggregate industry----------------------------

df_list[["job_openings_aggregate_industry"]] <- demand_industry|>
  group_by(aggregate_industry, geographic_area, name)|>
  summarize(`Job Openings`=sum(`Job Openings`, na.rm = TRUE),
            value=sum(value, na.rm=TRUE)
  )|>
  ungroup()|>
  arrange(geographic_area, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_aggregate_industry"]]|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","jo_ind_agg.rds"))

df_list[["employment_aggregate_industry"]]<- employment_industry|>
  filter(name==min(name))|>
  group_by(geographic_area, aggregate_industry)|>
  summarize(value=sum(value, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, value)|>
  mutate(colour=lmo_colours[1])

df_list[["employment_aggregate_industry"]]|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","emp_agg_ind.rds"))

df_list[["cagr_aggregate_industry"]] <- employment_industry|>
  group_by(aggregate_industry, geographic_area, name)|>
  summarize(value=sum(value, na.rm = TRUE))|>
  nest()|>
  mutate(value=map_dbl(data, get_cagr),
         colour=lmo_colours[1])|>
  select(-data)|>
  ungroup()|>
  arrange(geographic_area, value)

df_list[["cagr_aggregate_industry"]]|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","cagr_agg_ind.rds"))

#by LMO industry-----------------------------------------

df_list[["job_openings_lmo_industry"]] <- demand_industry|>
  filter(`Job Openings`>0)|>
  group_by(geographic_area)|>
  slice_max(`Job Openings`, n=100)|> #this keeps the top 50!!! industries (2 rows for each industry/region)
  arrange(`Job Openings`, .by_group = TRUE)|>
  mutate(colour=if_else(name=="Expansion Demand",lmo_colours[1], lmo_colours[2]))

df_list[["job_openings_lmo_industry"]]|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","jo_ind.rds"))

df_list[["employment_lmo_industry"]] <- employment_industry|>
  filter(name==min(name))|>
  group_by(geographic_area)|>
  slice_max(value, n=50)|>
  ungroup()|>
  arrange(geographic_area, value)|>
  mutate(colour=lmo_colours[1])

df_list[["employment_lmo_industry"]] |>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","emp_ind.rds"))

df_list[["cagr_lmo_industry"]] <- employment_industry|>
  group_by(industry, geographic_area)|>
  nest()|>
  mutate(value=map_dbl(data, get_cagr),
         colour=lmo_colours[1])|>
  select(-data)|>
  group_by(geographic_area)|>
  slice_max(value, n=50)|>
  ungroup()|>
  arrange(geographic_area, value)

df_list[["cagr_lmo_industry"]]|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","cagr_ind.rds"))

#dump data into excel file.

openxlsx::write.xlsx(df_list, "LMO_dashboard_datasets.xlsx")


