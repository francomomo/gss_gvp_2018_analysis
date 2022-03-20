# Purpose: The purpose of this script is to clean the 2018 Canada General Social 
# Survey (GSS) Giving, Volunteering, and Participating (GVP) data that would be 
# used in this paper. This script cleaned and subset a portion of the GVP data.
#
# Author: Ka Chun Mo
#
# Thanks: Thank you to Rohan Alexander and Sam Caetano who provided some code
# to cleaned the data.

# Import library for data cleaning
library(janitor)
# Import library for data manipulation
library(tidyverse)

# Read raw 2018 GSS GVP data
readr::read_csv('./inputs/data/gss_giving_volunteering_participating.csv')
raw_data_gvp <- read_csv('./inputs/data/gss_giving_volunteering_participating.csv')
# Read 2018 GSS GVP dictionary that contains variable column names
dict_gvp <- read_lines("./inputs/data-reference/gss_gvp_dict.txt", skip = 18)
# Read 2018 GSS GVP labels that contains options to the survey questions
labels_raw_gvp <- read_file("./inputs/data-reference/gss_gvp_labels.txt")

# Extract variable names and variable descriptions
variable_descriptions <- as_tibble(dict_gvp) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data_gvp)[-1]))

# Removed unused variables
variable_descriptions <- variable_descriptions %>%
  filter(!startsWith(variable_description, '_column('))

# Extract variable names and possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw_gvp, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na() %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# Setup REGEX
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# Some cleanup for REGEX
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement) %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

# Extract variables that we are interested in
gss_gvp <- raw_data_gvp %>% 
  select(CASEID, 
         # Demographic
         # age group
         dh1gage,
         # place of birth (Canada/macro-region)
         brthcan,
         brthmacr,
         # gender
         gndr,
         # marital status
         marstat,
         # education (highest degree)
         dh1ged,
         # province of residence
         prv,
         # household size
         hsdsizec,

         # Giving
         # decisions on giving
         dg_005,
         dg_030,
         dg_050,
         dg_060,
         dg_080,
         dg_090,
         # financial giving
         fg1a_030,
         fg1a_040,
         fg1a_050,
         fg1a_060,
         fg1a_070,
         fg1a_080,
         fg1a_090,
         fg1a_100,
         fg1a_110,
         fg1a_120,
         fg1a_130,
         fg1a_140,
         fg1a_170,
         # reasons for giving
         rg_010,
         rg_020,
         rg_030,
         rg_035,
         rg_040,
         rg_050,
         rg_060,
         rg_070,
         # reasons for not giving more
         ng_020,
         ng_030,
         ng_040,
         ng_050,
         ng_060,
         ng_070,
         ng_080,
         ng_090,
         ng_110,
         ng_130,
         ng_150,
         ng_160,
         
         # Volunteering
         # how to volunteer
         gv_020, # with family
         gv_030, # with friends
         # reasons for volunteering
         rv_020,
         rv_025,
         rv_030,
         rv_040,
         rv_050,
         rv_060,
         rv_070,
         rv_080,
         rv_090,
         rv_100,
         rv_110,
         # reasons for not volunteering more
         nv_020,
         nv_030,
         nv_040,
         nv_050,
         nv_060,
         nv_070,
         nv_080,
         nv_090,
         nv_100,
         nv_110,
         nv_120,
         nv_130,
         # formal volunteer activities
         fv_020,
         fv_030,
         fv_050,
         fv_080,
         fv_090,
         fv_100,
         fv_110,
         fv_120,
         fv_130,
         fv_150,
         fv_160,
         
         # Participating
         # youth experience
         ea_020,
         ea_030,
         ea_040,
         ea_050,
         ea_060,
         ea_070,
         ea_080,
         ea_090,
         # religious participation
         ree_02,
         ree_03
         )

# Rename the extracted variables
gss_gvp <- gss_gvp %>% 
  clean_names() %>% 
  rename(# Demographic
         # age group
         age_group = dh1gage,
         # place of birth (Canada/macro-region)
         birth_place_can = brthcan,
         birth_place_other = brthmacr,
         # gender
         gender = gndr,
         # education (highest degree)
         education = dh1ged,
         # province of residence
         province = prv,
         # household size
         household_size = hsdsizec,
         
         # Giving
         # decisions on giving
         giving_decision_tax_credit = dg_005,
         giving_decision_dececide_in_advanced = dg_030,
         giving_decision_pattern = dg_050,
         giving_decision_search = dg_060,
         giving_decision_know_verify = dg_080,
         giving_decision_monitoring = dg_090,
         # financial giving
         fin_giving_mail = fg1a_030,
         fin_giving_telephone = fg1a_040,
         fin_giving_television = fg1a_050,
         fin_giving_online = fg1a_060,
         fin_giving_own = fg1a_070,
         fin_giving_charity = fg1a_080,
         fin_giving_in_memory_of = fg1a_090,
         fin_giving_work = fg1a_100,
         fin_giving_canvassing = fg1a_110,
         fin_giving_shopping_centre = fg1a_120,
         fin_giving_worship = fg1a_130,
         fin_giving_sponsoring_someone = fg1a_140,
         fin_giving_other = fg1a_170,
         # reasons for giving
         reason_giving_personal = rg_010,
         reason_giving_tax = rg_020,
         reason_giving_religious = rg_030,
         reason_giving_spiritual = rg_035,
         reason_giving_cause = rg_040,
         reason_giving_compassion = rg_050,
         reason_giving_community = rg_060,
         reason_giving_asked = rg_070,
         # reasons for not giving more
         reason_not_giving_already = ng_020,
         reason_not_giving_not_afford = ng_030,
         reason_not_giving_no_on_asked = ng_040,
         reason_not_giving_not_know_how = ng_050,
         reason_not_giving_no_cause = ng_060,
         reason_not_giving_gave_time_instead = ng_070,
         reason_not_giving_gave_directly = ng_080,
         reason_not_giving_no_tax_credit = ng_090,
         reason_not_giving_no_use_of_money = ng_110,
         reason_not_giving_too_many_org = ng_150,
         reason_not_giving_charity_fraud = ng_160,
         
         # Volunteering
         # how to volunteer
         volunteer_with_family = gv_020, # with family
         volunteer_with_friends = gv_030, # with friends
         # reasons for volunteering
         reason_volunteering_personal = rv_020,
         reason_volunteering_family = rv_025,
         reason_volunteering_friends = rv_030,
         reason_volunteering_network = rv_040,
         reason_volunteering_job = rv_050,
         reason_volunteering_religious = rv_060,
         reason_volunteering_explore_strength = rv_070,
         reason_volunteering_community = rv_080,
         reason_volunteering_use_skills = rv_090,
         reason_volunteering_support_cause = rv_100,
         reason_volunteering_improve_health = rv_110,
         # reasons for not volunteering more
         reason_not_volunteering_enough_time = nv_020,
         reason_not_volunteering_previous_experience = nv_030,
         reason_not_volunteering_no_one_asked = nv_040,
         reason_not_volunteering_not_know_how = nv_050,
         reason_not_volunteering_health = nv_060,
         reason_not_volunteering_no_time = nv_070,
         reason_not_volunteering_financial_cost = nv_080,
         reason_not_volunteering_commitment = nv_090,
         reason_not_volunteering_give_money = nv_100,
         reason_not_volunteering_no_interest = nv_110,
         reason_not_volunteering_no_opportunity = nv_120,
         reason_not_volunteering_no_meaning = nv_130,
         # main volunteer activities
         volunteer_canvassing = fv_020,
         volunteer_fundraising = fv_030,
         volunteer_teaching = fv_050,
         volunteer_coaching = fv_080,
         volunteer_councel = fv_090,
         volunteer_health = fv_100,
         volunteer_deliver_goods = fv_110,
         volunteer_maintenance = fv_120,
         volunteer_volunteer_driving = fv_130,
         volunteer_environment = fv_150,
         volunteer_others = fv_160,
         
         # Participating
         # youth experience
         youth_team_sport = ea_020,
         youth_youth_group = ea_030,
         youth_volunteer = ea_040,
         youth_saw_someone_helping = ea_050,
         youth_canvassing = ea_060,
         youth_student_government = ea_070,
         youth_religious_org = ea_080,
         youth_parents_volunteered = ea_090,
         # religious participation
         religious_participation_services = ree_02,
         religious_participation_own = ree_03
         ) 

# Clean age variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(age_group = case_when(
    age_group == '1' ~ '15-24 years',
    age_group == '2' ~ '25-34 years',
    age_group == '3' ~ '35-44 years',
    age_group == '4' ~ '45-54 years',
    age_group == '5' ~ '55-64 years',
    age_group == '6' ~ '65 years and older',
  ))

# Clean gender variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(gender = case_when(
    gender == '1' ~ 'Male',
    gender == '2' ~ 'Female',
  ))

# Clean education variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(education = case_when(
    education == '1' ~ 'Less than High School',
    education == '2' ~ 'Graduated from High school',
    education == '3' ~ 'Post-secondary diploma',
    education == '4' ~ 'University Diploma',
    education == '9' ~ 'Refusal',
  ))

# Clean province of resident variables and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(province = case_when(
    province == '10' ~ 'Newfoundland and Labrador',
    province == '11' ~ 'Prince Edward Island',
    province == '12' ~ 'Nova Scotia',
    province == '13' ~ 'New Brunswick',
    province == '24' ~ 'Quebec',
    province == '35' ~ 'Ontario',
    province == '46' ~ 'Manitoba',
    province == '47' ~ 'Saskatchewan',
    province == '48' ~ 'Alberta',
    province == '59' ~ 'British Columbia',
  ))

# Clean decision of giving variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(decision_of_giving = case_when(
    giving_decision_tax_credit == '1' ~ 'Tax Credit',
    giving_decision_dececide_in_advanced == '1' ~ 'Decide In Advance',
    giving_decision_pattern == '1' ~ 'Pattern of Giving',
    giving_decision_search == '1' ~ 'Search',
    giving_decision_know_verify == '1' ~ 'Know How to Verify',
    giving_decision_monitoring == '1' ~ 'Organizations that monitoring',
  ))

# Clean financial giving variable and and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(financial_giving = case_when(
    fin_giving_mail == '1' ~ 'Mail',
    fin_giving_telephone == '1' ~ 'Telephone',
    fin_giving_television == '1' ~ 'Television',
    fin_giving_online == '1' ~ 'Online',
    fin_giving_own == '1' ~ 'On Your Own Initiatives',
    fin_giving_charity == '1' ~ 'Charity Event',
    fin_giving_in_memory_of == '1' ~ 'In Memory of Someone',
    fin_giving_work == '1' ~ 'Work',
    fin_giving_canvassing == '1' ~ 'Door-to-door Canvassing',
    fin_giving_shopping_centre == '1' ~ 'Shopping Centre',
    fin_giving_worship == '1' ~ 'Worship',
    fin_giving_sponsoring_someone == '1' ~ 'By Sponsoring Someone',
    fin_giving_other == '1' ~ 'Other',
  ))

# Clean reason of giving variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(reason_of_giving = case_when(
    reason_giving_personal == '1' ~ 'Personally Affected',
    reason_giving_tax == '1' ~ 'Tax Credit',
    reason_giving_religious == '1' ~ 'Religious Reasons',
    reason_giving_spiritual == '1' ~ 'Spiritual or Other Beliefs',
    reason_giving_cause == '1' ~ 'Cause',
    reason_giving_compassion == '1' ~ 'Compassion',
    reason_giving_community == '1' ~ 'Community Contribution',
    reason_giving_asked == '1' ~ 'Asked by Someone You Know',
  ))

# Clean reason of not giving variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(reason_of_not_giving = case_when(
    reason_not_giving_already == '1' ~ 'Already Gave',
    reason_not_giving_not_afford == '1' ~ 'Could Not Afford A Larger Donation',
    reason_not_giving_no_on_asked == '1' ~ 'No One Asked',
    reason_not_giving_not_know_how == '1' ~ 'Did not know where to make donation',
    reason_not_giving_no_cause == '1' ~ 'Hard to Find a Cause',
    reason_not_giving_gave_time_instead == '1' ~ 'Gave Time Instead',
    reason_not_giving_gave_directly == '1' ~ 'Gave Directly To People',
    reason_not_giving_no_tax_credit == '1' ~ 'Tax Credit not Enough Incentives',
    reason_not_giving_no_use_of_money == '1' ~ 'Money would not be used efficiently',
    reason_not_giving_too_many_org == '1' ~ 'Too Many Organizations',
    reason_not_giving_charity_fraud == '1' ~ 'Charity Fraud',
  ))

# Clean volunteer with variable and convert numbers to categories in words
volunteer_with_cleaned <- gss_gvp %>%
  mutate(volunteer_with = case_when(
    volunteer_with_family == '1' ~ 'With Family',
    volunteer_with_friends == '1' ~ 'With Friends')) %>% 
  select(volunteer_with) %>% 
  pull()

gss_gvp <- gss_gvp %>% mutate(volunteer_with = volunteer_with_cleaned)

# Clean volunteer experience variable and convert numbers to categories in words
volunteer_experience_cleaned <- gss_gvp %>%
  mutate(volunteer_experience = case_when(
    volunteer_canvassing == '1' ~ 'Canvassing',
    volunteer_fundraising == '1' ~ 'Fundraising',
    volunteer_teaching == '1' ~ 'Teaching or Mentoring',
    volunteer_coaching == '1' ~ 'Coach, Referee, or Officiate',
    volunteer_councel == '1' ~ 'Counsel or Provide Advice',
    volunteer_health == '1' ~ 'Health Care or Support',
    volunteer_deliver_goods == '1' ~ 'Collect, Serve, or Deliver Goods',
    volunteer_maintenance == '1' ~ 'Maintenance, Repair, or Building',
    volunteer_volunteer_driving == '1' ~ 'Volunteer Driving',
    volunteer_environment == '1' ~ 'Protection of the Environment',
    volunteer_others == '1' ~ 'Other')) %>% 
  select(volunteer_experience) %>% 
  pull()

gss_gvp <- gss_gvp %>% mutate(volunteer_experience = volunteer_experience_cleaned)

# Clean reason for volunteering variable and convert numbers to categories in words
reason_for_volunteering_cleaned <- gss_gvp %>% 
  mutate(reason_for_volunteering = case_when(
    reason_volunteering_personal == '1' ~ 'Personally Affected',
    reason_volunteering_family == '1' ~ 'Family Member Volunteers',
    reason_volunteering_friends == '1' ~ 'Friend Volunteers',
    reason_volunteering_network == '1' ~ 'To Network',
    reason_volunteering_job == '1' ~ 'Job Opportunities',
    reason_volunteering_religious == '1' ~ 'Religious Reasons',
    reason_volunteering_explore_strength == '1' ~ 'To Explore Your Own Strengths',
    reason_volunteering_community == '1' ~ 'Community Contribution',
    reason_volunteering_use_skills == '1' ~ 'To Use Your Skills',
    reason_volunteering_support_cause == '1' ~ 'Support a cause',
    reason_volunteering_improve_health == '1' ~ 'Improve Health')) %>% 
  select(reason_for_volunteering) %>% 
  pull()

gss_gvp <- gss_gvp %>% mutate(reason_for_volunteering = reason_for_volunteering_cleaned)

# Clean reason for not volunteering variable and convert numbers to categories in words
reason_for_not_volunteering_cleaned <- gss_gvp %>% 
  mutate(reason_for_not_volunteering = case_when(
    reason_not_volunteering_enough_time == '1' ~ 'Gave Enough Time',
    reason_not_volunteering_previous_experience == '1' ~ 'Previous Experience',
    reason_not_volunteering_no_one_asked == '1' ~ 'No One Asked',
    reason_not_volunteering_not_know_how == '1' ~ 'Did not know how',
    reason_not_volunteering_health == '1' ~ 'Health Problems',
    reason_not_volunteering_no_time == '1' ~ 'No Time',
    reason_not_volunteering_financial_cost == '1' ~ 'Financial Cost',
    reason_not_volunteering_commitment == '1' ~ 'Unable to Make Commitment',
    reason_not_volunteering_give_money == '1' ~ 'Preferred to Give Money',
    reason_not_volunteering_no_interest == '1' ~ 'No Interest',
    reason_not_volunteering_no_opportunity == '1' ~ 'No Opportunity',
    reason_not_volunteering_no_meaning  == '1' ~ 'Not Meaningful')) %>% 
  select(reason_for_not_volunteering) %>% 
  pull()

gss_gvp <- gss_gvp %>% mutate(reason_for_not_volunteering = reason_for_not_volunteering_cleaned)

# Clean youth experience variable and convert numbers to categories in words
youth_experience_cleaned <- gss_gvp %>% 
  mutate(youth_experience = case_when(
    youth_team_sport == '1' ~ 'Team Sport',
    youth_youth_group == '1' ~ 'Youth Group',
    youth_volunteer == '1' ~ 'Volunteer Work',
    youth_saw_someone_helping == '1' ~ 'Saw Someone Helping',
    youth_canvassing == '1' ~ 'Door-to-door canvassing',
    youth_student_government == '1' ~ 'Student government',
    youth_religious_org == '1' ~ 'Religious organization',
    youth_parents_volunteered == '1' ~ 'Parents volunteered',
    TRUE~ "NA")) %>% 
  select(youth_experience) %>% 
  pull()

gss_gvp <- gss_gvp %>% mutate(youth_experience = youth_experience_cleaned)

# Clean religious participation service frequency variable and convert numbers to categories in words
gss_gvp <- gss_gvp %>%
  mutate(religious_participation_services = case_when(
    religious_participation_services == '1' ~ 'At least once a week',
    religious_participation_services == '2' ~ 'At least once a month',
    religious_participation_services == '3' ~ 'At least 3 times a year',
    religious_participation_services == '4' ~ 'Once or twice a year',
    religious_participation_services == '5' ~ 'Not at all',
    religious_participation_services == '9' ~ 'Not stated',
  ))

# Write the cleaned data to a file
write_csv(gss_gvp, "./outputs/data/gss_gvp.csv")

