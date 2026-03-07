library(gitcreds)
library(readxl)
library(tidyverse)
library(gt)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(scales)

#gitcreds_set()

getwd()
list.files('../')
cfmg <- read_xlsx('../GIZ_CFM Survey_clean_Final.xlsx',na = "NA")

cfmg <- cfmg |>
        mutate(
                survey_date = demo_information_survey_date,
                province = as_factor(demo_information_province_name),
                province = str_to_title(province),
                district = as_factor(demo_information_district_name),
                cfmg_name = as_factor(demo_information_demo_information_cfmg_name),
                interviewer = as_factor(demo_information_interviewer_name),
                interview_type = as_factor(demo_information_interview_type)
        )

cfmg <- cfmg |>
        mutate( dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "10_times","often","NA"),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "12 times (every_month)","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "12_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "12_times_at_least_every_month","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "12_times(every_month)","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "15_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "24_times","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "24_times (twice per month)","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "36_times","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "4 times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "4_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "5_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "6_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "7_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "8_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "a_ lot_of_times_i_cant_even_count","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "at_least_24_times (at_least_2_times_a_month)","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "comes_only_when_bcp_has_a_meeting","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "dfo_only_comes_when _there_is_a_program","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "district_forestry_office_has_no_program_for_cfm","never",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "dont_know","dont_know",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "many_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_ 5_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_5_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_ 3_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_3_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_30_times","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_4_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_5_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_5_times_but_always_called_when_there_is_a_need","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_6_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_6_tmes","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_than_7_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "more_that_5_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "never_0_times","never",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "never_0_times_in_7_years","never",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "numerous_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "often_several_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "once_1_time","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "only_comes_when_the_hfos_have_apprehended_a_charcoal_producer","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "rarely_involved","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "several_times","occasionally",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "the_group_hasnt_been_working_for_the_last_2_years_and_the_forest_officer_did_not_visit","never",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "thrice_3_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "twice_2_times","rarely",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "very_often","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "very_often (atleast_once_a_month)","very_often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "very_often /several_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "very_often/several_times","often",dfo_visits),
                dfo_visits = ifelse(cfmg_governance_dfo_visit_cfmg == "whenever_there_is_need","occasionally",dfo_visits),
                dfo_visits = factor(dfo_visits, levels = c("never","rarely","occasionally","often","very_often","dont_know"))
        )

cfmg <- cfmg |>
        mutate(
                interview_type = case_when(
                        interview_type == "cfmg" ~ paste(interview_type, demo_information_fgd_participants_fgd_gender, sep = "_"),
                        interview_type != "cfmg" ~ interview_type,
                        .default = interview_type
                )
                        
        )

# Unable to for the executive category unable to assign genders to age groups
# Assume that all youth on executive are male
cfmg <- cfmg |>
        mutate(
                senior_male = case_when(
                        interview_type == "cfmg_executive" ~ 
                                case_when(
                                        demo_information_fgd_executive_num_youth_001 == 0 ~ demo_information_fgd_executive_num_male_executive_members,
                                        demo_information_fgd_executive_num_youth_001 > 0 ~ demo_information_fgd_executive_num_male_executive_members - demo_information_fgd_executive_num_youth_001,
                                        .default = demo_information_fgd_executive_num_male_executive_members
                                        ),
                        interview_type == "cfmg_male" ~ demo_information_fgd_participants_num_adults,
                        interview_type == "cfmg_female" ~ 0,
                        interview_type == "cfmg_youth" ~ 0,
                        .default = NA
                ),
                senior_female = case_when(
                        interview_type == "cfmg_executive" ~ demo_information_fgd_executive_num_female_executive_members,
                        interview_type == "cfmg_male" ~ 0,
                        interview_type == "cfmg_female" ~ demo_information_fgd_participants_num_adults,
                        interview_type == "cfmg_youth" ~ 0,
                        .default = NA
                ),
                youth_male = case_when(
                        interview_type == "cfmg_executive" ~ demo_information_fgd_executive_num_youth_001,
                        interview_type == "cfmg_male" ~ demo_information_fgd_participants_num_youth,
                        interview_type == "cfmg_female" ~ 0,
                        interview_type == "cfmg_youth" ~ demo_information_youth_fgd_participants_youth_fgd_male_num,
                        .default = NA
                ),
                youth_female = case_when(
                        interview_type == "cfmg_executive" ~ 0,
                        interview_type == "cfmg_male" ~ 0,
                        interview_type == "cfmg_female" ~ demo_information_fgd_participants_num_youth,
                        interview_type == "cfmg_youth" ~ demo_information_youth_fgd_participants_youth_fgd_female_num,
                        .default = NA
                ),
        )


# Need to standardise the types of individual interviewee
cfmg <- cfmg |> 
        mutate(
                interviewee_role = fct_collapse(demo_information_interviewee_role,
                                                cfmg_exec_member = c("cfmg_executive_member",
                                                                     "cfmg_executive_committee_trustee",
                                                                     "cfmg_executive_secretary"),
                                                cfmg_member = c("cfmg_ordinary_member",
                                                                "committee_member",
                                                                "former_committee_member",
                                                                "honorary_forestry_officer",
                                                                "cooperative_chairman",
                                                                "beekeeping_chairperson",
                                                                "representative_of_forest_user_group"),
                                                trad_leader = c("chief_representative",
                                                                "sub_chief",
                                                                "headman",
                                                                "headwoman",
                                                                "village_representative"),
                                                dfo = c("district_forestry_officer",
                                                        "forestry_officer",
                                                        "forestry_extension_officer"),
                                                NGO_rep =c("representative_of_ngo_associated_with_cfm",
                                                           "forestry_technologist",
                                                           "forestry_technician",
                                                           "representative_of_carbon_trading_company"))
        )


save(cfmg, file = "cfmg.RData")
