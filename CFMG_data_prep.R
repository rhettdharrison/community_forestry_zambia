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


# Governance index (out of 10) from executive responses; joined to all rows
gov_index_data <- cfmg |>
        filter(interview_type == "cfmg_executive") |>
        mutate(
                gi_receipt_book  = case_match(cfmg_governance_receipt_book_maintained,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_),
                gi_bank_acc      = case_match(cfmg_governance_bank_account,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_),
                gi_financial_rep = case_match(cfmg_governance_financial_reports_to_community,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_),
                gi_annual_rep    = case_match(cfmg_governance_annual_report_to_community,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_),
                gi_permits       = case_match(cfmg_governance_issue_licenses_permits,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_),
                gi_hfo           = case_match(cfmg_governance_yn_employ_honorary_forest_officers,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_),
                gi_workplan      = case_match(cfmg_governance_cfmg_workplan,
                                              "yes" ~ 1, "no" ~ 0, "don't_know" ~ NA_real_,
                                              .default = NA_real_),
                gi_budget        = case_match(cfmg_governance_cfmg_budget,
                                              "yes" ~ 1, "no" ~ 0, "don't_know" ~ NA_real_,
                                              .default = NA_real_),
                gi_business_plan = case_match(cfmg_governance_cfma_business_plan,
                                              "yes" ~ 1, "no" ~ 0, "don't_know" ~ NA_real_,
                                              .default = NA_real_),
                gi_term_limits   = case_match(cfmg_governance_term_limits_exist,
                                              "yes" ~ 1, "no" ~ 0, .default = NA_real_)
        ) |>
        mutate(
                gov_index = rowSums(
                        cbind(gi_receipt_book, gi_bank_acc, gi_financial_rep,
                              gi_annual_rep, gi_permits, gi_hfo,
                              gi_workplan, gi_budget, gi_business_plan,
                              gi_term_limits),
                        na.rm = TRUE
                )
        ) |>
        select(cfmg_name, gov_index,
               gi_receipt_book, gi_bank_acc, gi_financial_rep, gi_annual_rep,
               gi_permits, gi_hfo, gi_workplan, gi_budget, gi_business_plan,
               gi_term_limits) |>
        group_by(cfmg_name) |>
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
        mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))

cfmg <- cfmg |>
        left_join(gov_index_data, by = "cfmg_name", relationship = "many-to-one")

# SFM index (out of 5) averaged across all FGD types (executive, male, female, youth); joined to all rows
sfm_index_data <- cfmg |>
        filter(interview_type %in% c("cfmg_executive", "cfmg_male", "cfmg_female", "cfmg_youth")) |>
        mutate(
                si_deforest = case_match(
                        impact_on_sfm_cfm_reduced_forest_clearing_cfma,
                        "yes_significant" ~ 1,
                        "yes_moderate"    ~ 1,
                        "Yes there no more fields_  it's all forests"                          ~ 1,
                        "Yes_ significant reduction. There was chitemene system before CFM"    ~ 1,
                        "no_increase"                                     ~ 0,
                        "no_change"                                       ~ 0,
                        "no_there_has_never_been_clearing_for_agriculture" ~ 0,
                        "Agricultural practices ceased way before CFM"    ~ 0,
                        "One field for cassava"                           ~ 0,
                        "dont_know"                                       ~ NA_real_,
                        .default = NA_real_
                ),
                si_charcoal = case_match(
                        impact_on_sfm_charcoal_production_in_cfma,
                        "yes_high"    ~ 1,
                        "no"          ~ 1,
                        "no_stopped"  ~ 1,
                        "yes_reduced" ~ 1,
                        "No_ charcoal production is not a common practice for community members" ~ 0,
                        "There has never been  charcoal  production"                             ~ 0,
                        "dont_know"                                                              ~ NA_real_,
                        .default = NA_real_
                ),
                si_fire = case_match(
                        impact_on_sfm_fire_incidence_reduction_cfma,
                        "yes_significant_reduction" ~ 1,
                        "yes_moderate_reduction"    ~ 1,
                        "no_change"                                          ~ 0,
                        "no_incidents_have_increased"                        ~ 0,
                        "No change observed as at now"                       ~ 0,
                        "CFMG activities just starting. To be observed this year." ~ 0,
                        "Fire management team only recently received training"     ~ 0,
                        "Fire monitoring just started this year"                   ~ 0,
                        "dont_know"                                                ~ NA_real_,
                        .default = NA_real_
                ),
                si_wildlife = case_match(
                        impact_on_sfm_wildlife_increase,
                        "yes_significant_increase"  ~ 1,
                        "yes_moderate_increase"     ~ 1,
                        "Yes_ moderate increase for some species such as bush pig and antelope"                             ~ 1,
                        "Yes_ moderate increase. Impala_ waterbuck_ wild pigs_ guinea fowls can now ge seen in the community forest" ~ 1,
                        "Yes_ moderate increase. Not one goes to the forest because of distancing thus animals are freely multiplying" ~ 1,
                        "Yes_ significant increase. Leading to an increase in human an wildlife conflicts"  ~ 1,
                        "Yes_ significant increase. The CFMA is in the GMA"                                 ~ 1,
                        "Yes_ significant increase. Wild animals now coming closer to households"            ~ 1,
                        "no_change"                                          ~ 0,
                        "no_reduced"                                         ~ 0,
                        "No wild animals recorded"                           ~ 0,
                        "Other wildlife has decreased_ only the hyna population is on the increase" ~ 0,
                        "There  are no wildlife animals"                     ~ 0,
                        "There has never been  any wildlife recorded"        ~ 0,
                        "There has never been wild animals in this community" ~ 0,
                        "There is no wildlife in this community"             ~ 0,
                        "dont_know"                                          ~ NA_real_,
                        .default = NA_real_
                ),
                si_nrm = case_match(
                        impact_on_sfm_forest_resources_management,
                        "yes_significant_improvement" ~ 1,
                        "yes_moderate_improvement"    ~ 1,
                        "no_change"                   ~ 0,
                        "no_management_has_worsened"  ~ 0,
                        "dont_know"                   ~ NA_real_,
                        .default = NA_real_
                )
        ) |>
        mutate(
                sfm_index = rowSums(
                        cbind(si_deforest, si_charcoal, si_fire, si_wildlife, si_nrm),
                        na.rm = TRUE
                )
        ) |>
        select(cfmg_name, sfm_index,
               si_deforest, si_charcoal, si_fire, si_wildlife, si_nrm) |>
        group_by(cfmg_name) |>
        summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
        mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))

cfmg <- cfmg |>
        left_join(sfm_index_data, by = "cfmg_name", relationship = "many-to-one")

save(cfmg, file = "cfmg.RData")
