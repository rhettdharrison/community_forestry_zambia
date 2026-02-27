library(gitcreds)
library(readxl)
library(tidyverse)
library(gt)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(scales)

gitcreds_set()

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

## Interest in expanding Community Forest
# Examine responses by role and CFMG

cfmg |> filter(interview_type != "individual_interview") |>
        mutate(
                interest_expanding_cfma = fct_collapse(interest_expanding_cfma,
                                                       do_not_know = c("do_not_know",
                                                                       "other"))
                
        ) |>
        filter(interest_expanding_cfma != "do_not_know") |>
        group_by(fgd_type,interest_expanding_cfma) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE)

# Roughly 50:50 among both female and male community members
# Executive members strongly want
# Youth do not want
test.data <- cfmg |> filter(interview_type != "individual_interview") |>
        mutate(
                interest_expanding_cfma = fct_collapse(interest_expanding_cfma,
                                                       do_not_know = c("do_not_know",
                                                                       "other"))
        ) |>
        filter(interest_expanding_cfma != "do_not_know")



M1 <- glmer(interest_expanding_cfma ~
                    fgd_type +
                    (1 | province_name/cfmg_name),
            family = binomial(link = "logit"),
            data = test.data
)

summary(M1)
emm.M1 <- emmeans(M1, specs = ~ fgd_type)
pairwise.M1 <- pairs(emm.M1, adjust = "Tukey")
summary(pairwise.M1)



cfmg |> filter(!is.na(interviewee_role)) |>
        group_by(interviewee_role, interest_expanding_cfma) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE)

cfmg |> filter(!is.na(interviewee_role)) |>
        mutate(
                interest_expanding_cfma = fct_collapse(interest_expanding_cfma,
                                                       do_not_know = c("do_not_know",
                                                                       "other"))
        ) |>
        group_by(interviewee_gender, interest_expanding_cfma) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE)


test.data <- cfmg |> filter(!is.na(interviewee_role)) |>
        mutate(
                interest_expanding_cfma = fct_collapse(interest_expanding_cfma,
                                                       do_not_know = c("do_not_know",
                                                                       "other"))
        ) |>
        filter(interest_expanding_cfma != "do_not_know")

M2 <- glmer(interest_expanding_cfma ~
                    interviewee_role +
                    interviewee_gender +
                    #interviewee_age +
                    (1 | province_name/cfmg_name),
            family = binomial(link = "logit"),
            data = test.data
)

summary(M2)
# Age not significant factor
# Ordinary cfmg members less like to want to expand CFMA
# Men less likely to want to expand CFMA compared to women

emm.M2 <- emmeans(M2, specs = ~ interviewee_role)
pairwise.M2 <- pairs(emm.M2, adjust = "Tukey")
summary(pairwise.M2)
# none significant

cfmg |> filter(interview_type == "cfmg_executive") |>
        group_by(province_name) |>
        summarise(
                CF_agreement = round(mean(community_forest_management_agreement), digits = 2),
                manage_plan = round(mean(forest_management_plan),digits = 2),
                license = round(mean(forest_user_rights_license_permit),digits = 2),
                bylaws = round(mean(bylaws_and_constitution),digits = 2),
                benefit_sharing = round(mean(benefit_sharing_agreement),digits = 2)
        ) |>
        gt()

cfmg |> filter(interview_type == "cfmg_executive") |>
        #group_by(province_name) |>
        gather(key = "plans", val = "yes",
               cfmg_workplan,
               cfmg_budget,
               cfma_business_plan,
               any_user_groups) |>
        count(plans, yes) |>
        spread(yes, n) |>
        gt(row_group_as_column = FALSE)

cfmg |> filter(interview_type == "cfmg_executive") |>
        summarise(
                timber = sum(timber, na.rm = TRUE),
                firewood = sum(firewood, na.rm = TRUE),
                charcoal = sum(charcoal, na.rm = TRUE),
                honey = sum(honey, na.rm = TRUE),
                mushrooms = sum(mushrooms, na.rm = TRUE),
                wild_fruits = sum(wild_fruits, na.rm = TRUE),
                caterpillars = sum(caterpillars, na.rm = TRUE),
                bamboo = sum(bamboo, na.rm = TRUE),
                medicines = sum(medicinal_plants, na.rm = TRUE),
                bushmeat = sum(bushmeat, na.rm = TRUE),
                #resins = sum(resins_gums, na.rm = TRUE),
                #thatching = sum(thatching_grass, na.rm = TRUE),
                #fibers = sum(fibers, na.rm = TRUE),
                eco_tourism = sum(eco_tourism, na.rm = TRUE),
                #carbon = sum(carbon_credits, na.rm = TRUE)
        ) |>
        pivot_longer(cols = 1:11, names_to = "value_chain", values_to = "# of cfmgs") |>
        gt() 

cfmg |> filter(interview_type == "cfmg_executive") |>
        filter(!is.na(dfo)) |>
        group_by(province_name,dfo) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE) 



#Institutional parter types
cfmg |> filter(interview_type == "cfmg_executive") |>
        summarise(
                national = sum(national_government_agencies_departments_institutions, na.rm = T),
                local = sum(local_government_authorities, na.rm = T),
                private = sum(private_sector_partners, na.rm = T),
                research = sum(research_institutions_universities, na.rm = T),
                cbo = sum(community_based_organizations, na.rm = T),
                international = sum(international_development_agencies, na.rm = T),
                conservation = sum(local_conservation_groups, na.rm = T)
        ) |>
        pivot_longer(cols = 1:7, names_to = "Partner type", values_to = "# of CFMGs") |>
        gt() |>
        tab_header(title = md("**Partner Types**"),
                   subtitle = md("Numbers of CFMGs working with different<br> categories of institutional partner")) |>
        opt_align_table_header(align = "left") |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


#Institutional partners
cfmg |> filter(interview_type == "cfmg_executive") |>
        mutate(
                institution_partner_name = fct_collapse(institution_partner_name,
                                                        COMACO = c("COMACO",
                                                                   "Forest Department  and COMACO"),
                                                        Forestry_Department = c("Forestry Department",
                                                                                "Forest Department under the ZIFLP project",
                                                                                "Department of National Parks and Wildlife, Forestry Department, MAg"),
                                                        Bio_Carbon_Partners = c("Bio Carbon Partners (BCP)",
                                                                                "South luangwa and Bio Carbon Partners (BCP)"))
        ) |>
        group_by(institution_partner_name) |>
        summarise(
                Obs = n()
        ) |>
        gt() |>
        tab_header(title = md("**Main Partners**"),
                   subtitle = md("Main partner arrangements for different CFMGs")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = institution_partner_name) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


#CFMG management
cfmg |> 
        mutate(
                Meetings = fct_collapse(cfmg_meetings,
                                        adhoc = c("only_when_needed_adhoc_meetings",
                                                  "dont_know",
                                                  "other"))
        ) |>
        group_by(Meetings) |>
        summarise(
                Obs = n()
        ) |>
        gt() |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Frequency of CFMG meetings")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Meetings) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Elections = fct_collapse(cfmg_held_election,
                                         unknown = c("do_not_know", "other"))
        ) |>
        group_by(fgd_type, Elections) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Frequency of CFMG elections")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Elections) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Elections = fct_collapse(last_cfmg_election_when,
                                         unknown = c("dont_know", "other"),
                                         Zero_2_years_ago = c("1_2_years_ago",
                                                              "less_than_1_year_ago")),
                Elections = factor(Elections, levels = c("Zero_2_years_ago","3_5_years_ago",
                                                         "more_than_5_years_ago","unknown"))
        ) |>
        filter(!is.na(Elections)) |>
        group_by(fgd_type, Elections) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Last held CFMG elections")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Elections) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Tenure = fct_collapse(cfmg_postions_tenure,
                                      dont_know = c("dont_know","other"))
        ) |>
        filter(!is.na(Tenure)) |>
        group_by(fgd_type, Tenure) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Length of tenure for executive positions")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Tenure) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Term_limits = fct_collapse(term_limits_cfmg_exec,
                                           dont_know = c("dont_know","other"),
                                           one_term = c("one_term_only"),
                                           two_terms = c("maximum_of_two_terms"),
                                           three_terms = c("maximum_of_three_terms"),
                                           no_term_limits = c("none")),
                Term_limits = factor(Term_limits, levels = c("one_term","two_terms","three_terms",
                                                             "no_term_limits","dont_know"))
        ) |>
        filter(!is.na(Term_limits)) |>
        group_by(fgd_type, Term_limits) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Term limits for executive positions")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Term_limits) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


# Employment of HFO
cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                HFO = fct_collapse(yn_employ_honorary_forest_officers,
                                   dont_know = c("do_not_know","other"))
        ) |>
        filter(!is.na(HFO)) |>
        filter(HFO != "dont_know") |>
        group_by(fgd_type, HFO) |>
        summarise(
                Obs = n()
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("CFMG employment of Honorary Forest Officers")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = HFO) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


cfmg |> filter(!is.na(fgd_type)) |>
        summarise(
                Forestry = sum(hfo_manager_forestry_department, na.rm = T),
                CFMG_exec = sum(hfo_manager_cfmg_executive_committee, na.rm = T),
                Chief = sum(hfo_manager_traditional_leadership, na.rm = T),
                NGO = sum(hfo_manager_ngo_supporting_forest_mgmt, na.rm = T),
                Local_govt = sum(hfo_manager_local_government, na.rm = T),
                Partner = sum(hfo_manager_other, na.rm = T)
        ) |>
        pivot_longer(cols = 1:6, names_to = "Manager", values_to = "Obs") |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("CFMG employment of Honorary Forest Officers")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Manager) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Receipt_book = fct_collapse(receipt_book_maintained,
                                            dont_know = c("do_not_know")),
                Bank_account = fct_collapse(bank_account,
                                            dont_know = c("do_not_know")),
                License_permits = fct_collapse(issue_licenses_permits,
                                               dont_know = c("do_not_know", "other"))
        ) |>
        pivot_longer(cols = c(Receipt_book,Bank_account,License_permits),
                     names_to = "Process", values_to = "Obs") |>
        group_by(fgd_type,Process) |>
        count(Obs) |>
        pivot_wider(id_cols = c(fgd_type,Obs),names_from = Process, values_from = n) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Compliance with CFMG processes")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Financial_report = fct_collapse(financial_reports_to_community,
                                                dont_know = c("do_not_know", "other")),
                Annual_report = fct_collapse(annual_report_to_community,
                                             dont_know = c("do_not_know", "other"))
        ) |>
        pivot_longer(cols = c(Financial_report, Annual_report),
                     names_to = "Process", values_to = "Obs") |>
        group_by(fgd_type,Process) |>
        count(Obs) |>
        pivot_wider(id_cols = c(fgd_type,Obs),names_from = Process, values_from = n) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**CFMG Management**"),
                   subtitle = md("Compliance with CFMG processes")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

#### Forest clearing; charcoal and fire
#Inside CFMA
cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Clearing = fct_collapse(cfm_reduced_forest_clearing_cfma,
                                        dont_know = c("dont_know"),
                                        no_change = c("no_change",
                                                      "no_there_has_never_been_clearing_for_agriculture"),
                                        no_increased = c("no_increase")),
                Charcoal = fct_collapse(cfm_reduced_charcoal_cfma,
                                        dont_know = c("dont_know", "other"),
                                        no_change = c("no_change","no_there_has_never_been_charcoal_production"),
                                        yes_moderate = c("yes_moderate_reduction"),
                                        yes_significant = c("yes_significant_reduction")),
                Fire = fct_collapse(fire_incidence_reduction_cfma,
                                    dont_know = c("dont_know", "other"),
                                    no_increased = c("no_incidents_have_increased"),
                                    yes_moderate = c("yes_moderate_reduction"),
                                    yes_significant = c("yes_significant_reduction"))
        ) |>
        pivot_longer(cols = c(Clearing,
                              Charcoal,
                              Fire),
                     names_to = "Process", values_to = "Obs") |>
        group_by(fgd_type,Process) |>
        count(Obs) |>
        pivot_wider(id_cols = c(fgd_type,Obs),names_from = Process, values_from = n) |>
        mutate(
                Charcoal = ifelse(is.na(Charcoal),0,Charcoal),
                Clearing = ifelse(is.na(Clearing),0,Clearing),
                Fire = ifelse(is.na(Fire),0,Fire)
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Sustainable Forest Management**"),
                   subtitle = md("Has there been a reduction in charcoal procuction,
                                 forest clearing <br>or fire inside the CFMA?")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Wildlife = fct_collapse(wildlife_increase,
                                        dont_know = c("dont_know","other"))
        ) |>
        
        group_by(fgd_type) |>
        count(Wildlife) |>
        #pivot_wider(id_cols = c(fgd_type,Obs),names_from = Process, values_from = n) |>
        #mutate(
        #        Wildlife = ifelse(is.na(Wildlife),0,Wildlife)
        #) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Sustainable Forest Management**"),
                   subtitle = md("Has there been an increase in widlife?")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = n) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Resources = fct_collapse(forest_resources_management,
                                         worse = c("no_management_has_worsened"),
                                         moderate_improvement = c("yes_moderate_improvement"),
                                         significant_improvement = c("yes_significant_improvement"))
        ) |>
        
        group_by(fgd_type) |>
        count(Resources) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Sustainable Forest Management**"),
                   subtitle = md("Has there been improvement in forest resource management?")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = n) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

#Outside CFMA

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Clearing = fct_collapse(forest_clearing_increased_outside_cfma,
                                        dont_know = c("dont_know", "other"),
                                        no_reduced = c("no_decrease")),
                Charcoal = fct_collapse(charcoal_production_increased_outside_cfma,
                                        dont_know = c("dont_know", "other"),
                                        yes_moderate = c("yes_moderate_increase"),
                                        yes_significant = c("yes_significant_increase"))
        ) |>
        pivot_longer(cols = c(Clearing,
                              Charcoal),
                     names_to = "Process", values_to = "Obs") |>
        group_by(fgd_type,Process) |>
        count(Obs) |>
        pivot_wider(id_cols = c(fgd_type,Obs),names_from = Process, values_from = n) |>
        mutate(
                Clearing = ifelse(is.na(Clearing),0,Clearing),
                Charcoal = ifelse(is.na(Charcoal),0,Charcoal)
        ) |>
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Sustainable Forest Management**"),
                   subtitle = md("Has there been an increase in charcoal production 
                                 <br>or forest clearence outside the CFMA?")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


#### Forest restoration

cfmg |> filter(!is.na(fgd_type)) |>
        group_by(fgd_type) |>
        summarise(
                Protection = sum(restoration_methods_applied_protection, na.rm = T),
                Planting = sum(restoration_methods_applied_planting, na.rm = T),
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Types of restoration**"),
                   subtitle = md("Different types of restoration applied")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = ) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Restoration = natural_forest_restoration,
                Plantation = planted_forest_establishment,
                Agroforestry = yn_agroforestry
        ) |>
        
        pivot_longer(cols = c(Restoration,
                              Plantation,
                              Agroforestry),
                     names_to = "Process", values_to = "Obs") |>
        group_by(fgd_type,Process) |>
        count(Obs) |>
        pivot_wider(id_cols = c(fgd_type,Obs),names_from = Process, values_from = n) |>
        filter(Obs != "do_not_know") |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Restoration management**"),
                   subtitle = md("Principle restoration types")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


# Agroforestry types
with(cfmg, table(main_reason_agroforestry))

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                why_agroforestry = fct_collapse(main_reason_agroforestry,
                                                dont_know = c("dont_know", "other"))
        ) |>
        filter(!is.na(why_agroforestry)) |>
        group_by(fgd_type,why_agroforestry) |>
        summarise(
                Obs = n()
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Agroforestry**"),
                   subtitle = md("Stated reasons for agroforetry")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

cfmg |> filter(!is.na(fgd_type) & fgd_type == "cfmg_executive:") |>
        summarise(
                Alley = sum(alley_cropping, na.rm = T),
                Silvopasture = sum(Silvopasture, na.rm = T),
                Wind_break = sum(wind_breaks, na.rm = T),
                Intercropping = sum(intercropping, na.rm = T),
                Home_garden = sum(home_gardens, na.rm = T),
                Boundary = sum(boundary_planting_woodlots, na.rm = T)
        ) |>
        pivot_longer(cols = 1:6, names_to = "Agroforestry_type", values_to = "Obs") |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Agroforestry Type**"),
                   subtitle = md("Types of agroforestry applied")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Agroforestry_type) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

## Forest resources
cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Tubers = ifelse(harvested_forest_resources_other == "Edible roots" |
                                        harvested_forest_resources_other == "Edible roots such as lusala" |
                                        harvested_forest_resources_other == "Edible roots/Chikanda" |
                                        harvested_forest_resources_other == "Lusala (wild tubers)" |
                                        harvested_forest_resources_other == "Lusala and munkoyo" |
                                        harvested_forest_resources_other == "Munkoyo" |
                                        harvested_forest_resources_other == "Munkoyo roots" |
                                        harvested_forest_resources_other == "Munkoyo, busala" |
                                        harvested_forest_resources_other == "Roots (Munkoyo)" |
                                        harvested_forest_resources_other == "Roots and tubers" |
                                        harvested_forest_resources_other == "Tubers" |
                                        harvested_forest_resources_other == "Wild tubers" |
                                        harvested_forest_resources_other == "Wild tubers ( lusala), munkoyo",
                                1,0),
                Poles = ifelse(harvested_forest_resources_other == "Poles" |
                                       harvested_forest_resources_other == "Poles for construction",
                               1,0),
                Chikanda = ifelse(harvested_forest_resources_other == "Chikanda" |
                                          harvested_forest_resources_other == "Edible roots/Chikanda" |
                                          harvested_forest_resources_other == "Mukoyo and Chikanda",
                                  1,0),
                harvested_forest_none = ifelse(harvested_forest_resources_other == "Community members not allowed to harvest anything from the forest" |
                                                       harvested_forest_resources_other == "The community  does not harvest anything." |
                                                       harvested_forest_none == 1,
                                               1,0)
        ) |>
        group_by(fgd_type) |>
        
        summarise(
                Timber = sum(harvested_forest_timber, na.rm = T),
                Firewood = sum(harvested_forest_firewood, na.rm = T),
                Charcoal = sum(harvested_forest_charcoal, na.rm = T),
                Honey = sum(harvested_forest_honey, na.rm = T),
                Mushrooms = sum(harvested_forest_mushrooms, na.rm = T),
                Fruit = sum(harvested_forest_wild_fruits, na.rm = T),
                Caterpillars = sum(harvested_forest_caterpillars, na.rm = T),
                Bamboo = sum(harvested_forest_bamboo, na.rm = T),
                Medicine = sum(harvested_forest_medicinal_plants, na.rm = T),
                Bushmeat = sum(harvested_forest_bushmeat, na.rm = T),
                Resins = sum(harvested_forest_resins_gums, na.rm = T),
                Thatch = sum(harvested_forest_thatching_grass, na.rm = T),
                Fiber = sum(harvested_forest_fibers, na.rm = T),
                Ecotourism = sum(harvested_forest_eco_tourism, na.rm = T),
                Carbon = sum(harvested_forest_carbon_credits, na.rm = T),
                Tubers = sum(Tubers, na.rm = T),
                Poles = sum(Poles, na.rm = T),
                Chikanda = sum(Chikanda, na.rm = T),
                Nothing = sum(harvested_forest_none, na.rm = T)
                
        ) |>
        
        gt() |>
        tab_header(title = md("**Forest Resources**"),
                   subtitle = md("Types of resources harvested from the forest")) |>
        opt_align_table_header(align = "left") |>
        # cols_align(align = "left", columns = ) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

## Economic benefits from CFMG

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Nothing = ifelse(economic_benefits_from_cfm_other == "CFMG yet to see or experience economic benefits" |
                                         economic_benefits_from_cfm_other == "Economic benefits have not been seen yet" |
                                         economic_benefits_from_cfm_other == "No economic benefit has been seen yet" |
                                         economic_benefits_from_cfm_other == "No economic benefits have been received so far" |
                                         economic_benefits_from_cfm_other == "The benef8ts have not yet been seen" |
                                         economic_benefits_from_cfm_other == "We have not seen any development so far we need awareness as a community to know what the cfmg is doing" |
                                         economic_benefits_from_cfm_other == "We haven't yet seen the benefits of the CFM" |
                                         economic_benefits_from_cfm_other == "Benefits are only seen among leaders of the CFMG",
                                 1,0),
                Livlihood_support = ifelse(economic_benefits_from_cfm_other == "Agricultural trainings and inputs" |
                                                   economic_benefits_from_cfm_other == "Alternative livelihoods" |
                                                   economic_benefits_from_cfm_other == "Alternative livelihoods goat keeping, beehives" |
                                                   economic_benefits_from_cfm_other == "Communities received iron sheets per household" |
                                                   economic_benefits_from_cfm_other == "Communities were given iron sheets" |
                                                   economic_benefits_from_cfm_other == "Farming inputs" |
                                                   economic_benefits_from_cfm_other == "Households given seven iron sheets, electrical fences to keep elephants away" |
                                                   economic_benefits_from_cfm_other == "Iron sheets were given to some households" |
                                                   economic_benefits_from_cfm_other == "Livelihood projects eg., housing, goat keeping, farming inputs, bee keeping" |
                                                   economic_benefits_from_cfm_other == "Through Agroforestry - some households are being paid K1000 for planting 100 trees",
                                           1,0),
                Income = ifelse(economic_benefits_from_cfm_other == "Animal safaris  payments" |
                                        economic_benefits_from_cfm_other == "Income from eco-charcoal production" |
                                        economic_benefits_from_cfm_other == "Income from sustainable charcoal production",
                                1,income_from_ecotourism_e_g_guided_tours_camping_fees),
                Ecosystem_service = ifelse(economic_benefits_from_cfm_other == "Improved the weather patterns the rains has improved" |
                                                   economic_benefits_from_cfm_other == "Rainfall pattern has been restored." |
                                                   economic_benefits_from_cfm_other == "Rainfall patterns are being restored" |
                                                   economic_benefits_from_cfm_other == "There is less deforestation which would give more carbon credit when sold",
                                           1,0)
        ) |>
        group_by(fgd_type) |>
        
        summarise(
                Access_HH = sum(improved_access_to_natural_resources_for_household_use_e_g_firewood_building_materials, na.rm = T),
                Access_sale = sum(improved_access_to_natural_resources_for_sale_e_g_timber_honey_mushrooms, na.rm = T),
                Employment = sum(employment_opportunities_e_g_forest_management_eco_tourism_forest_patrols, na.rm = T),
                Income = sum(Income, na.rm = T),
                Income_processing = sum(income_from_forest_product_processing_e_g_honey_bamboo, na.rm = T),
                Revenue = sum(revenue_from_forest_conservation_programs_e_g_carbon_credits, na.rm = T),
                Busuness_dev = sum(local_business_development_e_g_forest_based_crafts_small_scale_industries, na.rm = T),
                Market_links = sum(market_linkages_and_trade_opportunities_for_forest_products, na.rm = T),
                Land_value = sum(increased_land_value_due_to_improved_forest_management, na.rm = T),
                Infrastructure = sum(infrastructure_development_e_g_boreholes_roads_markets_processing_centres, na.rm = T),
                Livlihood_support = sum(Livlihood_support, na.rm = T),
                Ecosystem_service = sum(Ecosystem_service, na.rm = T),
                Nothing = sum(Nothing, na.rm = T)
                
        ) |>
        
        gt() |>
        tab_header(title = md("**Economic benefits**"),
                   subtitle = md("Types of economic benefits received from CFMG")) |>
        opt_align_table_header(align = "left") |>
        # cols_align(align = "left", columns = ) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

## Equitable benefits sharing

with(cfmg, table(benefits_equity_across_groups))

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Benefits_equitable = fct_collapse(benefits_equity_across_groups,
                                                  dont_know = c("do_not_know", "other"))
        ) |>
        filter(!is.na(Benefits_equitable)) |>
        group_by(fgd_type,Benefits_equitable) |>
        summarise(
                Obs = n()
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Benefit sharing**"),
                   subtitle = md("Are benefits equiably shared")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

## Employment opportunities

with(cfmg, table(yn_formal_employment_opportunities_created_cfm))

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Employment_opportunities = yn_formal_employment_opportunities_created_cfm
        ) |>
        filter(!is.na(Employment_opportunities)) |>
        group_by(fgd_type,Employment_opportunities) |>
        summarise(
                Obs = n()
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Employment opportunities**"),
                   subtitle = md("Has the CFMG created employment?")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

# Numbers employed

cfmg |> filter(!is.na(fgd_type)) |>
        group_by(fgd_type) |>
        
        summarise(
                Before = sum(num_employed_before_cfmg, na.rm = T),
                After = sum(num_employed_after_cfmg, na.rm = T),
                After_male = sum(males_employed_cfmg_full_time, na.rm = T),
                After_female = sum(females_employed_cfmg_full_time, na.rm = T),
                #After_male_y = sum(male_youth_employed_cfmg_full_time, na.rm = T),
                #After_female_y = sum(female_youth_employed_cfmg_full_time, na.rm = T)
        ) |>
        
        gt() |>
        tab_header(title = md("**Formal employment**"),
                   subtitle = md("Numbers of people employed (FTE) before and after CFMG formed")) |>
        opt_align_table_header(align = "left") |>
        # cols_align(align = "left", columns = ) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

# Numbers employed in differnt roles

cfmg |> filter(!is.na(fgd_type)) |>
        group_by(fgd_type) |>
        
        mutate(
                forest_rangers = sum(forest_rangers,community_scouts,biodiversity_monitors),
                Professional = sum(researchers, data_analysts, surveyors,marketing_and_sales_agents),
                Processing_NTFPs = sum(processing_plant_staff_e_g_honey_timber_charcoal, craftsmen_and_woodcarvers),
                Professional = ifelse(formal_employment_opportunities_from_cfm_other == "Book keeper" |
                                              formal_employment_opportunities_from_cfm_other == "Chiefdom Assistant" |
                                              formal_employment_opportunities_from_cfm_other == "Teachers" |
                                              formal_employment_opportunities_from_cfm_other == "Community teachers",
                                      1,Professional),
                forest_rangers = ifelse(formal_employment_opportunities_from_cfm_other == "Community scouts" |
                                                formal_employment_opportunities_from_cfm_other == "Community Scouts" |
                                                formal_employment_opportunities_from_cfm_other == "Cattle herders" |
                                                formal_employment_opportunities_from_cfm_other == "Cattle herders/Fire martials" |
                                                formal_employment_opportunities_from_cfm_other == "Fire fighters",
                                        1,forest_rangers),
                Guard = ifelse(formal_employment_opportunities_from_cfm_other == "guard" |
                                       formal_employment_opportunities_from_cfm_other == "Guard" |
                                       formal_employment_opportunities_from_cfm_other == "Security guard" |
                                       formal_employment_opportunities_from_cfm_other == "Driver" |
                                       formal_employment_opportunities_from_cfm_other == "Community teachers,driver,cleaner",
                               1,0),
                Miller = ifelse(formal_employment_opportunities_from_cfm_other == "Hammer mill operators" |
                                        formal_employment_opportunities_from_cfm_other == "Hammer Mill Operators" |
                                        formal_employment_opportunities_from_cfm_other == "Hummer millers" |
                                        formal_employment_opportunities_from_cfm_other == "Retail shop trade and hammer mills employees",
                                1,0), 
                Herder = ifelse(formal_employment_opportunities_from_cfm_other == "Cattle herders",
                                1,0), 
                
        ) |>
        
        summarise(
                Rangers = sum(forest_rangers, na.rm = T),
                HFO = sum(hfos, na.rm = T),
                Admin = sum(administrative_staff, na.rm = T),
                Professional = sum(Professional, na.rm = T),
                Guides = sum(eco_tourism_operators_e_g_tour_guides_lodge_staff, na.rm = T),
                Processing_NTFPs = sum(Processing_NTFPs, na.rm = T),
                Miller = sum(Miller, na.rm = T),
                Herder = sum(Herder, na.rm = T)
        ) |>
        
        gt() |>
        tab_header(title = md("**Formal employment**"),
                   subtitle = md("Numbers of people employed (FTE) in different roles")) |>
        opt_align_table_header(align = "left") |>
        # cols_align(align = "left", columns = ) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

# Formal employment opportunities
with(cfmg, table(formal_employment_opportunities))

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Employment_opportunities = fct_collapse(formal_employment_opportunities,
                                                        Community_scouts = c("Community Scouts",
                                                                             "Forest Rangers")
                )
        ) |>
        filter(!is.na(Employment_opportunities) & fgd_type == "cfmg_executive:") |>
        group_by(Employment_opportunities) |>
        summarise(
                Obs = n()
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Employment opportunities**"),
                   subtitle = md("Types of employment opportunity offered")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


# Formal employment opportunities
with(cfmg, table(formal_employment_opportunities))

cfmg |> filter(!is.na(fgd_type)) |>
        
        filter(!is.na(yn_informal_employment_opportunities_created_cfm) &
                       yn_informal_employment_opportunities_created_cfm != "do_not_know") |>
        mutate(
                Informal_jobs = yn_informal_employment_opportunities_created_cfm
        ) |>
        
        
        group_by(Informal_jobs) |>
        summarise(
                Obs = n()
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Informal employment**"),
                   subtitle = md("Have informal employment opportunities increased?")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

# Informal job types
with(cfmg, table(informal_employment_opportunities_created_cfm_other))

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                Casual_labour = ifelse(informal_employment_opportunities_created_cfm_other == "To erect the fences" |
                                               informal_employment_opportunities_created_cfm_other == "Casual labour" |
                                               informal_employment_opportunities_created_cfm_other == "Beacon election and signage placements" |
                                               informal_employment_opportunities_created_cfm_other == "Casual labour for construction" |
                                               informal_employment_opportunities_created_cfm_other == "Teaching grass and herbal  medicine",
                                       1,0),
                Fire_management = ifelse(informal_employment_opportunities_created_cfm_other == "Casual labor for early burning" |
                                                 informal_employment_opportunities_created_cfm_other == "Casual labour labour fire management" |
                                                 informal_employment_opportunities_created_cfm_other == "Data collectors and fire management, skilled casual labour" |
                                                 informal_employment_opportunities_created_cfm_other == "Fire break maintenance" |
                                                 informal_employment_opportunities_created_cfm_other == "Fire guard maintenance" |
                                                 informal_employment_opportunities_created_cfm_other == "Fire management" |
                                                 informal_employment_opportunities_created_cfm_other == "Fire Management" |
                                                 informal_employment_opportunities_created_cfm_other == "Fire managers" |
                                                 informal_employment_opportunities_created_cfm_other == "Fire monitoring" |
                                                 informal_employment_opportunities_created_cfm_other == "Forest Fire fighters",
                                         1,0),
                Firewood = sum(firewood_collectors_and_sellers, firewood_carriers_and_transporters),
                NTFPs = sum(non_timber_forest_product_ntfp_harvesters_e_g_honey_mushrooms_wild_fruits,
                            wood_carvers_and_sculptors,
                            basket_weavers_and_mat_makers,
                            handicraft_vendors
                ),
                Charcoal = ifelse(informal_employment_opportunities_created_cfm_other == "Tree cutters for charcoal production",
                                  1,0),
                Charcoal = sum(Charcoal, sustainable_charcoal_producers),
                Restoration = ifelse(informal_employment_opportunities_created_cfm_other == "Potfilling for planting gliricidia" |
                                             informal_employment_opportunities_created_cfm_other == "Potfilling for gliricidia serpium planting",
                                     1,0),
                Restoration = sum(Restoration,
                                  casual_laborers_for_tree_planting,
                                  nursery_assistants)
        ) |>
        group_by(fgd_type) |>
        summarise(
                Casual = sum(Casual_labour, na.rm = T),
                Fire = sum(Fire_management, na.rm = T),
                NTFPs = sum(NTFPs, na.rm = T),
                Firewood = sum(Firewood, na.rm = T),
                Charcoal = sum(Charcoal, na.rm = T),
                Restoration = sum(Restoration, na.rm = T)
        ) |>
        
        gt() |>
        tab_header(title = md("**Informal employment**"),
                   subtitle = md("Informal employment types")) |>
        opt_align_table_header(align = "left") |>
        # cols_align(align = "left", columns = ) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )


# Main income source from CFMG
with(cfmg, table(main_income_source_from_cfm))
with(cfmg, table(main_income_source_from_cfm_other))

cfmg |> filter(!is.na(fgd_type)) |>
        mutate(
                main_income_source_from_cfm = fct_collapse(main_income_source_from_cfm,
                                                           dont_know = c("dont_know",
                                                                         "other")),
                Main_income = as.character(main_income_source_from_cfm),
                Main_income = ifelse(main_income_source_from_cfm_other == "BioCarbon Partners",
                                     paste("carbon_credits"), Main_income),
                #                Main_income = ifelse(main_income_source_from_cfm_other == "Money from illegal charcoal producers as penalty" |
                #                                             main_income_source_from_cfm_other == "Law enforcement activities",
                #                                     paste("penalties"), Main_income),
                #                Main_income = ifelse(main_income_source_from_cfm_other == "Membership fee contributions" |
                #                                             main_income_source_from_cfm_other == "Community donations" |
                #                                             main_income_source_from_cfm_other == "Community contributions/donations",
                #                                     paste("community_fees"), Main_income),
                Main_income = as.factor(Main_income)
        ) |>
        filter(!is.na(Main_income)) |>
        group_by(Main_income) |>
        summarise(
                Obs = n()
        ) |>
        
        gt(row_group_as_column = TRUE) |>
        tab_header(title = md("**Main income**"),
                   subtitle = md("Income sources for CFMGs")) |>
        opt_align_table_header(align = "left") |>
        cols_align(align = "left", columns = Obs) |>
        tab_options(
                heading.subtitle.font.size = px(12),
                table_body.hlines.style = "none"
        )

