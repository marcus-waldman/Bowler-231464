rm(list =ls())

library(tidyverse)
library(readxl)
library(writexl)
library(mice)
library(ggthemes)
library(ggtext)

 root_wd = "C:/Users/waldmanm/"
 onedrive_wd = file.path(root_wd,"The University of Colorado Denver", "Bowler, Fara - March 2023_FB BH SH")
 github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

#root_wd = "/Users/cristiansarabia/Library/CloudStorage"
#onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")

source(file.path(github_wd, "Code", "utils", "utils.R"))

#Load in the dictionary and raw dataset
dict = construct_data_dictionary(onedrive_wd=onedrive_wd)
raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))


# Get the skills analytic dataset
longdat = get_essential_environment_competence_longdat(raw, dict)


### Responses to Fara's email ##




#### Make Skills Consensus ####
essential_by_round = get_essential_by_round(longdat, round = T)
essential_skills = essential_by_round %>% dplyr::filter(`Yes (%)`>=80 | `No (%)`>=80) %>% dplyr::arrange(-`Yes (%)`) 


environment_by_round = get_environment_by_round(longdat)
competence_by_round = get_competence_by_round(longdat)

environment_met_consensus = essential_skills %>% 
  dplyr::select(-rq,-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join(environment_by_round %>% dplyr::select(rq,round,varshort,n,Mode,`Didactic (%)`,`Simulation (%)`,`Clinical (%)`), by = c("round","varshort")) 


competence_met_consensus =  essential_skills %>% 
  dplyr::select(-rq,-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join(get_competence_by_round(longdat) %>% dplyr::select(rq,round,varshort,n,Mode,`Yes (%)`, `No (%)`), by = c("round","varshort"))


# writexl::write_xlsx(list(essential_skills, environment_met_consensus, competence_met_consensus), path = file.path(onedrive_wd, "Meeting Memos", "2025-04-30 Follow-up", "Essential Skills - Environment and Competence - All-Participants.xlsx"))


essential_by_round_36 = get_essential_by_round(longdat %>% dplyr::filter(completer), round = T)
essential_met_consensus_36 = essential_skills %>% 
  dplyr::select(-rq,-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join(essential_by_round_36 %>% dplyr::select(rq,round,varshort,n,`Yes (%)`, `No (%)`), by = c("round","varshort"))


environment_met_consensus_36 = essential_skills %>% 
  dplyr::select(-rq,-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join( get_environment_by_round(longdat %>% dplyr::filter(completer)) %>% dplyr::select(rq,round,varshort,n,Mode,`Didactic (%)`,`Simulation (%)`,`Clinical (%)`), by = c("round","varshort")) 


competence_met_consensus_36 = essential_skills %>% 
  dplyr::select(-rq,-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join( get_competence_by_round(longdat %>% dplyr::filter(completer)) %>% dplyr::select(rq,round,varshort,n,Mode,`Yes (%)`, `No (%)`), by = c("round","varshort")) 



# writexl::write_xlsx(list(essential_met_consensus_36, environment_met_consensus_36, competence_met_consensus_36), path =  file.path(onedrive_wd, "Meeting Memos", "2025-04-30 Follow-up", "Essential Skills - Environment and Competence - Completers.xlsx"))


nonessential_skills = essential_by_round %>% 
  dplyr::filter(`Yes (%)` < 80 & `No (%)`< 80 & !(varshort %in% essential_skills$varshort)) %>% 
  dplyr::arrange(category,varshort) 

nonessential_skills = nonessential_skills %>% 
  dplyr::left_join(nonessential_skills %>% dplyr::group_by(skill) %>% dplyr::reframe(category = category[1], varshort = varshort[1]) %>% dplyr::ungroup() %>% dplyr::arrange(category,varshort) %>% dplyr::mutate(sid = 1:n()), by = c("category","skill","varshort")) %>% 
  dplyr::relocate(sid)


nonessential_skills36 = nonessential_skills %>% 
  dplyr::select(-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join(essential_by_round_36 %>% dplyr::select(round,varshort, n,`Yes (%)`, `No (%)`), by = c("round", "varshort"))

# writexl::write_xlsx(list(All = nonessential_skills, Completers = nonessential_skills36), path = file.path(onedrive_wd, "Meeting Memos", "2025-04-30 Follow-up", "Nonessential Skills.xlsx"))



# RQ1: Barplots
rq1 = essential_by_round %>% 
  dplyr::filter(skill_origin == "Original 166") %>%  
  dplyr::group_by(round) %>% 
  dplyr::reframe(n_yes = sum(`Yes (%)` >= 80, na.rm = T), n_no = sum(`No (%)` >= 80, na.rm = T)) %>% 
  tidyr::pivot_longer(n_yes:n_no, names_to = "result", values_to = "n") %>% 
  dplyr::bind_rows(data.frame(round=4,result="never",n = 166-97)) %>% 
  dplyr::mutate(
    x = factor(round,levels = 1:4, labels = c("1","2","3","Never")),
    Result = plyr::mapvalues(result, c("n_yes","n_no", "never"), c(2,1,3)) %>% factor(levels = 1:3, labels = c("Non-Essential", "Essential", "Non consensus"))
  ) %>% 
  dplyr::mutate(
    pct = round(100*n/166,1), 
    lbl = paste0(as.character(n), " (",pct,"%)")
  ) 


thm = ggthemes::theme_few(base_family = "serif", base_size = 16)
thm$legend.position = "inside"
thm$legend.direction = "horizontal" 
thm$legend.justification = c(.75,.99)
thm$plot.title = element_markdown(hjust = 0, size = 18)
thm$plot.subtitle = element_markdown(hjust = 0, size = 14)
thm$legend.text = element_text(size = 15)
thm$axis.text.x = element_text(size = 15)
thm$axis.text.y = element_text(size = 15)

library(ggtext)
plt_rq1 = ggplot(data = rq1, aes(x = x, y = n, col = Result, fill = Result)) + 
  geom_bar(stat="identity", position = "dodge", alpha = 2/3) +
  geom_text(aes(label=lbl), position=position_dodge(width=0.9), vjust=-0.25, size = 5.25) +
  labs(title = "<b>Essential Skills</b><br>Is this skill essential to be taught in pre-licensure nursing programs?", subtitle = "Totals (n=166): Non-Essential (n=2); Essential (n=95); Non consensus (n=69)", x = "Round", y = element_blank()) +
  ggthemes::scale_color_wsj(guide = "none") +
  ggthemes::scale_fill_wsj() +
  thm

plt_rq1
ggsave(plot = plt_rq1, filename = file.path(onedrive_wd, "Meeting Memos", "2025-05-07 Follow-up", "RQ1 Bar Chart.png"), height = 9, width = 9)


# RQ2: 
rq2_df = environment_by_round %>% dplyr::filter(skill_origin == "Original 166") %>% 
  dplyr::left_join(essential_skills %>% dplyr::filter(skill_origin == "Original 166", `Yes (%)` >= 80) %>% dplyr::select(varshort, round) %>% dplyr::mutate(keep = 1), by = c("round","varshort")) %>% 
  dplyr::filter(keep == 1) %>% 
  dplyr::group_by(Mode) %>% 
  dplyr::reframe(n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    pct = round(100*n/97,2), 
    lbl = paste0(as.character(n), " (",pct,"%)")
  ) 


# Add code here for plot!
thm$legend.position = "none"


plt_rq2 = ggplot(data = rq2_df, aes(x = Mode, y = n, col = Mode, fill = Mode)) +
  geom_bar(stat="identity", position = "dodge", alpha = 2/3) +
  geom_text(aes(label=lbl), position=position_dodge(width=0.9), vjust=-0.25, size = 5.25) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = '<b>Educational Environment</b><br>What is the best teaching enviornment for the psychomotor skill<sup>1</sup>?', subtitle = "*Note*: <sup>1</sup>n=95 Essential Skills only", y = element_blank()) +
  ggthemes::scale_color_wsj(guide = "none") +
  ggthemes::scale_fill_wsj() +
  thm

plt_rq2
ggsave(plot = plt_rq2, filename = file.path(onedrive_wd, "Meeting Memos", "2025-05-07 Follow-up", "rq2_plot.png"), height = 9, width = 9)


# RQ3: 

rq3_df = competence_by_round %>% dplyr::filter(skill_origin == "Original 166") %>% 
  dplyr::left_join(essential_skills %>% dplyr::filter(skill_origin == "Original 166", `Yes (%)` >= 80) %>% dplyr::select(varshort, round) %>% dplyr::mutate(keep = 1), by = c("round","varshort")) %>% 
  dplyr::filter(keep == 1) %>% 
  dplyr::group_by(Mode) %>% 
  dplyr::reframe(n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    pct = round(100*n/97,2), 
    lbl = paste0(as.character(n), " (",pct,"%)")
  ) 


# Add code here for plot!

plt_rq3 = ggplot(data = rq3_df, aes(x = Mode, y = n, col = Mode, fill = Mode)) +
  geom_bar(stat = 'identity', position = "dodge", alpha = 2/3) +
  geom_text(aes(label=lbl), position=position_dodge(width=0.9), vjust=-0.25, size = 5.25) +
  labs(title = '<b>Skill Competency</b><br>Does competence need to be evaluated for the psychomotor skill<sup>1</sup>?', subtitle = "*Note*: <sup>1</sup>n=95 Essential Skills only", y = element_blank()) +
  ggthemes::scale_color_wsj(guide = "none") +
  ggthemes::scale_fill_wsj() +
  thm

plt_rq3
ggsave(plot = plt_rq3, filename = file.path(onedrive_wd, "Meeting Memos", "2025-05-07 Follow-up", "rq3_plot.png"), height = 8.5, width = 8.5)



# # Is it accurate that out of the 9 Nutrition questions only 5 made it to the survey? 
# 
# foo = longdat %>% 
#   dplyr::group_by(round, varshort) %>% 
#   dplyr::reframe(n = n()) %>% 
#   tidyr::pivot_wider(names_from = "round", values_from = "n", names_prefix = "ro") %>% 
#   dplyr::left_join(varshort_skills(dict=dict), by = "varshort") %>% 
#   dplyr::relocate(skill:skill_origin, .after = varshort)
# 
# foo %>% dplyr::filter(skill_origin == "Omitted Nu Skills")
# 
# 
# #   varshort   skill                                                                            category  skill_origin        ro1   ro2   ro3
# #   <chr>      <chr>                                                                            <chr>     <chr>             <int> <int> <int>
# # 1 skill_n_10 Irrigating a feeding tube                                                        Nutrition Omitted Nu Skills    NA    14    12
# # 2 skill_n_3  Administering enteral feedings via nasoenteric, gastrostomy or jejunostomy tubes Nutrition Omitted Nu Skills    NA    14    NA
# # 3 skill_n_4  Administering peripheral parental nutrition with lipid                           Nutrition Omitted Nu Skills    NA    14    NA
# # 4 skill_n_7  Confirming placement of a nasogastric tube                                       Nutrition Omitted Nu Skills    NA    14    NA
# # 5 skill_n_8  Inserting a nasogastric tube                                                     Nutrition Omitted Nu Skills    NA    14    NA
# # 6 skill_n_9  Inserting and removing a small-bore nasoenteric tube for enteral feedings        Nutrition Omitted Nu Skills    NA    14    12
# # 7 skill_n_1  Administering central parental nutrition                                         Nutrition Omitted Nu Skills    NA    NA     2
# # 8 skill_n_12 Removing a nasogastric or nasoenteric tube                                       Nutrition Omitted Nu Skills    NA    NA     2
# # 9 skill_n_6  Assisting a patient with eating                                                  Nutrition Omitted Nu Skills    NA    NA     2
# 
# skills_nu = foo %>% dplyr::filter(skill_origin == "Omitted Nu Skills") %>% purrr::pluck("varshort") 
# skills_nu %in% essential_skills$varshort
# 
# # And out of the 5 questions the responses ranged from 2 – 14?
# 
# 
# 
# # Is it accurate that out of the 21 extra questions only 8 made it to the survey?
# 
# foo %>% dplyr::filter(skill_origin == "Respondant Provided") %>% print(n = 30)
# 
# #    varshort      skill                                                                                                       category skill_origin   ro1   ro2   ro3
# #    <chr>         <chr>                                                                                                       <chr>    <chr>        <int> <int> <int>
# #  1 skill_aic_4   Disinfecting surfaces and equipment properly                                                                Asepsis… Respondant …    NA     9     9
# #  2 skill_aic_5   Handling linen                                                                                              Asepsis… Respondant …    NA     9     9
# #  3 skill_aic_9   Preventing and responding to body fluid exposure                                                            Asepsis… Respondant …    NA     9    NA
# #  4 skill_am_1    Ambulating a patient with equipment (IV pole, oxygen tank, chest tube)                                      Activit… Respondant …    NA     9     9
# #  5 skill_am_13   Fall risk assessment/ get up and go test                                                                    Activit… Respondant …    NA     9    NA
# #  6 skill_am_15   Low & locked, side rails ups, head of bed elevation level adjustment                                        Activit… Respondant …    NA     9    NA
# #  7 skill_am_22   Utilizing a gait belt                                                                                       Activit… Respondant …    NA     9     9
# #  8 skill_am_6    Assisting a patient during and after a fall                                                                 Activit… Respondant …    NA     9     9
# #  9 skill_cc_2    Applying telemetry device                                                                                   Cardiov… Respondant …    NA     9     9
# # 10 skill_feabb_9 Performing I & O, daily weights                                                                             Fluid, … Respondant …    NA     9    NA
# # 11 skill_h_2     Caring for a female urinary collection device like a purewick.                                              Hygiene  Respondant …    NA     9     9
# # 12 skill_lsc_1   Obtaining a blood specimen from a central line                                                              Laborat… Respondant …    NA     9     9
# # 13 skill_ma_1    Accessing an implantable port                                                                               Medicat… Respondant …    NA     9     9
# # 14 skill_ma_28   Crushing and splitting tablets                                                                              Medicat… Respondant …    NA     9    NA
# # 15 skill_ma_33   Simulate/scanning software into medication administration process                                           Medicat… Respondant …    NA     9    NA
# # 16 skill_o_16    Performing peak flow testing                                                                                Oxygena… Respondant …    NA     9     9
# # 17 skill_o_22    Positioning for airway maintenance - elevating head of bed, turning to side (rescue position), stimulating… Oxygena… Respondant …    NA     9    NA
# # 18 skill_o_4     Applying High flow NC                                                                                       Oxygena… Respondant …    NA     9     9
# # 19 skill_siwc_11 Conducting wound photography                                                                                Skin In… Respondant …    NA     9     9
# # 20 skill_siwc_12 Packing a wound                                                                                             Skin In… Respondant …    NA     9     9
# # 21 skill_siwc_13 Performing a sterile dressing change.                                                                       Skin In… Respondant …    NA     9    NA
# 
# extra_skills = foo %>% dplyr::filter(skill_origin == "Respondant Provided") %>% print(n = 30) %>% purrr::pluck("varshort")
# 
# which(extra_skills %in% essential_skills$varshort)
# 
# 
# # I think that we have to take all Nutrition and Extra questions out. 
# # Is this an issue because the IRB protocol is that the extra questions would be added in Round 2.
# # 
# # 
# # 
# # We can report the 21 items that were identified by participants that are not found in textbooks.
# # 
# # 
# # 
# # How do we report that the Nutrition questions were erroneously not included?
# 
# 
# 
# 
# 
# 
# 
# 
# 
# longdat %>% dplyr::group_by(round, varshort) %>% 
#   dplyr::reframe()
# 
# 
# thm = theme_minimal()
# thm$legend.position = "left"
# cats = consensus_long$category %>% unique() %>% sort()
# for(cat_k in cats){
#   
#   pdf(file = file.path(onedrive_wd, "Meeting Memos","2025-04-23 Follow-up", paste0(stringr::str_replace_all(cat_k,"\\/","_"),".pdf")), height = 8, width = 11.5)
#   
#   skills_k = consensus_long %>% dplyr::filter(category==cat_k) %>% purrr::pluck("varshort") %>% unique() %>% sort() 
#   
#   for(a_skill in skills_k){
#     
#     skill_description =  consensus_long %>% dplyr::filter(category==cat_k, varshort == a_skill) %>% slice(1) %>% purrr::pluck("skill")
#     plt = ggplot(consensus_long %>% dplyr::filter(varshort == a_skill), aes(x = round, y = 100*agree_yes)) +
#       geom_ribbon(data = NULL, aes(xmin = 1, xmax = 3, ymin = 20, ymax = 80), col = "white", fill = "darkorange", alpha = .125) +
#       geom_hline(yintercept=100, size = 1) +
#       geom_hline(yintercept=0, size = 1) +
#       geom_point(size = 3, alpha = .8, col = "blue") + 
#       geom_line(linetype = 2, col = "blue") + 
#       coord_cartesian(ylim = c(0,100)) +
#       labs(title = skill_description, x = "Round", y = "Agreement (Yes vs. No)") + 
#       thm
#     print(plt)
#   }
#   
#   dev.off()
#   
#  
# }
# 
# 
# longdat_36 = longdat %>% dplyr::filter(completer)
# consensus_ro_1 = longdat %>% dplyr::filter(round==1) %>% dplyr::group_by(varshort) %>% dplyr::reframe(pct_yes_all = 100*mean(essential == "Yes")) %>% dplyr::arrange(-pct_yes_all)
# consensus36_ro_1 = longdat %>% dplyr::filter(round==1, completer) %>% dplyr::group_by(varshort) %>% dplyr::reframe(pct_yes_36 = 100*mean(essential == "Yes")) %>% dplyr::arrange(-pct_yes_36)
# writexl::write_xlsx(consensus_ro_1 %>% dplyr::left_join(consensus36_ro_1, by = "varshort"), file.path(onedrive_wd, "Meeting Memos","2025-04-23 Follow-up", "consensus_ro1_all_vs_36.xlsx"))
# 
# 
# 
# 
# 
# # Is it accurate that out of the 21 extra questions only 8 made it to the survey?
# 
# foo %>% dplyr::filter(skill_origin == "Respondant Provided") %>% print(n = 30)
# 
# #    varshort      skill                                                                                                       category skill_origin   ro1   ro2   ro3
# #    <chr>         <chr>                                                                                                       <chr>    <chr>        <int> <int> <int>
# #  1 skill_aic_4   Disinfecting surfaces and equipment properly                                                                Asepsis… Respondant …    NA     9     9
# #  2 skill_aic_5   Handling linen                                                                                              Asepsis… Respondant …    NA     9     9
# #  3 skill_aic_9   Preventing and responding to body fluid exposure                                                            Asepsis… Respondant …    NA     9    NA
# #  4 skill_am_1    Ambulating a patient with equipment (IV pole, oxygen tank, chest tube)                                      Activit… Respondant …    NA     9     9
# #  5 skill_am_13   Fall risk assessment/ get up and go test                                                                    Activit… Respondant …    NA     9    NA
# #  6 skill_am_15   Low & locked, side rails ups, head of bed elevation level adjustment                                        Activit… Respondant …    NA     9    NA
# #  7 skill_am_22   Utilizing a gait belt                                                                                       Activit… Respondant …    NA     9     9
# #  8 skill_am_6    Assisting a patient during and after a fall                                                                 Activit… Respondant …    NA     9     9
# #  9 skill_cc_2    Applying telemetry device                                                                                   Cardiov… Respondant …    NA     9     9
# # 10 skill_feabb_9 Performing I & O, daily weights                                                                             Fluid, … Respondant …    NA     9    NA
# # 11 skill_h_2     Caring for a female urinary collection device like a purewick.                                              Hygiene  Respondant …    NA     9     9
# # 12 skill_lsc_1   Obtaining a blood specimen from a central line                                                              Laborat… Respondant …    NA     9     9
# # 13 skill_ma_1    Accessing an implantable port                                                                               Medicat… Respondant …    NA     9     9
# # 14 skill_ma_28   Crushing and splitting tablets                                                                              Medicat… Respondant …    NA     9    NA
# # 15 skill_ma_33   Simulate/scanning software into medication administration process                                           Medicat… Respondant …    NA     9    NA
# # 16 skill_o_16    Performing peak flow testing                                                                                Oxygena… Respondant …    NA     9     9
# # 17 skill_o_22    Positioning for airway maintenance - elevating head of bed, turning to side (rescue position), stimulating… Oxygena… Respondant …    NA     9    NA
# # 18 skill_o_4     Applying High flow NC                                                                                       Oxygena… Respondant …    NA     9     9
# # 19 skill_siwc_11 Conducting wound photography                                                                                Skin In… Respondant …    NA     9     9
# # 20 skill_siwc_12 Packing a wound                                                                                             Skin In… Respondant …    NA     9     9
# # 21 skill_siwc_13 Performing a sterile dressing change.                                                                       Skin In… Respondant …    NA     9    NA
# 
# 
# # I think that we have to take all Nutrition and Extra questions out. 
# # Is this an issue because the IRB protocol is that the extra questions would be added in Round 2.
# # 
# # 
# # 
# # We can report the 21 items that were identified by participants that are not found in textbooks.
# # 
# # 
# # 
# # How do we report that the Nutrition questions were erroneously not included?
# 
# 
