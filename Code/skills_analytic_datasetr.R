rm(list =ls())

library(tidyverse)
library(readxl)
library(writexl)
library(mice)

root_wd = "C:/Users/waldmanm/"
onedrive_wd = file.path(root_wd,"The University of Colorado Denver", "Bowler, Fara - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

source(file.path(github_wd, "Code", "utils", "utils.R"))

#Load in the dictionary and raw dataset
dict = construct_data_dictionary(onedrive_wd=onedrive_wd)
raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))


# Get the skills analytic dataset
longdat = get_essential_environment_competence_longdat(raw, dict)



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


#writexl::write_xlsx(list(essential_skills, environment_met_consensus, competence_met_consensus), path = file.path(onedrive_wd, "Meeting Memos", "2025-04-30 Follow-up", "Essential Skills - Environment and Competence - All-Participants.xlsx"))


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



writexl::write_xlsx(list(essential_met_consensus_36, environment_met_consensus_36, competence_met_consensus_36), path =  file.path(onedrive_wd, "Meeting Memos", "2025-04-30 Follow-up", "Essential Skills - Environment and Competence - Completers.xlsx"))


nonessential_skills = essential_by_round %>% 
  dplyr::filter(`Yes (%)` < 80 & `No (%)`< 80 & !(varshort %in% essential_skills$varshort)) %>% 
  dplyr::arrange(category,varshort) 

nonessential_skills = nonessential_skills %>% 
  dplyr::left_join(nonessential_skills %>% dplyr::group_by(skill) %>% dplyr::reframe(category = category[1], varshort = varshort[1]) %>% dplyr::ungroup() %>% dplyr::arrange(category,varshort) %>% dplyr::mutate(sid = 1:n()), by = c("category","skill","varshort")) %>% 
  dplyr::relocate(sid)


nonessential_skills36 = nonessential_skills %>% 
  dplyr::select(-n, -`Yes (%)`, -`No (%)`) %>% 
  dplyr::left_join(essential_by_round_36 %>% dplyr::select(round,varshort, n,`Yes (%)`, `No (%)`), by = c("round", "varshort"))

writexl::write_xlsx(list(All = nonessential_skills, Completers = nonessential_skills36), path = file.path(onedrive_wd, "Meeting Memos", "2025-04-30 Follow-up", "Nonessential Skills.xlsx"))


longdat %>% dplyr::group_by(round, varshort) %>% 
  dplyr::reframe()


thm = theme_minimal()
thm$legend.position = "left"
cats = consensus_long$category %>% unique() %>% sort()
for(cat_k in cats){
  
  pdf(file = file.path(onedrive_wd, "Meeting Memos","2025-04-23 Follow-up", paste0(stringr::str_replace_all(cat_k,"\\/","_"),".pdf")), height = 8, width = 11.5)
  
  skills_k = consensus_long %>% dplyr::filter(category==cat_k) %>% purrr::pluck("varshort") %>% unique() %>% sort() 
  
  for(a_skill in skills_k){
    
    skill_description =  consensus_long %>% dplyr::filter(category==cat_k, varshort == a_skill) %>% slice(1) %>% purrr::pluck("skill")
    plt = ggplot(consensus_long %>% dplyr::filter(varshort == a_skill), aes(x = round, y = 100*agree_yes)) +
      geom_ribbon(data = NULL, aes(xmin = 1, xmax = 3, ymin = 20, ymax = 80), col = "white", fill = "darkorange", alpha = .125) +
      geom_hline(yintercept=100, size = 1) +
      geom_hline(yintercept=0, size = 1) +
      geom_point(size = 3, alpha = .8, col = "blue") + 
      geom_line(linetype = 2, col = "blue") + 
      coord_cartesian(ylim = c(0,100)) +
      labs(title = skill_description, x = "Round", y = "Agreement (Yes vs. No)") + 
      thm
    print(plt)
  }
  
  dev.off()
  
 
}


longdat_36 = longdat %>% dplyr::filter(completer)
consensus_ro_1 = longdat %>% dplyr::filter(round==1) %>% dplyr::group_by(varshort) %>% dplyr::reframe(pct_yes_all = 100*mean(essential == "Yes")) %>% dplyr::arrange(-pct_yes_all)
consensus36_ro_1 = longdat %>% dplyr::filter(round==1, completer) %>% dplyr::group_by(varshort) %>% dplyr::reframe(pct_yes_36 = 100*mean(essential == "Yes")) %>% dplyr::arrange(-pct_yes_36)
writexl::write_xlsx(consensus_ro_1 %>% dplyr::left_join(consensus36_ro_1, by = "varshort"), file.path(onedrive_wd, "Meeting Memos","2025-04-23 Follow-up", "consensus_ro1_all_vs_36.xlsx"))

