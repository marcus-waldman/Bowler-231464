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


longdat = get_essential_environment_competence_longdat(raw, dict)



consensus_long = lapply(1:3, function(ro){
  
  consensus_df = longdat %>% dplyr::filter(round == ro) %>% 
    dplyr::group_by(varshort) %>% 
    dplyr::reframe(agree_yes = mean(essential == "Yes"), n = length(essential))
  
  
  varshort_df = varshort_skills(dict=dict) %>% 
    dplyr::left_join(consensus_df, by = "varshort") %>% 
    dplyr::mutate(round = ro) %>% 
    dplyr::relocate(round)
  
  return(varshort_df)
  
}) %>% dplyr::bind_rows()

thm = theme_minimal()
thm$legend.position = "left"

cats = consensus_long$category %>% unique() %>% sort()
cat_k = "Asepsis and Infection Control"

pdf(file = "C:/Users/waldmanm/Downloads/skills_essential_over_rounds.pdf", height = 8, width = 11.5)
for(cat_k in cats){
  plt = ggplot(consensus_long %>% dplyr::filter(category==cat_k), aes(x = round, y = 100*agree_yes,  col = skill)) +
    geom_ribbon(data = NULL, aes(xmin = 1, xmax = 3, ymin = 20, ymax = 80), col = "white", fill = "darkorange", alpha = .125) +
    geom_hline(yintercept=100, size = 2) +
    geom_hline(yintercept=0, size = 2) +
    geom_point(size = 3, alpha = .8) + 
    geom_line(linetype = 2) + 
    coord_cartesian(ylim = c(0,100)) +
    labs(title = cat_k, x = "Round", y = "Agreement (Yes vs. No)") + 
    thm
  print(plt)
}
dev.off()

