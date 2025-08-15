



x = "role_primary"


dat2_x = get_dat2(dat, x)

lrt_results_role_primary = lrt_table$`_data` %>% dplyr::filter(Expert_Characteristic == names(covariate_name_map())[covariate_name_map()==x])


# ... overall (grand mean)
 overall_agg_role_primary = dat2_role_primary %>% 
   dplyr::select(name:essential, role_primary) %>% 
   dplyr::group_by(role_primary) %>% 
   dplyr::summarise(Overall = mean(essential, na.rm=T))
 
 lrt_M0_vs_M1 = data.frame(lrt_overall = with(lrt_results_role_primary, paste0("Chi2", "(", M0_vs_M1_df ,") = ", round(M0_vs_M1_ChiSq,2), ", p-adj = ", M0_vs_M1_p)))
 
 overall_agg_role_primary = lrt_M0_vs_M1 %>% dplyr::bind_rows(overall_agg_role_primary)  %>% dplyr::relocate(lrt_overall, .after = x)
 
 # ... by skill category
category_agg_role_primary = dat2_role_primary %>% 
   dplyr::select(name:essential, role_primary) %>% 
   dplyr::group_by(category, role_primary) %>% 
   dplyr::reframe(essential = mean(essential, na.rm = T)) %>% 
   tidyr::pivot_wider(names_from= "category",values_from = "essential") 


lrt_M1_vs_M2 = data.frame(lrt_categories = with(lrt_results_role_primary, paste0("Chi2", "(", M1_vs_M2_df ,") = ", round(M1_vs_M2_ChiSq,2), ", p-adj = ", M1_vs_M2_p)))
category_agg_role_primary = lrt_M1_vs_M2 %>% dplyr::bind_rows(category_agg_role_primary) %>% dplyr::relocate(lrt_categories, .after = x)

# ... now combine them
table_role_primary = overall_agg_role_primary %>% dplyr::left_join(category_agg_role_primary, by = x) %>% 
  dplyr::mutate(across(where(is.numeric), function(x){round(x,2) %>% as.character() %>% stringr::str_replace_all("0.", ".") %>% stringr::str_pad(3,"right", pad = "0") }))

# ...now append with counts
counts_role_primary = dat2_role_primary %>% 
  dplyr::group_by(role_primary) %>% 
  dplyr::summarise(n = length(unique(name))) 

table_role_primary %>% dplyr::left_join(counts_role_primary, by =x) %>% dplyr::relocate(n, .after = x)
