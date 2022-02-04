# Analysis
backcheck_analysis_plan <- read.csv("input/analysis plan/backcheck_analysis_plan.csv")
backcheck_analysis <- analysis_func(df = backcheck_complete, ap = backcheck_analysis_plan)

backcheck_analysis <- plyr::join(backcheck_analysis,
                      backcheck_analysis_plan %>% 
                         select(question_label, Question = variable),
                       type = "left",
                       by = "Question",
                       match = "first"
) %>% 
  relocate(c(question_label, Question), .after = "Disaggregation_level") %>% 
  select(-repeat_for)

# comparison
backcheck_casids <- unique(backcheck_complete$caseid)
comparison <- rbind(
  backcheck_complete %>% 
    select(caseid, m2a, m2b, m2c, m2d, m2j, m2pboy, m2p, mob7, Q10cbsg5, KEY) %>% 
    mutate(source = "backcheck")
  ,
  df_complete_approved %>% 
    filter(caseid %in% backcheck_casids) %>% 
    select(caseid, m2a, m2b, m2c, m2d, m2j, m2pboy, m2p, mob7, Q10cbsg5, KEY) %>% 
    mutate(source = "actual_data")
)

comparison_long <- comparison %>% 
  pivot_longer(-c(caseid, source, KEY)) %>% 
  pivot_wider(-KEY, names_from = source, values_from = value) %>% 
  mutate(is_equal = case_when(
    ((backcheck == actual_data) | (is.na(backcheck) & is.na(actual_data))) ~ TRUE,
    TRUE ~ FALSE
  ))

comparison_wide <- comparison_long %>% 
  mutate(is_equal = as.character(is_equal)) %>% 
  pivot_longer(-c(caseid, name), names_to = "source") %>% 
  pivot_wider(names_from = c(name, source), values_from = value)



