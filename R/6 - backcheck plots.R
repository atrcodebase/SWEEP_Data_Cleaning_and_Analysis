comparison_table <- comparison_wide %>% 
  select(contains("is_equal")) %>% 
  lapply(function(is_equal)
    table(is_equal) %>% data.frame() %>% mutate(percent = round(Freq/sum(Freq)*100, 1))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>% 
  mutate(question = case_when(
    question == "m2a_is_equal" ~ "Who is the head of your household?",
    question == "m2b_is_equal" ~ "Is the head of the household currently living with you?",
    question == "m2c_is_equal" ~ "When did the head of the household move?",
    question == "m2d_is_equal" ~ "Why did the head of the household leave?",
    question == "m2j_is_equal" ~ "Are you currently living in database_province/database_district?",
    question == "m2pboy_is_equal" ~ "Are boys in your household able to attend primary school?",
    question == "m2p_is_equal" ~ "Are girls in your household able to attend primary school?",
    question == "mob7_is_equal" ~ "In your village, over the last 30 days: Are women able to leave the village (with Maharam) and go to health clinics in the locality or district center?",
    question == "Q10cbsg5_is_equal" ~ "Over the last 30 days, are you usually accompanied by any other person when you go out of your compound/dwelling?",
    TRUE ~ question
  ))

comparison_table %>% 
  mutate(ord = ifelse(is_equal == TRUE, 1, 2)) %>% 
  mutate(question = str_wrap(question, 20)) %>% 
  ggplot(aes(x = question, y = percent, fill = reorder(is_equal, ord))) +
  geom_col() +
  geom_text(aes(label = glue::glue("{round(percent)}% (n={Freq})")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("#009076", "#f0b505")) +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 7)) +
  labs(x = NULL, y = "Percent", fill = "Is equal?")

ggsave(glue::glue("{output_path_backcheck}comparison_of_backcheck_and_actual_data.png"), width = 10, height = 6)




