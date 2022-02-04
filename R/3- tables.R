# Analysis
df_complete_approved <- df_complete_approved %>% 
  mutate(
    Q10cbsg1b = as.numeric(Q10cbsg1b),
    Q10cbsg88 = as.numeric(Q10cbsg88),
    Q10cbsg89 = as.numeric(Q10cbsg89)
  )

analysis_plan <- read.csv("input/analysis plan/analysis_plan.csv")
analysis <- analysis_func(df = df_complete_approved, ap = analysis_plan)

analysis <- plyr::join(analysis,
           analysis_plan %>% 
             select(question_label, Question = variable),
           type = "left",
           by = "Question",
           match = "first"
           ) %>% 
  relocate(c(question_label, Question), .after = "Disaggregation_level") %>% 
  select(-repeat_for)

# Decision making
decison_making <- df_complete_approved %>%
  filter(m2a == "Myself") %>% 
  select(caseid, m2a, Q10h2:Q10h9, KEY) %>% 
  mutate(
    Q10h2 = ifelse(Q10h2 == "Never consulted", 1, 0),
    Q10h3 = ifelse(Q10h3 == "Never consulted", 1, 0),
    Q10h4 = ifelse(Q10h4 == "Never consulted", 1, 0),
    Q10h5 = ifelse(Q10h5 == "Never consulted", 1, 0),
    Q10h6 = ifelse(Q10h6 == "Never consulted", 1, 0),
    Q10h7 = ifelse(Q10h7 == "Never consulted", 1, 0),
    Q10h8 = ifelse(Q10h8 == "Never consulted", 1, 0),
    Q10h9 = ifelse(Q10h9 == "Never consulted", 1, 0)
  ) %>% 
  mutate(decison_making_score = rowSums(select(., Q10h2:Q10h9))) %>% 
  select(-c(Q10h2:Q10h9)) %>% 
  left_join(df_complete_approved %>% select(caseid, KEY, Q10h2:Q10h9), by = c("caseid", "KEY")) %>% 
  relocate(decison_making_score, .after = Q10h9) %>% 
  arrange(-decison_making_score)

# Respondents who have been relocated
relocated <- df_complete_approved %>% 
  filter(m2j == "No") %>% 
  select(caseid, database_province, database_district, Province, District, "When did you move?" = m2k, "Why did you move?" = m2m, "Why did you move?_Other" = m2m_other, KEY)

# attempts vs. completed interviews
attempts_vs_complete_interviews <- df %>%
  mutate(phone_response_short = ifelse(phone_response_short == "Complete", "Completed", "Not completed")) %>% 
  group_by(
    province = database_province,
    district = database_district
  ) %>%
  count(q= phone_response_short) %>% 
  mutate("Total attempt" = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = q, values_from = n) %>% 
  mutate(across(c("Completed", "Not completed"),
                function(x)
                  x = ifelse(is.na(x), 0, x)
  ))

#### export tables -----------




