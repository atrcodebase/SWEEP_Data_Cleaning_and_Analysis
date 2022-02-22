# Module 1
df_complete_approved %>% 
  count(q = m2a) %>% 
  mutate(percent = round(n/sum(n)*100)) %>% 
  ggplot(aes(x = reorder(q, -n), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={n})")), vjust = -0.1) +
  scale_y_continuous(expand = c(0, 5)) +
  labs(x = NULL, y = "Percent")

ggsave(glue::glue("{output_path_graphs}module1_m2a.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2b, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#f0b505", "#009076")) +
  labs(title = "Is the head of the household currently living with you?")

ggsave(glue::glue("{output_path_graphs}module1_m2b.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2c, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505")) +
  labs(title = "When did the head of the household move?")

ggsave(glue::glue("{output_path_graphs}module1_m2c.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2e, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#f0b505", "#009076", "#f0b505")) +
  labs(title = "Has any other household member left your household in the last 30 days?")

ggsave(glue::glue("{output_path_graphs}module1_m2e.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(m2f, m2g) %>% 
  pivot_longer(everything()) %>% 
  filter(value != 0) %>% 
  mutate(name = case_when(
    name == "m2f" ~ "Males",
    name == "m2g" ~ "Females",
    TRUE ~ name
  )) %>% 
  mutate(name = str_wrap(name, 25)) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_boxplot(show.legend = F, alpha = 0.25) +
  scale_fill_manual(values = c("#f0b505", "#009076")) +
  scale_y_continuous(breaks = c(0:10)) +
  labs(x = NULL, y = "# HH members", title = "Number of household members left the household in the last 30 days")

# ggsave(glue::glue("{output_path_graphs}module1_m2f & m2g.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2h, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#5a02ad", "#009076", "#f0b505")) +
  labs(title = "Did any new member moved in your household in the last 30 days?")

ggsave(glue::glue("{output_path_graphs}module1_m2h.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("m2i_"), -m2i_other) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>% 
  filter(x == 1) %>% 
  mutate(ord = as.numeric(gsub(".*_", "", question))) %>% 
  mutate(question = case_when(
    question == "m2i_1" ~ "My extended family",
    question == "m2i_2" ~ "My husband’s extended family",
    question == "m2i_3" ~ "Other mothers with their children",
    question == "m2i_666" ~ "Other",
    question == "m2i_98" ~ "Don’t know",
    question == "m2i_99" ~ "Refused to answer",
    TRUE ~ question
  )) %>% 
  ggplot(aes(x = reorder(question, ord), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.1, size = 3) +
  labs(x = NULL, y = "Percent", title = "Who else moved in?") +
  scale_y_continuous(expand = c(0, 5))

ggsave(glue::glue("{output_path_graphs}module1_m2i.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2j, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5),
  ) +
  scale_fill_manual(values = c("#f0b505", "#009076")) +
  labs(title = "Are you currently living in [database_province] - [database_district]?")

ggsave(glue::glue("{output_path_graphs}module1_m2j.png"), width = 10, height = 6)

df_complete_approved %>% 
  group_by(Province, District) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(District) %>% 
  mutate(duplicate = n()) %>% 
  mutate(District = ifelse(duplicate > 1, glue::glue("{District}\n({Province})"), District)) %>% 
  ggplot(aes(x = reorder(District, -n), y = n, fill = Province)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.1) +
  labs(x = NULL, y = "Frequency", fill = NULL, title = "Current location") +
  theme(legend.position = "bottom") +
  # scale_y_continuous(expand = c(0, 5)) +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505", "#5a02ad", "#04bdb0", "#02631e", "#e86f05"))

ggsave(glue::glue("{output_path_graphs}module1_current location.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2k, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5),
  ) +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505")) +
  labs(title = "When did you move?")

ggsave(glue::glue("{output_path_graphs}module1_m2k.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m2l, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{round(Percent)}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505")) +
  labs(title = "Where are you currently living?")

ggsave(glue::glue("{output_path_graphs}module1_m2l.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("m2m_"), -m2m_other) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>% 
  filter(x != 0) %>%
  mutate(ord = as.numeric(str_remove(question, ".*_"))) %>% 
  mutate(question = case_when(
    question == "m2m_1" ~ "Find work / better work",
    question == "m2m_2" ~ "Security problem/Safe",
    question == "m2m_3" ~ "Refugee / Return from immigration",
    question == "m2m_4" ~ "Engagement to be married",
    question == "m2m_5" ~ "Education",
    question == "m2m_6" ~ "Gather with family",
    question == "m2m_7" ~ "Access to health facilities",
    question == "m2m_8" ~ "Better economic opportunities",
    question == "m2m_9" ~ "Natural disaster, drought",
    question == "m2m_10" ~ "Current political situation",
    question == "m2m_666" ~ "Other",
    question == "m2m_98" ~ "Don’t know",
    question == "m2m_99" ~ "Refused to answer"
  )) %>% 
  mutate(question = str_wrap(question, 15)) %>% 
  ggplot(aes(x = reorder(question, ord), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.25, size = 3) +
  labs(x = NULL, y = "Percent", title = "Why did you move?")

# ggsave(glue::glue("{output_path_graphs}module1_m2m.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(m2n_primary_boys:m2o_upper_girls) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100, 1))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>%  
  mutate(ord = case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 2,
    x == "Don't know" ~ 3,
    TRUE ~ 4
  )) %>%  
  mutate(question = case_when(
    question == "m2n_primary_boys" ~ "Primary schools for boys",
    question == "m2n_primary_girls" ~ "Primary schools for girls",
    question == "m2o_lower_boys" ~ "Lower secondary schools for boys",
    question == "m2o_lower_girls" ~ "Lower secondary schools for girls",
    question == "m2o_upper_boys" ~ "Upper secondary schools for boys",
    question == "m2o_upper_girls" ~ "Upper secondary schools for girls"
  )) %>% 
  mutate(question = str_wrap(question, 20)) %>% 
  ggplot(aes(x = question, y = percent, fill = reorder(x, ord))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord < 3, glue::glue("{round(percent)}% (n={Freq})"), NA)), position = position_stack(vjust = 0.5), size = 3) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#009076", "#f0b505", "#039e2f", "#5a02ad", "#04bdb0")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "Are schools open for in the community you live in?")

ggsave(glue::glue("{output_path_graphs}module1_m2n_primary_boys to m2o_upper_girls.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(m2p, m2q, m2pboy, m2qboy) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>%  
  mutate(x = ifelse(grepl("Not applicable", x), "Not applicable", as.character(x))) %>% 
  mutate(ord = case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 2,
    x == "Don't know" ~ 3,
    x == "Not applicable" ~ 4
  )) %>%  
  mutate(question = case_when(
    question == "m2p" ~ "Are girls in your household able to attend primary school?",
    question == "m2q" ~ "Are girls in your household able to attend secondary school?",
    question == "m2pboy" ~ "Are boys in your household able to attend primary school?",
    question == "m2qboy" ~ "Are boys in your household able to attend secondary school?"
  )) %>% 
  mutate(question = str_wrap(question, 20)) %>% 
  ggplot(aes(x = question, y = percent, fill = reorder(x, ord))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord != 3, glue::glue("{percent}% (n={Freq})"), NA)), position = position_stack(vjust = 0.5), size = 3) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#009076", "#f0b505", "#039e2f", "#5a02ad")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "Ability to attend school")

ggsave(glue::glue("{output_path_graphs}module1_m2p & m2q & m2pboy & m2qboy.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("m2r_"), -m2r_other) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>%  
  filter(x != 0) %>% 
  mutate(question = case_when(
    question == "m2r_1" ~ "Completed education",
    question == "m2r_2" ~ "Too young",
    question == "m2r_4" ~ "Too old",
    question == "m2r_5" ~ "Not allowed to go back to school by family",
    question == "m2r_6" ~ "No money/too expensive",
    question == "m2r_7" ~ "No schools close to home",
    question == "m2r_8" ~ "Started working/had to work",
    question == "m2r_9" ~ "Family/social restrictions",
    question == "m2r_10" ~ "Caring for sick household members",
    question == "m2r_11" ~ "Lack of food or income/need to work",
    question == "m2r_12" ~ "For marriage",
    question == "m2r_13" ~ "Family displaced",
    question == "m2r_14" ~ "Concerns due to COVID",
    question == "m2r_15" ~ "School is currently closed due to COVID",
    question == "m2r_16" ~ "School is closed by Taliban / Taliban does not allow to attend school.",
    question == "m2r_17" ~ "School is currently closed due to conflict/insecurity/war",
    question == "m2r_18" ~ "Safety concerns due to conflict/insecurity/war",
    question == "m2r_19" ~ "School is currently closed due to other reasons",
    question == "m2r_20" ~ "Other safety concerns",
    question == "m2r_21" ~ "Disability/illness",
    question == "m2r_666" ~ "Other",
    question == "m2r_98" ~ "Don’t know",
    question == "m2r_99" ~ "Refused to answer"
  )) %>% 
  mutate(question = str_wrap(question, 15)) %>% 
  ggplot(aes(x = reorder(question, -Freq), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.1, size = 3) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "What are the reasons some or all the girls have not gone back to school?") +
  scale_y_continuous(expand = c(0, 10))

ggsave(glue::glue("{output_path_graphs}module1_m2r.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("m2rboy_"), -m2rboy_other) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>%  
  filter(x != 0) %>% 
  mutate(question = case_when(
    question == "m2rboy_1" ~ "Completed education",
    question == "m2rboy_2" ~ "Too young",
    question == "m2rboy_4" ~ "Too old",
    question == "m2rboy_5" ~ "Not allowed to go back to school by family",
    question == "m2rboy_6" ~ "No money/too expensive",
    question == "m2rboy_7" ~ "No schools close to home",
    question == "m2rboy_8" ~ "Started working/had to work",
    question == "m2rboy_9" ~ "Family/social restrictions",
    question == "m2rboy_10" ~ "Caring for sick household members",
    question == "m2rboy_11" ~ "Lack of food or income/need to work",
    question == "m2rboy_12" ~ "For marriage",
    question == "m2rboy_13" ~ "Family displaced",
    question == "m2rboy_14" ~ "Concerns due to COVID",
    question == "m2rboy_15" ~ "School is currently closed due to COVID",
    question == "m2rboy_16" ~ "School is closed by Taliban / Taliban does not allow to attend school.",
    question == "m2rboy_17" ~ "School is currently closed due to conflict/insecurity/war",
    question == "m2rboy_18" ~ "Safety concerns due to conflict/insecurity/war",
    question == "m2rboy_19" ~ "School is currently closed due to other reasons",
    question == "m2rboy_20" ~ "Other safety concerns",
    question == "m2rboy_21" ~ "Disability/illness",
    question == "m2rboy_666" ~ "Other",
    question == "m2rboy_98" ~ "Don’t know",
    question == "m2rboy_99" ~ "Refused to answer"
  )) %>% 
  mutate(question = str_wrap(question, 15)) %>% 
  ggplot(aes(x = reorder(question, -Freq), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.1, size = 3) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "What are the reasons some or all the boys have not gone back to school?") +
  scale_y_continuous(expand = c(0, 10))

ggsave(glue::glue("{output_path_graphs}module1_m2rboy.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(m2s1:m2s5) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>%  
  mutate(ord = case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 2,
    x == "Don't know" ~ 3,
    x == "Refuse to answer" ~ 4
  )) %>% 
  mutate(question = case_when(
    question == "m2s1" ~ "In the last 7 days, did you work for a wage, salary, commission or any payment in kind; including doing paid domestic work or paid farm work even if for one hour?",
    question == "m2s2" ~ "In the last 7 days, did you run a business of any size for yourself or the household or with partners, even if for one hour?",
    question == "m2s3" ~ "In the last 7 days, did you help in any kind of business run by this household, even if for one hour?",
    question == "m2s4" ~ "In the last 7 days, did you work as a paid or unpaid apprentice even if just for one hour?",
    question == "m2s5" ~ "In the last 7 days, did you work on your household's farm or work with livestock or poultry, even if for one hour?"
  )) %>% 
  mutate(question = str_wrap(question, 30)) %>% 
  ggplot(aes(x = question, y = percent, fill = reorder(x, ord))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord < 3, glue::glue("{percent}%\n(n={Freq})"), NA)), position = position_stack(vjust = 0.5), size = 3) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#009076", "#f0b505", "#039e2f")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "Source of income")

ggsave(glue::glue("{output_path_graphs}module1_m2s1 to m2s5.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("Q2t_"), -Q2t_other) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>%  
  filter(x != 0) %>% 
  mutate(question = case_when(
    question == "Q2t_1" ~ "Own illness",
    question == "Q2t_2" ~ "Maternity/child rearing leave",
    question == "Q2t_3" ~ "Household member sick/death",
    question == "Q2t_4" ~ "Retired",
    question == "Q2t_5" ~ "Work suspension",
    question == "Q2t_6" ~ "Temporary work load reduction",
    question == "Q2t_7" ~ "Enterprise closure",
    question == "Q2t_8" ~ "Bad weather",
    question == "Q2t_9" ~ "School / education / training",
    question == "Q2t_10" ~ "On Leave",
    question == "Q2t_11" ~ "Current political situation",
    question == "Q2t_12" ~ "Prohibited by household member",
    question == "Q2t_13" ~ "Transportation/Mobility constraints",
    question == "Q2t_14" ~ "No chances to get a job / no jobs available",
    question == "Q2t_666" ~ "Other",
    question == "Q2t_98" ~ "Don’t know",
    question == "Q2t_99" ~ "Refused to answer"
  )) %>% 
  mutate(question = str_wrap(question, 15)) %>% 
  ggplot(aes(x = reorder(question, -Freq), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.1, size = 3) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "What are the reasons some or all the boys have not gone back to school?") +
  scale_y_continuous(expand = c(0, 5))

ggsave(glue::glue("{output_path_graphs}module1_Q2t.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(Label = m4a, name = "fraction") %>% 
  drop_na() %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100)) %>% 
  mutate(ord_color = case_when(
    Label == "More than usual" ~ 1,
    Label == "About the same" ~ 2,
    Label == "Less than usual" ~ 3,
    Label == "Don't know" ~ 4,
    Label == "Refuse to answer" ~ 5,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=4, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#009076", "#f0b505", "#039e2f", "#5a02ad")) +
  labs(title = str_wrap("Were your earnings for the last 30 days more, less or the same as the earnings you earn in a regular month?", 60))

ggsave(glue::glue("{output_path_graphs}module1_m4a.png"), width = 8)

# Module 2
df_complete_approved %>% 
  count(Label = Q4c, name = "fraction") %>% 
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(fraction/sum(fraction)*100, 1)) %>% 
  mutate(ord_color = case_when(
    Label == "Yes" ~ 1,
    Label == "No" ~ 2,
    Label == "Don't know" ~ 3,
    Label == "Refuse to answer" ~ 4,
  )) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Label, -ord_color))) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=glue::glue("{Label}\n{Percent}% (n={fraction})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#009076", "#f0b505", "#039e2f")) +
  labs(title = "During the last 30 days did you have enough food for your household?")

ggsave(glue::glue("{output_path_graphs}module2_Q4c.png"))

df_complete_approved %>% 
  select(Q4d1:Q4d5) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100, 1))
  ) %>% 
  data.table::rbindlist(id = "question") %>% 
  mutate(ord_label = case_when(
    x == "Never" ~ 1,
    x == "Rarely (1-2 times)" ~ 2,
    x == "Sometimes (3-10 times)" ~ 3,
    x == "Often (more than 10 times)" ~ 4,
    x == "Do not know" ~ 5,
    x == "Refuse to answer" ~ 6,
  )) %>% 
  mutate(q = case_when(
    question == "Q4d1" ~ "Not have enough food or money",
    question == "Q4d2" ~ "Rely on less-preferred and less-expensive foods because of lack of resources",
    question == "Q4d3" ~ "Limit portion sizes at meal times",
    question == "Q4d4" ~ "Restrict consumption by adults in order for small children to eat",
    question == "Q4d5" ~ "Skip meals or reduce number of meals eaten by anyone in household",
  )) %>% 
  mutate(q = str_wrap(q, 20)) %>% 
  ggplot(aes(x = q, y = percent,
             fill = reorder(x, ord_label))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord_label > 4, NA, glue::glue("{round(percent)}%\n(n={Freq})"))),
            position = position_stack(vjust = 0.5), size = 2.5) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#009076", "#039e2f", "#f0b505", "#04bdb0", "#5a02ad")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "In the last 7 days, how often did anyone in your household have to:")

ggsave(glue::glue("{output_path_graphs}module2_Q4d1_to_Q4d5.png"), width = 10, height = 6)

rbind(
  df_complete_approved %>% 
    select(rank1_1:rank1_99) %>% 
    lapply(function(x)
      table(x) %>% data.frame() %>% 
        mutate(percent = round(Freq/sum(Freq)*100, 1), rank = "rank1")
    ) %>% 
    data.table::rbindlist(id = "question") %>% 
    filter(x == 1)
  ,
  df_complete_approved %>% 
    select(rank2_1:rank2_99) %>% 
    lapply(function(x)
      table(x) %>% data.frame() %>% 
        mutate(percent = round(Freq/sum(Freq)*100, 1), rank = "rank2")
    ) %>% 
    data.table::rbindlist(id = "question") %>% 
    filter(x == 1)
  ,
  df_complete_approved %>% 
    select(rank3_1:rank3_99) %>% 
    lapply(function(x)
      table(x) %>% data.frame() %>% 
        mutate(percent = round(Freq/sum(Freq)*100, 1), rank = "rank3")
    ) %>% 
    data.table::rbindlist(id = "question") %>% 
    filter(x == 1)
  ,
  df_complete_approved %>% 
    select(rank4_1:rank4_99) %>% 
    lapply(function(x)
      table(x) %>% data.frame() %>% 
        mutate(percent = round(Freq/sum(Freq)*100, 1), rank = "rank4")
    ) %>% 
    data.table::rbindlist(id = "question") %>% 
    filter(x == 1)
  ,
  df_complete_approved %>% 
    select(rank5_1:rank5_99) %>% 
    lapply(function(x)
      table(x) %>% data.frame() %>% 
        mutate(percent = round(Freq/sum(Freq)*100, 1), rank = "rank5")
    ) %>% 
    data.table::rbindlist(id = "question") %>% 
    filter(x == 1)
) %>% 
  mutate(question = case_when(
    grepl("_1", question) ~ "Respondent",
    grepl("_2", question) ~ "Male adults",
    grepl("_3", question) ~ "Female adults",
    grepl("_4", question) ~ "Male children",
    grepl("_5", question) ~ "Female children",
    grepl("_6", question) ~ "Food is never short",
    grepl("_98", question) ~ "Don't know",
    grepl("_99", question) ~ "Refused"
  )) %>% 
  group_by(question) %>% 
  mutate(ord = sum(percent)) %>% 
  ggplot(aes(x = reorder(question, -ord), y = percent, fill = rank)) +
  geom_col() +
  geom_text(aes(label = glue::glue("{round(percent)}% (n={Freq})")),
            position = position_stack(vjust = 0.5), size = 2.5) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505", "#04bdb0", "#02631e")) +
  labs(x = NULL, y = NULL, fill = NULL, title = str_wrap("When food is short (meaning there is not enough for everyone), in what order are household members generally served?", 110))

ggsave(glue::glue("{output_path_graphs}module2_rank1 to rank5.png"), width = 10, height = 6)

# Module 3
df_complete_approved %>% 
  count(q = barrier5) %>% 
  mutate(
    ymax = cumsum(n),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(n/sum(n)*100)) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=q)) +
  geom_rect() +
  geom_text(x=4, aes(y=labelPosition, label=glue::glue("{q}\n{Percent}% (n={n})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#f0b505", "#039e2f", "#009076")) +
  labs(title = str_wrap("In the past 30 days, was any asset that you used in your business sold to purchase food or in response to any other emergency of the household?", 100))

ggsave(glue::glue("{output_path_graphs}module3_barrier5.png"), width = 10, height = 6)

# Module 4
df_complete_approved %>% 
  count(q = Q10cbsg5) %>% 
  mutate(
    ymax = cumsum(n),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(n/sum(n)*100)) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=q)) +
  geom_rect() +
  geom_text(x=4, aes(y=labelPosition, label=glue::glue("{q}\n{Percent}% (n={n})")), size=2) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505", "#5a02ad", "#04bdb0", "#02631e")) +
  labs(title = str_wrap("Over the last 30 days, are you usually accompanied by any other person when you go out of your compound/dwelling?", 80))

ggsave(glue::glue("{output_path_graphs}module4_Q10cbsg5.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(paste0("mob", 1:7)) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100, 1))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>% 
  mutate(question = case_when(
    question == "mob1" ~ "Women are able to go to public water points and collect water",
    question == "mob2" ~ "Women continue to wash clothes in public spaces",
    question == "mob3" ~ "Women are able to go to the field",
    question == "mob4" ~ "Women are able to visit relatives and friends",
    question == "mob5" ~ "Women are (alone or moving in groups) dropping off/ picking up boys at school",
    question == "mob6" ~ "Women are able to go and visit friends / relatives in other parts of the village",
    question == "mob7" ~ "Women are able to leave the village (with Maharam) and go to health clinics in the locality or district center"
  )) %>%
  mutate(question = str_wrap(question, 20)) %>%
  mutate(ord = case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 2,
    x == "Not applicable" ~ 3,
    x == "Don't know" ~ 4,
    x == "Refuse to answer" ~ 5,
  )) %>% 
  ggplot(aes(x = question, y = percent, fill = reorder(x, ord))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord < 3, glue::glue("{round(percent)}% (n={Freq})"), NA)), position = position_stack(vjust = 0.5), size = 3) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c( "#009076", "#f0b505", "#04bdb0", "#02631e", "#5a02ad")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "In your mohalla, over the last 30 days: Are women able to:")

ggsave(glue::glue("{output_path_graphs}module4_mob1 to mob7.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("Q10cbsg6_")) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>% 
  filter(x == 1) %>% 
  mutate(question = case_when(
    question == "Q10cbsg6_0" ~ "Self",
    question == "Q10cbsg6_1" ~ "Spouse",
    question == "Q10cbsg6_2" ~ "Children under 12",
    question == "Q10cbsg6_3" ~ "Teenage boys",
    question == "Q10cbsg6_4" ~ "Teenage girls",
    question == "Q10cbsg6_5" ~ "Other male household member",
    question == "Q10cbsg6_6" ~ "Other female household member",
    question == "Q10cbsg6_7" ~ "Male relative outside of household",
    question == "Q10cbsg6_8" ~ "Female relative outside of household",
    question == "Q10cbsg6_10" ~ "Female non-relative"
  )) %>% 
  mutate(question = str_wrap(question, 15)) %>% 
  ggplot(aes(x = reorder(question, -percent), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.25, size = 3) +
  labs(x = NULL, y = "Percent", title = "Who usually accompanies you?") +
  scale_y_continuous(expand = c(0, 5))

ggsave(glue::glue("{output_path_graphs}module4_Q10cbsg6.png"), width = 10, height = 6)

df_complete_approved %>% 
  mutate(Q10cbsg1b = as.numeric(Q10cbsg1b)) %>% 
  select(Q10cbsg1b, Q10cbsg2, Q10cbsg3) %>% 
  pivot_longer(everything()) %>% 
  mutate(name = case_when(
    name == "Q10cbsg1b" ~ "In the last 7 days, on how many days did you leave your compound/dwelling?",
    name == "Q10cbsg2" ~ "In the last 30 days, how many times did you leave your village?",
    name == "Q10cbsg3" ~ "In the last 30 days, how many times did you go to your local market?"
  )) %>% 
  mutate(name = str_wrap(name, 25)) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  scale_fill_manual(values = c("#039e2f", "#009076", "#f0b505")) +
  scale_y_continuous(breaks = c(0:4, seq(5, 30, 5))) +
  labs(x = NULL, y = "# Days")

ggsave(glue::glue("{output_path_graphs}module4_Q10cbsg1b & Q10cbsg2 & Q10cbsg3.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(q = Q10a) %>% 
  mutate(percent = round(n/sum(n)*100)) %>% 
  mutate(ord = case_when(
    q == "Never, it is not permitted" ~ 4,
    q == "Never, group has dissolved" ~ 6,
    q == "Never, group has a conflict" ~ 7,
    q == "One time" ~ 1,
    q == "Between 1 and 5 times" ~ 2,
    q == "More than 5 times" ~ 3,
    q == "Not Applicable" ~ 5,
  )) %>% 
  mutate(q = str_wrap(q, 25)) %>% 
  ggplot(aes(x = reorder(q, ord), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label=glue::glue("{percent}%\n(n={n})")), vjust = -0.1, size = 3) +
  labs(x = NULL, y = "Percent", fill = NULL, title = str_wrap("In the last 30 days, that is, the last one month, how many days did you attend any kind of CBSG group meeting with members of your community?", 110)) +
  scale_y_continuous(expand = c(0, 5))

ggsave(glue::glue("{output_path_graphs}module4_Q10a.png"), width = 10, height = 6)

# Module 5
(day <- df_complete_approved %>% 
  count(q = Q10cbsg88) %>% 
  mutate(q = str_wrap(q, 10)) %>% 
  mutate(percent = round(n/sum(n)*100)) %>% 
  mutate(ord = as.numeric(q)) %>% 
  mutate(Color = ifelse(is.na(ord), "green", "orange")) %>%
  ggplot(aes(x = reorder(q, ord), y = percent, fill = Color)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = glue::glue("{percent}%\n(n={n})")), vjust = -0.2) +
  labs(x = NULL, y = "Percent", subtitle =  "During the day."))

(night <- df_complete_approved %>% 
  count(q = Q10cbsg89) %>% 
  mutate(q = str_wrap(q, 10)) %>% 
  mutate(percent = round(n/sum(n)*100)) %>% 
  mutate(ord = as.numeric(q)) %>% 
  mutate(Color = ifelse(is.na(ord), "green", "orange")) %>%
  ggplot(aes(x = reorder(q, ord), y = percent, fill = Color)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = glue::glue("{percent}%\n(n={n})")), vjust = -0.2) +
  labs(x = NULL, y = "Percent", subtitle = "During the night."))

day + night + plot_annotation(title = "On a scale from 1 to 10, how often have you felt unsafe walking in your village in the last 30 days?")

# ggsave(glue::glue("{output_path_graphs}module5_Q10cbsg88 & Q10cbsg89.png"), width = 10, height = 6)

df_complete_approved %>% 
  count(q = barrier28) %>% 
  mutate(
    ymax = cumsum(n),
    ymin = c(0, head(ymax, n=-1)),
    labelPosition = (ymax + ymin) / 2
  ) %>% 
  mutate(Percent = round(n/sum(n)*100)) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=q)) +
  geom_rect() +
  geom_text(x=4, aes(y=labelPosition, label=glue::glue("{q}\n{Percent}% (n={n})")), size=3) +
  coord_polar(theta="y") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(vjust = 0.5, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("#f0b505", "#039e2f", "#009076")) +
  labs(title = "In the last six months, were there any times when you felt unsafe in your home?")

ggsave(glue::glue("{output_path_graphs}module5_barrier28.png"), width = 10, height = 6)

df_complete_approved %>% 
  select(starts_with("wfws12_"), -wfws12_other) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100))
  ) %>% 
  data.table::rbindlist(idcol = "question") %>% 
  filter(x == 1) %>% 
  mutate(question = case_when(
    question == "wfws12_1" ~ "Friend's home",
    question == "wfws12_2" ~ "Relative's home (including parents and family members)",
    question == "wfws12_3" ~ "Neighbor’s home",
    question == "wfws12_4" ~ "A shelter in the community (e.g. mosque, clinic)",
    question == "wfws12_666" ~ "Other",
    question == "wfws12_98" ~ "Do not know",
    question == "wfws12_99" ~ "Refuse to answer"
  )) %>% 
  mutate(question = str_wrap(question, 15)) %>% 
  ggplot(aes(x = reorder(question, -percent), y = percent)) +
  geom_col(show.legend = F, fill = "#009076") +
  geom_text(aes(label = glue::glue("{percent}%\n(n={Freq})")), vjust = -0.25, size = 3) +
  labs(x = NULL, y = "Percent", title = str_wrap("If there were an emergency or if you felt unsafe in your home, is there another place where you know you could go to sleep?", 120)) +
  scale_y_continuous(expand = c(0, 5))

ggsave(glue::glue("{output_path_graphs}module5_wfws12.png"), width = 10, height = 6)

# Module 6
df_complete_approved %>% 
  select(phq1, phq2, gad1, gad2) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100, 1))
  ) %>% 
  data.table::rbindlist(id = "question") %>% 
  mutate(ord_label = case_when(
    x == "Not at all" ~ 1,
    x == "Several days" ~ 2,
    x == "More than half the days" ~ 3,
    x == "Often (more than 10 times)" ~ 4,
    x == "Nearly every day" ~ 5,
    x == "Do not know" ~ 98,
    x == "Refuse to answer" ~ 998,
  )) %>% 
  mutate(q = case_when(
    question == "phq1" ~ "Little interest or pleasure in doing things",
    question == "phq2" ~ "Feeling down, depressed, or hopeless",
    question == "gad1" ~ "Feeling nervous, anxious, or on edge",
    question == "gad2" ~ "Not being able to stop or control worrying"
  )) %>% 
  mutate(q = str_wrap(q, 20)) %>% 
  ggplot(aes(x = q, y = percent,
             fill = reorder(x, ord_label))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord_label > 5, NA, glue::glue("{round(percent)}%\n(n={Freq})"))),
            position = position_stack(vjust = 0.5), size = 2.5) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#039e2f", "#04bdb0", "#009076", "#02631e", "#5a02ad", "#f0b505")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "Over the last 2 weeks, how often have you been bothered by the following problems:")

ggsave(glue::glue("{output_path_graphs}module6_phq1 & phq2 & gad1 & gad2.png"), width = 10, height = 6)

# Module 7
df_complete_approved %>% 
  select(Q10h2:Q10h9) %>% 
  lapply(function(x)
    table(x) %>% data.frame() %>% 
      mutate(percent = round(Freq/sum(Freq)*100, 1))
  ) %>% 
  data.table::rbindlist(id = "question") %>% 
  mutate(ord_label = case_when(
    x == "Never consulted" ~ 1,
    x == "Sometimes consulted" ~ 2,
    x == "Always consulted" ~ 3,
    x == "Always consulted and decision followed" ~ 4,
    x == "Not applicable" ~ 5,
    x == "Do not know" ~ 98,
    x == "Refuse to answer" ~ 998,
  )) %>% 
  mutate(q = case_when(
    question == "Q10h2" ~ "Buying groceries",
    question == "Q10h3" ~ "Buying expensive items (i.e. mobile phone, farm equipment)",
    question == "Q10h4" ~ "Buying or selling land or property",
    question == "Q10h5" ~ "Whether your daughter should attend school",
    question == "Q10h6" ~ "Whether your son should attend school",
    question == "Q10h7" ~ "Marriage of children",
    question == "Q10h8" ~ "How many children to have",
    question == "Q10h9" ~ "You wanted to pursue a job outside the home",
  )) %>% 
  mutate(q = str_wrap(q, 20)) %>% 
  ggplot(aes(x = q, y = percent,
             fill = reorder(x, ord_label))) +
  geom_col() +
  geom_text(aes(label = ifelse(ord_label > 5, NA, glue::glue("{round(percent)}%\n(n={Freq})"))),
            position = position_stack(vjust = 0.5), size = 2.5) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#039e2f", "#04bdb0", "#009076", "#02631e", "#f0b505", "#5a02ad", "#e86f05")) +
  labs(x = NULL, y = "Percent", fill = NULL, title = "How much would your opinion be considered in the final decision if your household needs to decide on the following things:")

ggsave(glue::glue("{output_path_graphs}module7_Q10h2 to Q10h9.png"), width = 10, height = 6)

response_status <- full_join(
  df %>% 
    group_by(caseid) %>%
    summarise(last_call = max(starttime))
  ,
  df %>% 
    select(caseid, starttime, phone_response_short)
  , by = "caseid"
) %>% 
  filter(starttime == last_call | phone_response_short == "Complete") %>% 
  count(q = phone_response_short) %>% 
  mutate(N = sum(n))

response_status %>% 
  mutate(percent = round(n/N*100, 1)) %>% 
  mutate(ord = ifelse(q == "Complete", 0, percent)) %>% 
  mutate(Color = ifelse(q == "Complete", "b", "a")) %>% 
  mutate(q = str_wrap(q, 20)) %>% 
  ggplot(aes(x = reorder(q, -ord), y = percent, fill = Color)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = glue::glue("{round(percent)}%\n(n={n})")), vjust = -0.1) +
  scale_fill_manual(values = c("#f0b505", "#009076")) +
  scale_y_continuous(expand = c(0, 5)) +
  labs(x = NULL, y = "Percent")

ggsave(glue::glue("{output_path_graphs}reasons_for_incomplete_interviews.png"), width = 10, height = 6)

