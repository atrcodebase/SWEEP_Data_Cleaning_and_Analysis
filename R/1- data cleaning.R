df <- read_excel(data_path, guess_max = 10000)
remove_columns <- c("subscriberid", "simid", "devicephonenum", "username", "instance_time", "text_audit", "phone_call_log", "phone_call_duration",
                    "collect_phone_app", "call_time", "reschedule", "reschedule_full", "reschedule_no_ans",  "call_respondent", "device_info",
                    "address", "users", "pub_to_users", "call_datetime", "new_sortby", "num_calls", "Please_Select_The_Qa_Status_Of_The_Survey",
                    "needs_review", "userid", "AA1", "AA2", "AA3", "AA4", "AA5", "end_time", "qa", "instanceID", "instanceName", "formdef_version",
                    "review_quality", "review_comments", "review_corrections", "Surveyor_Name_label", "District_New_label", "Province_New_label",
                    "deviceid", "callback_time", "last_call_status", "call_num", "stop_at", "now_complete", "full_name", "call_date", "Surveyor_Name",
                    "phone_response", "phone_response_label", "Reached_out_to_respondent", "Reached_out_to_respondent_label",	"Not_reached_out_to_respondent",
                    "Not_reached_out_to_respondent_label",	"Is_the_target_respondent_living_in_this_hh", "Is_the_target_respondent_living_in_this_hh_label",
                    "Phone_passed_over", "Phone_passed_over_label", "Obtions_to_reach_out_to_respondent", "Obtions_to_reach_out_to_respondent_label",
                    "callback_availability", "callback_availability_label", "wfws12_Own", "wfws12_place_", "wfws12_at", "wfws12_home", "resp_pn1",
                    "resp_pn2", "resp_pn3", "resp_pn4", "resp_pn5", "calltime", "sfcbsg1", "resp_pn", "alternate_resp_pn", "answered_response",
                    "answered_response_label", "whynot", "whynot_label", "whynot_other", "resp_pn0",	"resp_pn0_label", "AA_Full"
                    )

if (sum(!remove_columns %in% colnames(df)) > 0 ) {
  remove_columns <- remove_columns[remove_columns %in% colnames(df)]
}

df <- select(df, -all_of(remove_columns))
df <- select(df, -c(grep("_label", colnames(df))-1))
colnames(df) <- str_remove(colnames(df), "_label")
df <- df %>% 
  # mutate(
  #   AA_Full = gsub("File skipped from exports: |File skipped from exports: media/", "", AA_Full),
  #   AA_Full = case_when(
  #     !is.na(AA_Full) ~ glue::glue("https://atrconsultingaf.surveycto.com/view/submission-attachment/{AA_Full}?uuid=uuid%3A{str_remove(KEY, 'uuid:')}"),
  #     TRUE ~ AA_Full
  #   )) %>% 
  mutate(
    Province = toupper(Province),
    District = toupper(str_remove(District, "_bad|_bam|_par|_tak"))
  )

for (rowi in 1:nrow(correction_log)){
  uuid_i <- correction_log$KEY[rowi]
  var_i  <- correction_log$question_name[rowi]
  old_i  <- correction_log$old_value[rowi]
  new_i  <- correction_log$new_value[rowi]
  print(paste("uuid:", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  df[df$KEY == uuid_i, var_i] <- new_i
}

df_complete_approved <- filter(df, phone_response_short == "Complete" & review_status == "APPROVED")


