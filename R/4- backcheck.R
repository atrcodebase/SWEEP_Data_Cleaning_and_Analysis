backcheck <- readxl::read_excel(backcheck_path)
remove_columns <- c("deviceid", "subscriberid", "simid", "devicephonenum", "username", "instance_time", "text_audit",
                    "phone_call_log", "phone_call_duration", "collect_phone_app", "device_info", "full_name", "users",
                    "pub_to_users", "num_calls", "call_num", "stop_at", "now_complete", "userid", "calltime", "Supervisor_Name",
                    "Supervisor_Name_label", "Call_Type", "Call_Type_label", "call_respondent", "phone_response",
                    "phone_response_label", "Not_reached_out_to_respondent", "Not_reached_out_to_respondent_label",
                    "Is_the_target_respondent_living_in_this_hh", "Is_the_target_respondent_living_in_this_hh_label", "Phone_passed_over",
                    "Phone_passed_over_label", "Phone_passed_over_other", "answered_response", "answered_response_label", "reschedule",
                    "instanceID", "instanceName", "formdef_version"
                    )

if (sum(!remove_columns %in% colnames(backcheck)) > 0 ) {
  remove_columns <- remove_columns[remove_columns %in% colnames(backcheck)]
}

backcheck <- select(backcheck, -remove_columns)
backcheck <- select(backcheck, -c(grep("_label", colnames(backcheck))-1))
colnames(backcheck) <- str_remove(colnames(backcheck), "_label")

backcheck <- backcheck %>% 
  mutate(
    AA_Full = gsub("File skipped from exports: |File skipped from exports: media/", "", AA_Full),
    AA_Full = case_when(
      !is.na(AA_Full) ~ glue::glue("https://atrconsultingaf.surveycto.com/view/submission-attachment/{AA_Full}?uuid=uuid%3A{str_remove(KEY, 'uuid:')}"),
      TRUE ~ AA_Full
    ))

# count(backcheck, phone_response_short)
backcheck_complete <- filter(backcheck, phone_response_short == "Complete")




