#include "shim.h"

// onig_error_code_to_str is variadic.
int hs_onig_error_code_to_str(UChar* err_buf, OnigPosition err_code) {
  onig_error_code_to_str(err_buf, err_code);
}

// for use with onig_new
int hs_onig_error_code_to_str_with_err_info(UChar* err_buf, OnigPosition err_code, OnigErrorInfo* err_info) {
  onig_error_code_to_str(err_buf, err_code, err_info);
}

void hs_onig_region_free_with_self(OnigRegion* region) {
  onig_region_free(region, 1);
}

int foreach_name_callback(const UChar* name, const UChar* name_end, int num_groups, int* groups, regex_t* regex, void* arg) {
  MatchInfo** info_ptr = arg;
  MatchInfo* info = *info_ptr;

  info->name = name;
  info->name_len = (int)(name_end - name);
  info->groups = groups;
  info->num_groups = num_groups;

  *info_ptr = &info[1];

  return 0;
}

void hs_onig_match_info(OnigRegex reg, MatchInfo* info_buf) {
  onig_foreach_name(reg, &foreach_name_callback, &info_buf);
}
