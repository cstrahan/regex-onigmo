#include <oniguruma.h>

typedef struct MatchInfoStruct {
  int num_groups;
  const int* groups;
  const UChar* name;
  int name_len;
} MatchInfo;

