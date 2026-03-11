#include<R.h>
#include"geom.h"
#include"keyword.h"

static struct keyword_enum_row geom_keyword_enum_table[] = {
  {"point", POINT},
  {"points", POINT},
	{"bar", BAR},
	{"bars", BAR},
	{"line", LINE},
	{"lines", LINE},
	{"box", BOX},
	{"boxes", BOX}
};

static int geom_count = KEYWORD_COUNT(geom_keyword_enum_table);

int valid_geom_str(char *geom_str) {
  return valid_keyword_str(geom_str, geom_keyword_enum_table, geom_count);
}

enum geom geom_enum(char *geom_str) {
  return (enum geom)enum_int(geom_str, geom_keyword_enum_table, geom_count);
}

