#ifndef GEOM_H
#define GEOM_H

enum geom { 
	POINT,
	BAR,
	LINE,
	BOX
};

int valid_geom_str(const char *geom_str);

enum geom geom_enum(const char *geom_str);

#endif
