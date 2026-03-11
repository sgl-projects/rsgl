#ifndef SCALE_H
#define SCALE_H

enum scale { 
	LINEAR,
	LOG
};

int valid_scale_str(char *scale_str);

enum scale scale_enum(char *scale_str);

#endif
