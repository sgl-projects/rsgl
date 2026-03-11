#ifndef QUAL_H
#define QUAL_H

enum qual {
	DEFAULT,
	JITTERED,
	REGRESSION,
	UNSTACKED
};

int valid_qual_str(char *qual_str);

enum qual qual_enum(char *qual_str);

#endif
