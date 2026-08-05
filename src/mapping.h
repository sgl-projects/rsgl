#ifndef MAPPING_H
#define MAPPING_H

#include"aes.h"
#include"cgs.h"

int mapping_exists(
	enum aes aes,
	struct aes_mapping *aes_mappings
);

#endif
