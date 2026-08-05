#include<stddef.h>
#include"aes.h"
#include"cgs.h"
#include"mapping.h"

int mapping_exists(enum aes aes, struct aes_mapping *mappings) {
	struct aes_mapping *current_mapping = mappings;
	while (current_mapping != NULL) {
		if (current_mapping->aes == aes) {
			return 1;
		}
		current_mapping = current_mapping->next;
	}
	return 0;
}
