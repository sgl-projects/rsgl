#include<stddef.h>
#include"aes.h"
#include"cgs.h"
#include"aes_mapping.h"

int aes_mapping_exists(enum aes aes, struct aes_mapping *aes_mappings) {
	struct aes_mapping *current_mapping = aes_mappings;
	while (current_mapping != NULL) {
		if (current_mapping->aes == aes) {
			return 1;
		}
		current_mapping = current_mapping->next;
	}
	return 0;
}
