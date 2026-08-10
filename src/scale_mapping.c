#include<stddef.h>
#include"aes.h"
#include"cgs.h"
#include"scale_mapping.h"

int scale_mapping_exists(enum aes aes, struct scale_expr *scales) {
	struct scale_expr *current_scale = scales;
	while (current_scale != NULL) {
		if (current_scale->aes == aes) {
			return 1;
		}
		current_scale = current_scale->next;
	}
	return 0;
}
