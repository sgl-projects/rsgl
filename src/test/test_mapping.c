#include<stddef.h>
#include<criterion/criterion.h>
#include<aes.h>
#include<cgs.h>
#include<mapping.h>

Test(mapping_exists, returns_0_for_null_ptr) {
	cr_expect(!mapping_exists(X, NULL));
}

Test(mapping_exists, returns_0_if_mapping_for_aes_doesnt_exist) {
	struct aes_mapping mapping_1 = {
		.aes=Y
	};
	struct aes_mapping mapping_2 = {
		.aes=COLOR
	};
	mapping_1.next=&mapping_2;

	cr_expect(!mapping_exists(X, &mapping_1));
}

Test(mapping_exists, returns_1_if_mapping_for_aes_exists) {
	struct aes_mapping mapping_1 = {
		.aes=Y
	};
	struct aes_mapping mapping_2 = {
		.aes=X
	};
	mapping_1.next=&mapping_2;

	cr_expect(mapping_exists(X, &mapping_1));
}
