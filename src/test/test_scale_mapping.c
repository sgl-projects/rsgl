#include<stddef.h>
#include<criterion/criterion.h>
#include<aes.h>
#include<cgs.h>
#include<scale_mapping.h>

Test(scale_mapping_exists, returns_0_for_null_ptr) {
	cr_expect(!scale_mapping_exists(X, NULL));
}

Test(scale_mapping_exists, returns_0_if_scale_for_aes_doesnt_exist) {
	struct scale_expr scale_1 = {
		.aes=Y
	};
	struct scale_expr scale_2 = {
		.aes=COLOR
	};
	scale_1.next=&scale_2;

	cr_expect(!scale_mapping_exists(X, &scale_1));
}

Test(scale_mapping_exists, returns_1_if_scale_for_aes_exists) {
	struct scale_expr scale_1 = {
		.aes=Y
	};
	struct scale_expr scale_2 = {
		.aes=X
	};
	scale_1.next=&scale_2;

	cr_expect(scale_mapping_exists(X, &scale_1));
}
