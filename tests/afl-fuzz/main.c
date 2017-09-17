#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Decode_stub.h"
extern void __stginit_Decode(void);
#endif
#include <stdio.h>

int main(int argc, char *argv[]) {
	int i;
	hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
	hs_add_root(__stginit_Decode);
#endif

	i = decodeStdin_hs();
	printf("Decoded length: %d\n", i);

	hs_exit();
	return 0;
}

