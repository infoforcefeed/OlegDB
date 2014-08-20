#include "data.h"

void inline ol_string_free(ol_string** strp)
{
	ol_string* str = *strp;
	if (str == NULL)
		return;
	free(str->data);
	free(str);
	*strp = NULL;
}
