#include "square.h"
#include "stdio.h"

int main()
{
	int original = 15;
	int squared = square_faren(original);
	printf("Faren square\nOriginal: %d\nSquared: %d\n", original, squared);
	return 0;
}
