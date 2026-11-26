#include <stddef.h>
size_t strlen(const char*);
int strcmp(const char*,const char*);
char*strchr(const char*,int);
int memcmp(const void*,const void*,size_t);
#define memcpy __builtin_memcpy
#define memmove __builtin_memmove
#define memset __builtin_memset
