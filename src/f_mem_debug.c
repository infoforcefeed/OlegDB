#include <stdlib.h>
#include <stdio.h>
#define F_NO_MEMORY_DEBUG
#include "forge.h"

#define F_MEMORY_OVER_ALLOC 32
#define F_MEMORY_MAGIC_NUMBER 132
typedef struct{
	uint size;
	void *buf;
}STMemAllocBuf;

typedef struct{
	uint line;
	char file[256];
	STMemAllocBuf *allocs;
	uint alloc_count;
	uint alloc_alocated;
	uint size;
	uint alocated;
	uint freed;
}STMemAllocLine;

STMemAllocLine f_allock_lines[1024];
uint f_allock_line_count = 0;

boolean f_debug_memory()
{
	boolean output = FALSE;
	uint i, j, k;
	for(i = 0; i < f_allock_line_count; i++)
	{
		for(j = 0; j < f_allock_lines[i].alloc_count; j++)
		{
			uint8 *buf;
			uint size;
			buf = f_allock_lines[i].allocs[j].buf;
			size = f_allock_lines[i].allocs[j].size;
			for(k = 0; k < F_MEMORY_OVER_ALLOC; k++)
				if(buf[size + k] != F_MEMORY_MAGIC_NUMBER)
					break;
			if(k < F_MEMORY_OVER_ALLOC)
			{
				printf("MEM ERROR: Overshoot at line %u in file %s\n", f_allock_lines[i].line, f_allock_lines[i].file);
				{
					uint *X = NULL;
					X[0] = 0;
				}
				output = TRUE;
			}
		}
	}
	return output;
}

void f_debug_mem_add(void *pointer, uint size, char *file, uint line)
{
	uint i, j;
	for(i = 0; i < F_MEMORY_OVER_ALLOC; i++)
		((uint8 *)pointer)[size + i] = F_MEMORY_MAGIC_NUMBER;
	f_debug_memory();

	for(i = 0; i < f_allock_line_count; i++)
	{
		if(line == f_allock_lines[i].line)
		{
			for(j = 0; file[j] != 0 && file[j] == f_allock_lines[i].file[j] ; j++);
			if(file[j] == f_allock_lines[i].file[j])
				break;
		}
	}
	if(i < f_allock_line_count)
	{
		if(f_allock_lines[i].alloc_alocated == f_allock_lines[i].alloc_count)
		{
			f_allock_lines[i].alloc_alocated += 1024;
			f_allock_lines[i].allocs = realloc(f_allock_lines[i].allocs, (sizeof *f_allock_lines[i].allocs) * f_allock_lines[i].alloc_alocated);
		}
		f_allock_lines[i].allocs[f_allock_lines[i].alloc_count].size = size;
		f_allock_lines[i].allocs[f_allock_lines[i].alloc_count++].buf = pointer;
		f_allock_lines[i].size += size;
		f_allock_lines[i].alocated++;
	}else
	{
		if(i < 1024)
		{
			f_allock_lines[i].line = line;
			for(j = 0; j < 255 && file[j] != 0; j++)
				f_allock_lines[i].file[j] = file[j];
			f_allock_lines[i].file[j] = 0;
			f_allock_lines[i].alloc_alocated = 1024;
			f_allock_lines[i].allocs = malloc((sizeof *f_allock_lines[i].allocs) * f_allock_lines[i].alloc_alocated);
			f_allock_lines[i].allocs[0].size = size;
			f_allock_lines[i].allocs[0].buf = pointer;
			f_allock_lines[i].alloc_count = 1;
			f_allock_lines[i].size = size;
			f_allock_lines[i].freed = 0;
			f_allock_lines[i].alocated++;
			f_allock_line_count++;
		}
	}
}

void *f_debug_mem_malloc(uint size, char *file, uint line)
{
	void *pointer;
	uint i;
	pointer = malloc(size + F_MEMORY_OVER_ALLOC);
	for(i = 0; i < size + F_MEMORY_OVER_ALLOC; i++)
 		((uint8 *)pointer)[i] = F_MEMORY_MAGIC_NUMBER + 1;
	f_debug_mem_add(pointer, size, file, line);
	return pointer;
}

void *f_debug_mem_calloc(uint nmemb, uint size, char *file, uint line)
{
	void *pointer;

	const uint TRUE_SIZE = nmemb * size;
	pointer = calloc(1, TRUE_SIZE + F_MEMORY_OVER_ALLOC);
	f_debug_mem_add(pointer, TRUE_SIZE, file, line);

	return pointer;
}

boolean f_debug_mem_remove(void *buf)
{
	uint i, j, k;
	for(i = 0; i < f_allock_line_count; i++)
	{
		for(j = 0; j < f_allock_lines[i].alloc_count; j++)
		{
			if(f_allock_lines[i].allocs[j].buf == buf)
			{
				for(k = 0; k < F_MEMORY_OVER_ALLOC; k++)
					if(((uint8 *)buf)[f_allock_lines[i].allocs[j].size + k] != F_MEMORY_MAGIC_NUMBER)
						break;
				if(k < F_MEMORY_OVER_ALLOC)
					printf("MEM ERROR: Overshoot at line %u in file %s\n", f_allock_lines[i].line, f_allock_lines[i].file);
				f_allock_lines[i].size -= f_allock_lines[i].allocs[j].size;
				f_allock_lines[i].allocs[j] = f_allock_lines[i].allocs[--f_allock_lines[i].alloc_count];
				f_allock_lines[i].freed++;
				return TRUE;
			}
		}	
	}
	return FALSE;
}

void f_debug_mem_free(void *buf)
{
	f_debug_mem_remove(buf);

	free(buf);
}


void *f_debug_mem_realloc(void *pointer, uint size, char *file, uint line)
{
	f_debug_memory();
	if(!f_debug_mem_remove(pointer))
	{
		/* tried to realloc a pointer  not allocated*/
		uint *X = NULL;
		X[0] = 0;
	}
	pointer = realloc(pointer, size + F_MEMORY_OVER_ALLOC);
	f_debug_mem_add(pointer, size, file, line);
	return pointer;
}

void f_debug_mem_print(uint min_allocs)
{
	uint i;
	printf("Memory report:\n----------------------------------------------\n");
	for(i = 0; i < f_allock_line_count; i++)
	{
		if(min_allocs < f_allock_lines[i].alocated)
		{
			printf("%s line: %u\n",f_allock_lines[i].file, f_allock_lines[i].line);
			printf(" - Bytes allocated: %u\n - Allocations: %u\n - Frees: %u\n\n", f_allock_lines[i].size, f_allock_lines[i].alocated, f_allock_lines[i].freed);
		}
	}
	printf("----------------------------------------------\n");
}


void f_debug_mem_reset()
{
/*	uint i;
	for(i = 0; i < f_allock_line_count; i++)
		free(f_allock_lines[i].allocs);*/
	f_allock_line_count = 0;
}

void exit_crash(uint i)
{
	uint *a = NULL;
	a[0] = 0;
}
