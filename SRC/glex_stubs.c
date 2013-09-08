#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <GL/gl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define t_value CAMLprim value

t_value ml_glshadersources (value shader, value cnt, value str, value len)
{
	CAMLparam4 (shader, cnt, str, len);
	CAMLlocal2 (some_str, some_len);
	if(Is_long(str) && Is_long(len))
	{
		printf("both str and len are null.\n");
		glShaderSource(Long_val(shader), Int_val(cnt), NULL, NULL);
	}
	else if(Is_long(str))
	{
		printf("only str is null.\n");
		some_len = Field(len,0);
		int size = Wosize_val(some_len);
		int * length = (int *)malloc(sizeof(int) * size);
		int i;
		for(i=0;i<size;++i)
		{
			length[i] = Int_val(Field(some_len, i));
		}
		glShaderSource(Long_val(shader), Int_val(cnt), NULL, length);
		for(i=0;i<size;++i)	
			free(length);
	}
	else if(Is_long(len))
	{
		printf("only len is null.\n");
		some_str = Field(str,0);
		int size = Wosize_val(some_str);
		int i=0;
		char ** vp;
		vp = (char **)malloc(sizeof(char *) * size);
		for(i=0;i<size;++i)
		{
			char *tmp = String_val(Field (some_str,i));
			vp[i] = (char *)malloc(sizeof(char) * strlen(tmp));
			memcpy(vp[i], tmp, strlen(tmp));
			printf("%s\n", vp[i]);
		}
		glShaderSource(Long_val(shader), Int_val(cnt), vp, NULL);
		for(i=0;i<size;++i)
			free(vp[i]);
		free(vp);
	}
	else
	{	
		printf("both len and str are not null.\n");
		some_str = Field(str,0);
		some_len = Field(len,0);
		int size = Wosize_val(some_str);
		int i=0;
		char ** vp;
		vp = (char **)malloc(sizeof(char *) * size);
		for(i=0;i<size;++i)
		{
			char *tmp = String_val(Field (some_str,i));
			vp[i] = (char *)malloc(sizeof(char) * strlen(tmp));
			memcpy(vp[i], tmp, strlen(tmp));
			printf("%s\n%s\n", vp[i],tmp);
		}
		int size2 = Wosize_val(some_len);
		int * length = (int *)malloc(sizeof(int) * size2);
		for(i=0;i<size2;++i)
		{
			length[i] = Int_val(Field(some_len, i));
		}
		glShaderSource(Long_val(shader), Int_val(cnt), vp, length);
		for(i=0;i<size;++i)
			free(vp[i]);
		free(vp);
		for(i=0;i<size2;++i)	
			free(length);
	}
	CAMLreturn (Val_unit);
}
