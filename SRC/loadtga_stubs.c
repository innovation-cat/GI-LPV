#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define t_value CAMLprim value

struct tgaheader
{
	unsigned char identsize;
	unsigned char cmaptype;
	unsigned char imagetype;
	unsigned char cmaporigin[2]; 
	unsigned char cmapsize[2];
	unsigned char cmapentrysize;
	unsigned char xorigin[2];
	unsigned char yorigin[2];
	unsigned char width[2];
	unsigned char height[2];
	unsigned char pixelsize;
	unsigned char descbyte;
};

struct tgaheader header;

int init (FILE *fp)
{
	fread((char *)&header, 1, sizeof(struct tgaheader), fp);
	if(header.imagetype != 2)
	{
		return 0;
	}
	if(header.pixelsize!=8 && header.pixelsize!=24 && header.pixelsize!=32)
	{
		return 0;
	}
	return 1;
}

int load_image(FILE *fp, unsigned char * buffer, unsigned int w, unsigned int h, unsigned int b)
{
	fseek(fp, header.identsize + sizeof(header), SEEK_SET);

	int buffer_size = w * h * b;

	if(fread(buffer, 1, buffer_size, fp) != buffer_size)
	{
		return 0;
	}

	if (b >= 3)
	{
		unsigned char *p = buffer;
		unsigned char* end = &buffer[buffer_size];
	    	for(; p < end; p += b)
		{
			unsigned char tmp = p[0];
			p[0] = p[2];
			p[2] = tmp;
		}
	}	
	return 1;
}

t_value load_image_c (value name)
{
	CAMLparam1 (name);
	CAMLlocal2 (pixels, res);
	char * filename = String_val (name);
	FILE *fp = fopen(filename, "rb");
	if (fp==NULL)
	{
		fclose(fp);
		caml_failwith("Could not load image.");
	}
	if(init(fp)==0)
	{
		fclose (fp);
		caml_failwith("unsupport image.");
	}
		
	int b = header.pixelsize / 8;
    	int w = header.width[0] + (header.width[1] << 8);
    	int h = header.height[0] + (header.height[1] << 8);
	
//	printf("%d %d %d\n", w, h, b);

	
	intnat dims[3] = {w, h, b};
	pixels = alloc_bigarray(BIGARRAY_UINT8 | BIGARRAY_C_LAYOUT, 3, NULL, dims);

	if (load_image(fp, (unsigned char *)(Data_bigarray_val(pixels)), w, h, b) == 0)
		caml_failwith ("load pixels unsuccessfully."); 

	fclose(fp);

	res = caml_alloc(4, 0);
	Store_field (res, 0, Val_int(w));
	Store_field (res, 1, Val_int(h));
	Store_field (res, 2, Val_int(b));
	Store_field (res, 3, pixels);

	CAMLreturn (res);
}
