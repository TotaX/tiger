#include <stdarg.h>

void _tigermain();

long *_createArray(long init,long size)
{
	long *tmp,i;
	tmp = malloc((size+1)*sizeof(long));
	tmp[0] = size;
	for (i=0;i<size;i++)
		tmp[i+1]=init;
	return &tmp[1];
}

void _checkIndex(long i, long *a)
{
	if ( i<0 || i>=a[-1]) 
	{
		fprintf(stderr,"indice %d fuera de rango %d",i,a[-1]);
		exit(-1);
	}

}
long * _createRecord(long ctos,...)
{
    long *p,i;
    va_list va;
    p=malloc(ctos*sizeof(long));
    va_start(va,ctos);
    for (i=0; i<ctos,i++)
        p[i]=va_arg(va,long);
    return p;
}
void _checkNil(long *rec)
{
    if (rec==NULL) {fprint(stderr,"record no inicializado");exit(-1);}
}

int main()
{
		_tigermain();
		return 0;
}

