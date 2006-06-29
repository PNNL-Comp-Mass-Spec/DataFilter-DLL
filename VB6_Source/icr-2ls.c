/************************************************************************
*                                                                       *
*                               ICR-2LS.DLL                             *
*                                                                       *
*		32 bit huge memory array alocation dll.                 *
*                                                                       *
*   Gordon Anderson                                                     *
*   Sep 2, 1995                                                         *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <math.h>
#include <float.h>
#include <memory.h>
#include <signal.h>
#include <windows.h>
#include <winbase.h>
#include <winnt.h>
#include <dos.h>
#include "icr-2ls.h"
#include "matrix.h"

// Create an array to hold memory handels...
#define   MAXHANDEL   100

typedef struct
{
	int   handle;
	int   address;
	int   size;
}  MEMHANDLE;

static MEMHANDLE   MemHandels[MAXHANDEL];
extern char  IsotopeFile[];
typedef void (*fptr)();

void Catcher(int *reglist)
{
	signal(SIGFPE, (fptr)Catcher);  //  ******reinstall signal handler
}


int _RTLENTRY _EXPFUNC _matherr (struct exception _FAR *__e)
{
	if (__e->type == OVERFLOW	)
	{
		__e->retval =  0.0;
	}
	else __e->retval = 0.0;
	return(1);
}

int _RTLENTRY _EXPFUNC  _matherrl(struct _exceptionl *e)
{
 	e->retval = 0.0;
	return(1);
}

BOOL WINAPI DllEntryPoint( HINSTANCE hinstDll,
									DWORD fdwRreason,
									LPVOID plvReserved)
{
	int   i;

	 switch(fdwRreason)
	 {
		 case DLL_PROCESS_ATTACH:
			 strcpy(IsotopeFile,"ISOTOPE.DAT");
			 for(i=0;i<MAXHANDEL;i++) MemHandels[i].handle = MemHandels[i].address = -1;   // Flag all pointers as free
			 signal(SIGFPE, (fptr)Catcher);
			 break;
		 case DLL_THREAD_ATTACH:
			 break;
		 case DLL_THREAD_DETACH:
			 break;
		 case DLL_PROCESS_DETACH:
			 for(i=0;i<MAXHANDEL;i++)
			 {
				 // free all handels...
				 if(MemHandels[i].handle != -1)
				 {
					 HugeErase(MemHandels[i].address);
					 MemHandels[i].handle = MemHandels[i].address = -1;
				 }
			 }
			 break;
	 }
	 return 1;
}

/************************************************************************
* HugeDim -                                                             *
************************************************************************/
int pascal _export HugeDim(int *lpdata,int recsize, int ubound)
{
	int    i;
	int    hmem;
	int    memadd;

	for(i=0;i<MAXHANDEL;i++) if(MemHandels[i].handle == -1) break;
	if(i==MAXHANDEL) return(-1);
	*lpdata = 0;
	hmem = (int)GlobalAlloc(GMEM_MOVEABLE,ubound*recsize);
	if(hmem==0) return(-2);
	memadd=(int)GlobalLock((HGLOBAL)hmem);
	if(memadd==0)
	{
		GlobalFree((HGLOBAL)hmem);
		return(-3);
	}
	*lpdata = memadd;
	for(i=0;i<MAXHANDEL;i++)
	{
		if(MemHandels[i].handle == -1)
		{
			MemHandels[i].handle = hmem;
			MemHandels[i].address = memadd;
			MemHandels[i].size = ubound*recsize;
			break;
		}
	}
	return(0);
}

/********************************************************************
* HugeErase -                                                       *
********************************************************************/
int pascal _export HugeErase(int lpdata)
{
	int   i,iStat,count;

   if(lpdata == 0) return(-1);
	if(lpdata == -1)
	{
		count = 0;
		for(i=0;i<MAXHANDEL;i++)
		{
			if(MemHandels[i].handle != -1)
			{
				while(GlobalUnlock((HGLOBAL)MemHandels[i].handle));
				GlobalFree((HGLOBAL)MemHandels[i].handle);
				MemHandels[i].handle = -1;
				MemHandels[i].address = -1;
				count++;
			}
		}
		return(count);
	}
	for(i=0;i<MAXHANDEL;i++)
	{
		if(MemHandels[i].address == lpdata)
		{
			while(GlobalUnlock((HGLOBAL)MemHandels[i].handle));
			iStat = (int)GlobalFree((HGLOBAL)MemHandels[i].handle);
			if(iStat == 0)
			{
				MemHandels[i].handle = -1;
				MemHandels[i].address = -1;
			}
			return(iStat);
		}
	}
	return(-1);
}

/************************************************************************
* HugeRedim -                                                           *
************************************************************************/
int pascal _export HugeRedim(int *lpdata,int recsize, int ubound)
{
	int   hmem,memadd;
	int   i;

	for(i=0;i<MAXHANDEL;i++)
	{
		if(MemHandels[i].address == *lpdata)
		{
			while(GlobalUnlock((HGLOBAL)MemHandels[i].handle));
			hmem=(int)GlobalReAlloc((HGLOBAL)MemHandels[i].handle,recsize*ubound,GMEM_MOVEABLE);
			if(hmem==0) return(-1);
			memadd=(int)GlobalLock((HGLOBAL)hmem);
			if(memadd==0)
			{
				GlobalFree((HGLOBAL)hmem);
				return(-2);
			}
			MemHandels[i].handle = hmem;
			MemHandels[i].address = memadd;
			MemHandels[i].size = ubound*recsize;
			*lpdata = memadd;
			return(0);
		}
	}
	return(-3);
}

int GetUbound(int lpdata)
{
	int   i;

	for(i=0;i<MAXHANDEL;i++)
	{
		if(MemHandels[i].address==lpdata) return(MemHandels[i].size);
	}
	return(0x7FFFFFFF);
}

/********************************************************************
* GetHugeEl -                                                       *
********************************************************************/
int pascal _export GetHugeEl(int *lpdata,int recsize, int element, BYTE *buffer)
{
	BYTE  *data;
	int   i,li,max;

	max=GetUbound(*lpdata);
	data = (BYTE *)*lpdata;
	li = recsize*element;
	for(i=0;i<recsize;i++) if(li<max) buffer[i] = data[li++];
	return(0);
}

/********************************************************************
* SetHugeEl -                                                       *
********************************************************************/
int pascal _export SetHugeEl(int *lpdata,int recsize, int element, BYTE *buffer)
{
	BYTE  *data;
	int   i,li,max;

	max=GetUbound(*lpdata);
	data = (BYTE *)*lpdata;
	li = recsize*element;
	for(i=0;i<recsize;i++) if(li<max) data[li++] = buffer[i];
	return(0);
}

void pascal _export CopyMem(char *src, char *dest, int nCount)
{
	int   i;

	for(i=0;i<nCount;i++)
	{
		dest[i]=src[i];
	}
}

/********************************************************************
* HugeFill -                                                        *
********************************************************************/
int pascal _export HugeFill(int *lpdata,int recsize, int element,int num, BYTE *buffer)
{
	BYTE    *data;
	int     i,li;

	data = (BYTE *)*lpdata;
	for(li=0;li<num;li++)
		for(i=0;i<recsize;i++) data[(li*recsize+element*recsize)+i]=buffer[i];
	return(0);
}

/********************************************************************
* HugeZero -                                                        *
********************************************************************/
int pascal _export HugeZero(int *lpdata,int Num)
{
	float   *data;
	int     i;

	data = (float *)*lpdata;
	for(i=0;i<Num;i++) data[i] = 0.0;
	return(0);
}

/********************************************************************
* HugeZeroRange -                                                        *
********************************************************************/
int pascal _export HugeZeroRange(int *lpdata,int Start,int Num)
{
	float        *data;
	int          i;
	unsigned int li;

	if(Start<0) return(0);
	if(Num<0) return(0);
	data = (float *)*lpdata;
	li=GetUbound(*lpdata)/4;
	for(i=Start;i<Start+Num;i++) if(i<li) data[i] = 0.0;
	return(0);
}

/************************************************************************
* HugeLoadInt -                                                         *
************************************************************************/
int pascal _export HugeLoadInt(int *lpdata, int Num, int hLoadFile, int pos, int ByteOrder)
{
	int                 BytesRead;          // number of bytes read from the file
	int                 element;            // element in the huge array
	int                 Size;
	short int           iVals[1024];
	int                 i;
	float               *data;
	BYTE                *ch;
	BYTE                by;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		BytesRead = (int)_lread(hLoadFile, (BYTE *)iVals, sizeof(short int)*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		if(ByteOrder)
		{
			for(i=0;i<BytesRead/sizeof(short int);i++)
			{
				ch = (BYTE *)&iVals[i];
				by    = ch[0];
				ch[0] = ch[1];
				ch[1] = by;
			}
		}
		// Now convert them to floats
		for(i = 0; i<BytesRead/sizeof(short int);i++) data[element+i] = iVals[i];
		for(;i<Size;i++) data[element+i] = 0.0;
	}
	return(element);
}

/************************************************************************
* HugeLoadLong -                                                        *
************************************************************************/
int pascal _export HugeLoadLong(int *lpdata, int Num, int hLoadFile,int pos,int ByteOrder)
{
	int                 BytesRead;          // number of bytes read from the file
	int                 element;            // element in the huge array
	int                 Size;
	int                 iVals[1024];
	int                 i;
	float               *data;
	BYTE                *ch;
	BYTE                by;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the ints from file
		BytesRead = (int)_lread(hLoadFile, (BYTE *)iVals, sizeof(long)*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		if(ByteOrder)
		{
			for(i=0;i<BytesRead/sizeof(long);i++)
			{
				ch = (BYTE *)&iVals[i];
				by    = ch[0];
				ch[0] = ch[3];
				ch[3] = by;
				by    = ch[1];
				ch[1] = ch[2];
				ch[2] = by;
			}
		}
		// Now convert them to floats
		for(i = 0; i<BytesRead/sizeof(long);i++) data[element+i] = iVals[i];
		for(;i<Size;i++) data[element+i] = 0.0;
	}
	return(element);
 }

/************************************************************************
* HugeLoadLongBig -                                                     *
************************************************************************/
int pascal _export HugeLoadLongBig(int *lpdata, int Num, int hLoadFile,double pos,int ByteOrder)
{
	int                 BytesRead;          // number of bytes read from the file
	int                 element;            // element in the huge array
	int                 Size;
	int                 iVals[1024];
	unsigned int                 i;
	float               *data;
	BYTE                *ch;
	BYTE                by;
        unsigned int                 addlow,addhigh;

//   addhigh = pos / 2000000000.0;
//   addlow = pos - (double)addhigh * 2000000000.0;
	data = (float *)*lpdata;

//        _llseek(hLoadFile,0,FILE_BEGIN);
//	if(pos > 0.0) 
//        {
//           for(i=0;i<addhigh;i++) _llseek(hLoadFile, 2000000000L, FILE_CURRENT);
//           _llseek(hLoadFile, addlow, FILE_CURRENT);
//        }

        addhigh = pos / 4294967296.0;
        addlow = pos - (double)addhigh * 4294967296.0;
        SetFilePointer((HANDLE)hLoadFile,addlow,&addhigh,FILE_BEGIN);

	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the ints from file
		BytesRead = (int)_lread(hLoadFile, (BYTE *)iVals, sizeof(long)*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		if(ByteOrder)
		{
			for(i=0;i<BytesRead/sizeof(long);i++)
			{
				ch = (BYTE *)&iVals[i];
				by    = ch[0];
				ch[0] = ch[3];
				ch[3] = by;
				by    = ch[1];
				ch[1] = ch[2];
				ch[2] = by;
			}
		}
		// Now convert them to floats
		for(i = 0; i<BytesRead/sizeof(long);i++) data[element+i] = iVals[i];
		for(;i<Size;i++) data[element+i] = 0.0;
	}
	return(element);
 }

/************************************************************************
* HugeLoad24 -                                                          *
************************************************************************/
int pascal _export HugeLoad24(int *lpdata, int Num, int hLoadFile,int pos,int ByteOrder)
{
	int                 BytesRead;          // number of bytes read from the file
	int                 element;            // element in the huge array
	int                 Size,lVal;
	BYTE                Val[1024*3];
	int                 i;
	float               *data;
	BYTE                *ch;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the ints from file
		BytesRead = (int)_lread(hLoadFile, (BYTE *)Val, 3*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		if(ByteOrder)
		{
			for(i=0;i<BytesRead/3;i++)
			{
				ch = (BYTE *)&lVal;
				ch[0] = (BYTE)Val[i*3];
				ch[1] = (BYTE)Val[i*3+1];
				ch[2] = (BYTE)Val[i*3+2];
				ch[3] = 0;
				if((ch[2]&0x80) != 0) ch[3] = 0xff;
				data[i+element] = lVal;
			}
		}
		else
		{
			for(i=0;i<BytesRead/3;i++)
			{
				ch =   (BYTE *)&lVal;
				ch[0] = (BYTE)Val[i*3+2];
				ch[1] = (BYTE)Val[i*3+1];
				ch[2] = (BYTE)Val[i*3];
				ch[3] = 0;
				if((ch[2]&0x80) != 0) ch[3] = 0xff;
				data[i+element] = lVal;
			}
		}
		for(;i<Size;i++) data[i+element] = 0.0;
	}
	return(element);
}

/************************************************************************
* HugeLoadFloat -                                                       *
************************************************************************/
int pascal _export HugeLoadFloat(int *lpdata, int Num, int hLoadFile,int pos)
{
	int                 BytesRead;          // number of bytes read from the file
	float               *data;              // pointer to array element
	int                 element;            // element in the huge array
	int                 Size;
	float               fVals[1024];
	int                 i;

	data = (float *)*lpdata;
	if(hLoadFile==0) return(HA_FILEREADERROR);
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the floats from file
		BytesRead = (int)_lread(hLoadFile, (BYTE *)fVals, sizeof(float)*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		for(i=BytesRead/sizeof(float);i<Size;i++) fVals[i] = 0.0;
//		memcpy(&data[element], fVals, sizeof(float) * Size);   // copy data
for(i=0;i<Size;i++) data[i+element]=fVals[i];
	}
	return(element);
 }


/************************************************************************
* HugeReverseOrder -                                                    *
************************************************************************/
int pascal _export HugeReverseOrder(int *lpdata, int Num, int Size)
{
	BYTE   *ch;
	BYTE   *chtmp;
	int    i;

	ch = (BYTE *)*lpdata;
	chtmp = (BYTE *)calloc(Size,sizeof(BYTE));
	if(!chtmp) return(-1);
	for(i=0;i<Num/2;i++)
	{
		memcpy(chtmp,&ch[i*Size], Size);
		memcpy(&ch[i*Size],&ch[(Num-i-1)*Size],Size);
		memcpy(&ch[(Num-i-1)*Size],chtmp,Size);
	}
   free(chtmp);
	return(0);
}
/************************************************************************
* HugeLoadSunFloat -                                                    *
************************************************************************/
int pascal _export HugeLoadSunFloat(int *lpdata, int Num, int hLoadFile,int pos, int Ftype)
{
	int                 BytesRead;          // number of bytes read from the file
	float               *data;              // pointer to array element
	int                 element;            // element in the huge array
	int                 Size;
	int                 FP;
	int                 i;
	BYTE                *ch;
	BYTE                by;
	float               fVals[1024],temp;
	float               Phase[512];

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the floats from file
		if(Ftype == 1)	BytesRead = (int)_lread(hLoadFile, (BYTE *)fVals, sizeof(float)*(long)Size);
		if(Ftype == 2)	BytesRead = (int)_lread(hLoadFile, (BYTE *)fVals, sizeof(float)*(long)Size/2l);
		if(Ftype == 3)
		{
			BytesRead = (int)_lread(hLoadFile, (BYTE *)fVals, sizeof(float)*(long)Size/2l);
			FP = tell(hLoadFile);
			lseek(hLoadFile, sizeof(float)*(Num/2l - Size/2l), SEEK_CUR);
			_lread(hLoadFile, (BYTE *)Phase, sizeof(float)*(long)Size/2l);
			lseek(hLoadFile, FP, SEEK_SET);
		}
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		// Now convert to pc byte order
		for(i=0;i<BytesRead/sizeof(float);i++)
		{
			ch = (BYTE *)&fVals[i];
			by    = ch[0];
			ch[0] = ch[3];
			ch[3] = by;
			by    = ch[1];
			ch[1] = ch[2];
			ch[2] = by;
                        fVals[i] *= 1.0;
                        if((_status87() & (EM_INVALID|EM_DENORMAL|EM_ZERODIVIDE|EM_OVERFLOW|EM_UNDERFLOW)) != 0) fVals[i]=0.0;
		}
		for(;i<Size;i++) fVals[i] = 0.0;
		if((Ftype == 2)||(Ftype == 3)) for(i=Size/2-1;i>=0;i--)
		{
			temp = fVals[i];
			fVals[i] = 0.0;
			fVals[i*2] = temp;
		}
		if(Ftype == 3) for(i=0;i<Size;i+=2)
		{
			ch = (BYTE *)&Phase[i/2];
			by    = ch[0];
			ch[0] = ch[3];
			ch[3] = by;
			by    = ch[1];
			ch[1] = ch[2];
			ch[2] = by;
			fVals[i+1] = fVals[i] * sin(Phase[i/2]);
			fVals[i]  *= cos(Phase[i/2]);
		}
		for(i=0;i<Size;i++) data[i+element]=fVals[i];
	}
	return(element);
 }

/************************************************************************
* HugeSaveInt -                                                         *
************************************************************************/
int pascal _export HugeSaveInt(int *lpdata, int Num, int hLoadFile,int pos)
{
	int                 BytesSent;          // number of bytes sent to file
	int                 element;            // element in the huge array
	int                 Size;
	int                 iVals[1024],i;
	float               *data;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Now convert them to ints
		for(i = 0; i<Size;i++) iVals[i] = data[i+element];
		// Write ints to file
		BytesSent = (int)_lwrite(hLoadFile, (LPCSTR)iVals, sizeof(int)*Size);
		if(BytesSent < 0)
		{
			return HA_FILEREADERROR;
		}
	}
	return(element);
}

/************************************************************************
* HugeSaveFloat -                                                       *
************************************************************************/
int pascal _export HugeSaveFloat(int *lpdata, int Num, int hLoadFile,int pos)
{
	int                 BytesSent;          // number of bytes sent to file
	int                 element;            // element in the huge array
	int                 Size;
	float               fVals[1024];
	float               *data;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		memcpy(fVals,&data[element], sizeof(float) * (long)Size);   // copy data
		// Write floats to file
		BytesSent = (int)_lwrite(hLoadFile, (LPCSTR)&data[element], sizeof(float)*Size);
		if(BytesSent < 0)
		{
			return HA_FILEREADERROR;
		}
	}	return(Num);
}

/************************************************************************
* HugeExtract -                                                         *
*                                                                       *
* Options:                                                              *
*				1 = Every nth point, comb filter                            *
*				2 = Every nth point, alternate max/min                      *
*           3 = Extract magnitude freq data                             *
*           4 = Extract magnitude freq data, minimum                    *
*           5 = Every nth point, max value                              *
*           The following options are for X,Y pair data                 *
*           6 = Every nth point, max value, Y                           *
*           7 = Every nth point, comb filter, X                         *
************************************************************************/
int pascal _export HugeExtract(int *lpdata, float *fVals, float *max, float *min, int Start, int Stop, int maxN, int option)
{
	float                lmin,lmax;
	float                temp,temp1,temp2;
	int                  skip,li;
	int                  i,j,LastMax = -1;

	skip = ((Stop-Start)/maxN) + 1L;
	i = 0;
	if(option==6)
	{
		GetHugeEl(lpdata,sizeof(float),Start*2 -1,(BYTE *)&temp);
		*min=*max=temp;
	}
	if((option==1)||(option==2)||(option==5))
	{
		GetHugeEl(lpdata,sizeof(float),Start,(BYTE *)&temp);
		*min=*max=temp;
	}
	if((option==3)||(option==4))
	{
		if(GetHugeEl(lpdata,sizeof(float),2L*Start,(BYTE FAR *)&temp)!=0) temp=0.0;
		if(GetHugeEl(lpdata,sizeof(float),2L*Start+1L,(BYTE FAR *)&temp1)!=0) temp1=0.0;
		*min=*max=sqrt((double)temp*(double)temp + (double)temp1*(double)temp1);
	}
	for(li=Start;li<Stop;li+=skip)
	{
		switch(option)
		{
			case 6:
				GetHugeEl(lpdata,sizeof(float),li*2 + 1,(BYTE *)&temp);
				lmax  = temp;
				for(j=0;j<skip;j++)
				{
					if(((long)j+li) >= Stop) break;
					GetHugeEl(lpdata,sizeof(float),(li+j)*2 + 1,(BYTE *)&temp);
					if(temp > lmax) lmax = temp;
				}
				fVals[i] = lmax;
				break;
			case 7:
				GetHugeEl(lpdata,sizeof(float),li*2,(BYTE *)&fVals[i]);
				break;
			case 5:
				GetHugeEl(lpdata,sizeof(float),li,(BYTE *)&temp);
				lmax  = temp;
				for(j=0;j<skip;j++)
				{
					if(((long)j+li) >= Stop) break;
					GetHugeEl(lpdata,sizeof(float),li+j,(BYTE *)&temp);
					if(temp > lmax) lmax = temp;
				}
				fVals[i] = lmax;
				break;
			case 1:
				GetHugeEl(lpdata,sizeof(float),li,(BYTE *)&fVals[i]);
				break;
			case 2:
				GetHugeEl(lpdata,sizeof(float),li,(BYTE *)&temp);
				lmax =lmin = temp;
				for(j=0;j<skip;j++)
				{
					if(((long)j+li) >= Stop) break;
					GetHugeEl(lpdata,sizeof(float),li+j,(BYTE *)&temp);
					if(temp > lmax) lmax = temp;
					if(temp < lmin) lmin = temp;
				}
				if(LastMax)
				{
					fVals[i] = lmax;
					LastMax = 0;
				}
				else
				{
					fVals[i] = lmin;
					LastMax = -1;
				}
				break;
			case 3:
				if(GetHugeEl(lpdata,sizeof(float),2L*li,(BYTE FAR *)&temp1)!=0)temp1=0.0;
				if(GetHugeEl(lpdata,sizeof(float),2L*li+1L,(BYTE FAR *)&temp2)!=0)temp2=0.0;
				lmax =sqrt((double)temp1*(double)temp1 + (double)temp2*(double)temp2);
				for(j=0;j<skip;j++)
				{
					if(((long)j+li) >= Stop) break;
					if(GetHugeEl(lpdata,sizeof(float),2L*(li+j),(BYTE FAR *)&temp1)!=0)temp1=0.0;
					if(GetHugeEl(lpdata,sizeof(float),2L*(li+j)+1L,(BYTE FAR *)&temp2)!=0)temp2=0.0;
					temp =sqrt((double)temp1*(double)temp1 + (double)temp2*(double)temp2);
					if(temp > lmax) lmax = temp;
				}
				fVals[i] = lmax;
				break;
			case 4:
				if(GetHugeEl(lpdata,sizeof(float),2L*li,(BYTE FAR *)&temp1)!=0)temp1=0.0;
				if(GetHugeEl(lpdata,sizeof(float),2L*li+1L,(BYTE FAR *)&temp2)!=0)temp2=0.0;
				lmax =sqrt((double)temp1*(double)temp1 + (double)temp2*(double)temp2);
				for(j=0;j<skip;j++)
				{
					if(((long)j+li) >= Stop) break;
					if(GetHugeEl(lpdata,sizeof(float),2L*(li+j),(BYTE FAR *)&temp1)!=0)temp1=0.0;
					if(GetHugeEl(lpdata,sizeof(float),2L*(li+j)+1L,(BYTE FAR *)&temp2)!=0)temp2=0.0;
					temp =sqrt((double)temp1*(double)temp1 + (double)temp2*(double)temp2);
					if(temp < lmax) lmax = temp;
				}
				fVals[i] = lmax;
				break;
			default:
				break;
		}
		if(fVals[i] > *max) *max = fVals[i];
		if(fVals[i] < *min) *min = fVals[i];
		i++;
	}
	return((int)((Stop-Start)/skip));
}

int pascal _export CurvReg(double *x, double *y, unsigned int n, double *terms, unsigned int nterms, double *mse)
{
	 MATRIX *At,*B,*At_Ai_At,*Z,*OUTa;
	 int i,j;
	 float *w;
	 double **at,**b,**z,**out;
	 double yfit,xpow;

	 // weights
	 w = (float *)calloc(n,sizeof(double));
	 if(!w) return(-1);
	 for(i = 0 ; i < n ; i++) w[i] = 1.0;

/* weighted powers of x matrix transpose = At */
	 At = matrix_allocate(nterms+1,n,sizeof(double));
	 at = (double **) At->ptr;
	 for(i = 0 ; i < n ; i++) {
		  at[0][i] = w[i];
		  for(j = 1 ; j < (nterms + 1) ; j++)
				at[j][i] = at[j-1][i] * x[i];
	 }

/* Z = weighted y vector */
	 Z = matrix_allocate(n,1,sizeof(double));
	 z = (double **) Z->ptr;
	 for(i = 0 ; i < n ; i++) z[i][0] = w[i] * y[i];

	 At_Ai_At = matrix_mult(matrix_invert(matrix_mult(At,
									matrix_transpose(At))),At);

	 B = matrix_mult(At_Ai_At,Z);
	 b = (double **)B->ptr;

/* make a matrix with the fit y and the difference */
	 OUTa = matrix_allocate(2,n,sizeof(double));
	 out = (double **) OUTa->ptr;

/* calculate the least squares y values */
	 *mse=0.0;
	 for(i = 0 ; i < n ; i++) {
		  terms[0] = b[0][0];
		  yfit = b[0][0];
		  xpow = x[i];
		  for(j = 1 ; j <= nterms ; j++) {
				terms[j] = b[j][0];
				yfit += b[j][0]*xpow;
				xpow = xpow * x[i];
		  }
		  out[0][i] = yfit;
		  out[1][i] = y[i]-yfit;
		  *mse += y[i]-yfit;
	 }
	 matrix_free(At);
	 matrix_free(At_Ai_At);
	 matrix_free(B);
	 matrix_free(Z);
	 matrix_free(OUTa);
	 return(0);
}

int pascal _export Int2Float(int *lpdata, int Num)
{
	int                 element;            // element in the huge array
	int                 i;
	float               *data;
	short int           *iptr;

	iptr = (short int *)*lpdata;
	data = (float *)*lpdata;
	for(element = (Num-1024L); element >= 0L ; element-=1024)
	{
		// Now convert ints to floats
		for(i = 1023; i>=0;i--) data[i+element] = iptr[i+element];
	}
	return(0);
}

double sgn(double a)
{
	if(a<0.0) return(-1.0);
	return(1.0);
}

//
// determins real roots of:
//
//     x^3 + ax^2 + bx + c =0
//
//     Returns the number of real roots.
//
int FAR pascal _export CubeRoots(double a, double b, double c, double *roots)
{
	#define PI 3.1415926
	double  Q,R;
	double  A,B;
	double  theta;

	Q = (a*a - 3.0*b)/9.0;
	R = (2.0*a*a*a - 9.0*a*b + 27.0*c)/54.0;
	if((R*R)<(Q*Q*Q))
	{
		// here with three real roots.
		theta = acos(R/sqrt(Q*Q*Q));
		roots[0] = -2.0*sqrt(Q)*cos(theta/3.0) - a/3.0;
		roots[1] = -2.0*sqrt(Q)*cos((theta+2.0*PI)/3.0) - a/3.0;
		roots[2] = -2.0*sqrt(Q)*cos((theta-2.0*PI)/3.0) - a/3.0;
		return(3);
	}
	// here with one real root...
	A = -1.0*sgn(R)*pow((fabs(R)+sqrt(fabs(R*R-Q*Q*Q))),1.0/3.0);
	if(A != 0.0) B=Q/A;
	else B = 0.0;
	roots[0] = (A+B)-a/3.0;
	return(1);
}

int pascal _export SearchMW(int *C, int *H,int *N, int *O,int *S,double Mass,double MonoMass)
{
#define  C1     12.0111069831375
#define  H1     1.00797557532606
#define  O1     15.9993703627226
#define  N1     14.0067248319416
#define  S1     32.0643887269403
#define  Cmono  12.0
#define  Hmono  1.0078246
#define  Nmono  14.0030732
#define  Omono  15.9949141
#define  Smono  31.97207
int   Cn,Cs,Cr;
int   Hn,Hs,Hr;
int   Nn,Ns,Nr;
int   On,Os,Or;
int   Sn,Ss,Sr;
float  Percent;
double MassError;
double M,MM,E;

	MassError = Mass;
	Percent = 0.1;
	Cr = (long)((float)*C * Percent +.5);
	Hr = (long)((float)*H * Percent +.5);
	Nr = (long)((float)*N * Percent +.5);
	Or = (long)((float)*O * Percent +.5);
	Sr = (long)((float)*S * Percent +.5);
	if (Cr > 50)  Cr = 50;
	if (Hr > 100) Hr = 100;
	if (Nr > 20)  Hr = 20;
	if (Or > 20)  Or = 20;
	if (Sr > 2)   Sr = 2;
	for(Cn= *C-Cr;Cn<= *C + Cr;Cn++)
	{
		for(Hn= *H-Hr;Hn<= *H + Hr;Hn++)
		{
			for(Nn= *N-Nr;Nn<= *N + Nr;Nn++)
			{
				for(On= *O-Or;On<= *O + Or;On++)
				{
					for(Sn= *S-Sr;Sn<= *S + Sr;Sn++)
					{
						E = 0.0;
						if (Mass != 0.0)
						{
							M = Cn * C1 + Hn * H1 + Nn * N1 + On * O1 + Sn * S1;
							E = (M - Mass) * (M - Mass);
						}
						if (MonoMass != 0.0)
						{
							MM = Cn * Cmono + Hn * Hmono + Nn * Nmono + On * Omono + Sn * Smono;
							E = E + (MM - MonoMass) * (MM - MonoMass);
						}
						E = sqrt(E);
						if(E < MassError)
						{
							Cs = Cn;
							Hs = Hn;
							Ns = Nn;
							Os = On;
							Ss = Sn;
							MassError = E;
						}
					}
				}
			}
		}
	}
	*C = Cs;
	*H = Hs;
	*N = Ns;
	*O = Os;
	*S = Ss;
	return(0);
}

typedef struct
{
   unsigned int  NumScans;
   unsigned int  UK1,UK2,UK3;
   unsigned int  CB;
   unsigned int  StartOfData;
   unsigned int  EndOfCB;
} LCQpointers;

int pascal _export LCQlocate(int hfile, int *ScanNum, int *event, int *segment, float *Start, float *Stop, int *NumPoints)
{
	int           i;
	char          e,s;
	LCQpointers   LCQp;
	unsigned int  fp,thisscan;

	// First load the pointer to the control blocks...
	_llseek(hfile,0x558,SEEK_SET);
	if(sizeof(LCQpointers) != (int)_lread(hfile, &LCQp, sizeof(LCQpointers))) return(-1);
	if(((LCQp.EndOfCB-LCQp.CB)/LCQp.NumScans) != 0x120) return(-1);
//   if(*ScanNum > LCQp.NumScans) return(-1);
	for(i=0;i<LCQp.NumScans;i++)
	{
//      fp = 0x120 * (ScanNum-1) + LCQp.CB;
		fp = 0x120 * i + LCQp.CB;
		_llseek(hfile,fp+16,SEEK_SET);
		if(sizeof(int) != (int)_lread(hfile, &thisscan, sizeof(int))) return(-1);
		if(*ScanNum!=0) if(thisscan != *ScanNum) continue;
		_llseek(hfile,fp+46,SEEK_SET);
		if(sizeof(char) != (int)_lread(hfile, &s, sizeof(char))) return(-1);
		if(sizeof(char) != (int)_lread(hfile, &e, sizeof(char))) return(-1);
		if(*event!=0) if(e!=*event) continue;
		if(*segment!=0) if(s!=*segment) continue;
		_llseek(hfile,fp+0x118,SEEK_SET);
		if(sizeof(int) != (int)_lread(hfile, &fp, sizeof(int))) return(-1);
		fp += LCQp.StartOfData;
		_llseek(hfile, fp+4, SEEK_SET);
		_lread(hfile, Start, sizeof(float));
		_lread(hfile, Stop, sizeof(float));
		_llseek(hfile, fp+20, SEEK_SET);
		_lread(hfile, NumPoints, sizeof(int));
		*NumPoints /= 4;
		*event=e;
		*segment=s;
		*ScanNum=thisscan;
		return(fp-*NumPoints*4);
	}
	return(-1);
}


// LCQ data file routines
// Return -1 if scan not found or error, else returns a pointer to the start of data.
int pascal _export LCQlocateold(int hfile, int ScanNum, float *Start, float *Stop, int *NumPoints)
{
	static int     current=0,oldhfile=0;
	int            i,j,k,pos=0,BytesRead,realine;
	unsigned char  Buffer[1024];

	_llseek(hfile, 0, SEEK_SET);
	// now loop through the data looking for 16 bytes of FF
	i = 0;
//	if ((oldhfile != hfile) || (current >= ScanNum))
//   {
		current=0;
//	}
	oldhfile=hfile;
	for(;;)
	{
		BytesRead = (int)_lread(hfile, &Buffer[i], sizeof(char)*(long)(1024-i));
		if(BytesRead<=0) break;
		for(i=0;i<1024;i++)
		{
			if(Buffer[i]==0xFF)
			{
				for(j=i;j<(i+16);j++)
				{
					if(Buffer[j]!=0xFF) break;
					if(j>=1024) break;
				}
				if((i+16)==j)
				{
//					if(current!=ScanNum)
//					{
//						current++;
//						i+=15;
//					}
//					else
					{
						// here if the scan was found!
						// Now read the start, stop, and length
						pos += i;
for(;;)
{
						_llseek(hfile, pos-20, SEEK_SET);
						_lread(hfile, Start, sizeof(float));
						_lread(hfile, Stop, sizeof(float));
						_llseek(hfile, pos-4, SEEK_SET);
						_lread(hfile, NumPoints, sizeof(int));
						if(current==ScanNum)
						{
							pos -= (*NumPoints + 24);
							*NumPoints /= 4;
							return(pos);
						}
						current++;
//						pos +=  (*NumPoints + 40) * ScanNum;
						// Now make sure the 16 bytes of FF are present at the end
						// of this scan
						_llseek(hfile, pos + (*NumPoints + 40), SEEK_SET);
						if((int)_lread(hfile, Buffer, sizeof(char)*16) != 16) return(-1);
						realine=0;
						for(k=0;k<16;k++) if(Buffer[k]!=0xff) realine=-1; // return(-1);
						if (realine)
						{
							pos += 16;
							_llseek(hfile, pos , SEEK_SET);
							i=1024;
							BytesRead=0;
							break;
						}
						else
						{
							pos +=  (*NumPoints + 40);
						}
}
//						*NumPoints /= 4;
//						return(pos);
					}
				}
			}
		}
		// Now look for FFs at the end of buffer and aline them with the top
		if(BytesRead==0) { i=0; continue;}
		pos += BytesRead;
		for(i=0;i<1024;i++)
		{
			if(Buffer[1023-i]==0xFF) Buffer[i]=0xFF;
			else break;
		}
	}
	return(-1);
}

int pascal _export LCQload(int *lpdata, int Num, int hLoadFile,int pos)
{
	int                 BytesRead;          // number of bytes read from the file
	int                 element;            // element in the huge array
	int                 Size;
	int                 iVals[1024];
	int                 i;
	float               *data;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the ints from file
		BytesRead = (int)_lread(hLoadFile, (BYTE *)iVals, sizeof(long)*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		for(i = 0; i<BytesRead/sizeof(long);i++)
		{
			data[(element+i)*2] = data[(element+i)*2] + (iVals[i] & 0x07FFFFFFF);
			data[(element+i)*2 + 1] = 0;
		}
		for(;i<Size;i++) data[element+i] = 0.0;
	}
	return(element);
}

typedef struct
{
	char res1;
	unsigned short int Amp;
	char res2;
	unsigned short int Mint;
	unsigned short int Mfrac;
} LCQrecord;

int LastFileHandle=-1;
int LastScan=-1;
int LastFilePos=-1;

int pascal _export LCQcentroidNumPeaks(int hLoadFile,int Scan)
{
	int   i,NumPoint,StartPos,EndPos,Pos,NumScans;
	short int j;
	unsigned short int k;
	float Mass,LastMass;
	LCQrecord lr;
	LCQpointers LCQp;

	// First load the pointer to the control blocks...
	_llseek(hLoadFile,0x558,SEEK_SET);
	if(sizeof(LCQpointers) != (int)_lread(hLoadFile, &LCQp, sizeof(LCQpointers))) return(-1);
   if(Scan >= LCQp.NumScans) return(-1);
	if(((LCQp.EndOfCB-LCQp.CB)/LCQp.NumScans) != 0x120) return(-1);
	Pos = LCQp.CB + Scan * 0x120;
	_llseek(hLoadFile, Pos + 0x11C, SEEK_SET);
	_lread(hLoadFile, &NumPoint, sizeof(int));
	return(NumPoint);
}

int pascal _export LCQcentroidGetPeaks(int hLoadFile,int Scan,float *X, float *Y)
{
	int   i,NumPoint,StartPos,EndPos,Pos,NumScans;
	short int j;
	unsigned short int k;
	float Mass,LastMass;
	LCQrecord lr;
	LCQpointers LCQp;

	// First load the pointer to the control blocks...
	_llseek(hLoadFile,0x558,SEEK_SET);
	if(sizeof(LCQpointers) != (int)_lread(hLoadFile, &LCQp, sizeof(LCQpointers))) return(-1);
	if(((LCQp.EndOfCB-LCQp.CB)/LCQp.NumScans) != 0x120) return(-1);
	Pos = LCQp.CB + Scan * 0x120;
	_llseek(hLoadFile, Pos + 0x11C, SEEK_SET);
	_lread(hLoadFile, &NumPoint, sizeof(int));
	// Get pointer to the data
	_llseek(hLoadFile, Pos + 0x118, SEEK_SET);
	_lread(hLoadFile, &Pos, sizeof(int));
	Pos += LCQp.StartOfData;
	_llseek(hLoadFile, Pos, SEEK_SET);
	for(i=0;i<NumPoint;i++)
	{
		_lread(hLoadFile, &lr, sizeof(LCQrecord));
		Mass = (float)lr.Mint + ((float)lr.Mfrac)/65535.0;
		Y[i] = lr.Amp + ((long)lr.res2 * 65536L);
		if (lr.res1 != 0) Y[i] *= 256L;
		X[i]=Mass;
	}
	return(NumPoint);
}

int pascal _export LCQscanINFO(int hLoadFile,int Scan,int *Stype,float *Smz,float *Pmz)
{
	int   Pos;
	LCQpointers LCQp;

	// First load the pointer to the control blocks...
	_llseek(hLoadFile,0x558,SEEK_SET);
	if(sizeof(LCQpointers) != (int)_lread(hLoadFile, &LCQp, sizeof(LCQpointers))) return(-1);
	if(((LCQp.EndOfCB-LCQp.CB)/LCQp.NumScans) != 0x120) return(-1);
	// Get pointer to control block
	Pos = LCQp.CB + 0x120 * Scan;
	_llseek(hLoadFile, Pos+20, SEEK_SET);
	_lread(hLoadFile, Stype, sizeof(int));
	_llseek(hLoadFile, Pos+0x24, SEEK_SET);
	_lread(hLoadFile, Smz, sizeof(float));
	_llseek(hLoadFile, Pos+0x58, SEEK_SET);
	_lread(hLoadFile, Pmz, sizeof(float));
	return(0);
}

int pascal _export LCQnumScans(int hLoadFile)
{
	int   Pos;
	LCQpointers LCQp;

	// First load the pointer to the control blocks...
	_llseek(hLoadFile,0x558,SEEK_SET);
	if(sizeof(LCQpointers) != (int)_lread(hLoadFile, &LCQp, sizeof(LCQpointers))) return(-1);
   return(LCQp.NumScans);
}

int pascal _export LCQmzRange(int hLoadFile,int Scan,float *Min, float *Max)
{
	int   Pos;
	LCQpointers LCQp;

	// First load the pointer to the control blocks...
	_llseek(hLoadFile,0x558,SEEK_SET);
	if(sizeof(LCQpointers) != (int)_lread(hLoadFile, &LCQp, sizeof(LCQpointers))) return(-1);
	if(((LCQp.EndOfCB-LCQp.CB)/LCQp.NumScans) != 0x120) return(-1);
	// Get pointer to control block
	Pos = LCQp.CB + 0x120 * Scan;
	_llseek(hLoadFile, Pos+8, SEEK_SET);
	// Read range
	_lread(hLoadFile, Min, sizeof(float));
	_lread(hLoadFile, Max, sizeof(float));
	return(0);
}

void pascal _export OrtechChan(char *str, short int *chan, int *chantime)
{
	sscanf(str,"%x %lx",chan,chantime);
}

typedef struct
{
	long  res1,res2,res3;
	long  TotalScans;
	long  res4;
	long  ControlBlock;
	long  StartOfData;
	long  res5,res6;
	float res7;
	float StartMZ,StopMZ;
} TSQhdr;

typedef struct
{
	long  OffsetToData;
	long  res1[4];
	float res2[3];
	float StartMZ,StopMZ;
	float res3[2];
	long  res4;
	long  ScanNumber;
	long  res5[3];
	long  NumPoints;
	long  res6[5];
	float M1;
	long  res7[8];
} TSQcb;

typedef struct
{
	int   Amp;
	float M;
} TSQrecord;

int pascal _export TSQcentroidNumPeaks(int hLoadFile,int Scan)
{
	int       Pos;
	TSQhdr    h;
	TSQcb     cb;

	_llseek(hLoadFile,0x0000,SEEK_SET);
	// Get the file header...
	if(sizeof(TSQhdr) != (int)_lread(hLoadFile, &h, sizeof(TSQhdr))) return(-1);
	// Now calculate the position of this scans control block...
	if (Scan > h.TotalScans) return(-1);
	Pos = (Scan) * sizeof(TSQcb) +h.ControlBlock;
	_llseek(hLoadFile,Pos,SEEK_SET);
	if(sizeof(TSQcb) != (int)_lread(hLoadFile, &cb, sizeof(TSQcb))) return(-1);
	return(cb.NumPoints);
}

int pascal _export TSQmzRange(int hLoadFile,int Scan,float *Min, float *Max)
{
	int       Pos;
	TSQhdr    h;
	TSQcb     cb;

	_llseek(hLoadFile,0x0000,SEEK_SET);
	// Get the file header...
	if(sizeof(TSQhdr) != (int)_lread(hLoadFile, &h, sizeof(TSQhdr))) return(-1);
	// Now calculate the position of this scans control block...
	if (Scan > h.TotalScans) return(-1);
	Pos = (Scan) * sizeof(TSQcb) + h.ControlBlock;
	_llseek(hLoadFile,Pos,SEEK_SET);
	if(sizeof(TSQcb) != (int)_lread(hLoadFile, &cb, sizeof(TSQcb))) return(-1);
	*Min = cb.StartMZ;
	*Max = cb.StopMZ;
	return(0);
}


int pascal _export TSQcentroidGetPeaks(int hLoadFile,int Scan,float *X, float *Y)
{
	int       Pos,i;
	TSQhdr    h;
	TSQcb     cb;
	TSQrecord tr;

	_llseek(hLoadFile,0x0000,SEEK_SET);
	// Get the file header...
	if(sizeof(TSQhdr) != (int)_lread(hLoadFile, &h, sizeof(TSQhdr))) return(-1);
	// Now calculate the position of this scans control block...
	if (Scan > h.TotalScans) return(-1);
	Pos = (Scan) * sizeof(TSQcb) + h.ControlBlock;
	_llseek(hLoadFile,Pos,SEEK_SET);
	if(sizeof(TSQcb) != (int)_lread(hLoadFile, &cb, sizeof(TSQcb))) return(-1);
	Pos = h.StartOfData + cb.OffsetToData;
	_llseek(hLoadFile,Pos,SEEK_SET);
	for(i=0;i<cb.NumPoints;i++)
	{
		_lread(hLoadFile, &tr, sizeof(TSQrecord));
		Y[i] = tr.Amp;
		X[i] = tr.M;
	}
	return(cb.NumPoints);
}

// TSQ data file routines
// Return -1 if scan not found or error, else returns a pointer to the start of data.
int pascal _export TSQlocate(int hfile, int ScanNum, float *Start, float *Stop, int *NumPoints)
{
	static int     current=0,oldhfile=0, OldPos;
	int            i,j,k,pos=0,BytesRead,realine;
	unsigned char  Buffer[1024];

	// now loop through the data looking for 12 bytes of FF
	i = 0;
	if ((oldhfile != hfile) || (current > ScanNum))
	{
		current=0;
		OldPos=0;
		_llseek(hfile, 0, SEEK_SET);
	}
	else _llseek(hfile, OldPos, SEEK_SET);
	pos=OldPos;
	oldhfile=hfile;
	for(;;)
	{
		BytesRead = (int)_lread(hfile, &Buffer[i], sizeof(char)*(long)(1024-i));
		if(BytesRead<=0) break;
		for(i=0;i<1024;i++)
		{
			if(Buffer[i]==0xFF)
			{
				for(j=i;j<(i+12);j++)
				{
					if(Buffer[j]!=0xFF) break;
					if(j>=1024) break;
				}
				if((i+12)==j)
				{
					if(current!=ScanNum)
					{
						current++;
						i+=11;
					}
					else
					{
						// here if the scan was found!
						// Now read the start, stop, and length
						pos += i;
						_llseek(hfile, pos-16, SEEK_SET);
						_lread(hfile, Start, sizeof(float));
						_lread(hfile, Stop, sizeof(float));
						_llseek(hfile, pos-4, SEEK_SET);
						_lread(hfile, NumPoints, sizeof(int));
						OldPos = pos+12;
						pos -= (*NumPoints + 16);
						*NumPoints /= 4;
                  current++;
						return(pos);
					}
				}
			}
		}
 		// Now look for FFs at the end of buffer and aline them with the top
		if(BytesRead==0) { i=0; continue;}
		pos += BytesRead;
		for(i=0;i<1024;i++)
		{
			if(Buffer[1023-i]==0xFF) Buffer[i]=0xFF;
			else break;
		}
	}
	return(-1);
}

int pascal _export TSQload(int *lpdata, int Num, int hLoadFile,int pos)
{
	int                 BytesRead;          // number of bytes read from the file
	int                 element;            // element in the huge array
	int                 Size;
	int                 iVals[1024];
	int                 i;
	float               *data;

	data = (float *)*lpdata;
	if(pos != -1L) _llseek(hLoadFile, pos, SEEK_SET);
	else _llseek(hLoadFile,0,SEEK_END);
	for(element = 0L; element < Num ; element+=1024)
	{
		// calculate pointer to element
		Size = Num - element;
		if(Size > 1024) Size = 1024;
		if(Size < 0) Size = Num;
		// Read the ints from file
		BytesRead = (int)_lread(hLoadFile, (BYTE *)iVals, sizeof(long)*(long)Size);
		if(BytesRead < 0)
		{
			return HA_FILEREADERROR;
		}
		for(i = 0; i<BytesRead/sizeof(long);i++)
		{
			data[(element+i)*2] = data[(element+i)*2] + (iVals[i] & 0x07FFFFFFF);
			data[(element+i)*2 + 1] = 0;
		}
		for(;i<Size;i++) data[element+i] = 0.0;
	}
	return(element);
}

#define   Pie   3.1415926535897932384626433832795

int pascal _export SwiftPhase(int *lpdata, int Num)
{
	double t0,dt,c,b,fval;
	double energy,phase;
	int    li;
	float  *data;

	data = (float *)*lpdata;
	t0 = Num / 4;
	dt = Num / 2;
	c = 0.0;
	b = 0.0;
	energy = 0.0;
	for(li = 0; li <Num/2; li++)
	{
		energy = energy + data[li*2] * data[li*2];
	}
	for(li=0;li<Num/2;li++)
	{
		c = c + data[li*2] * data[li*2];
		b = b + c;
		if(energy == 0.0)
		{
			phase = 0;
		}
		else
		{
			phase = 2.0 * Pie * (dt * b / energy + t0 * li) / Num;
		}
		fval = data[li*2];
		data[li*2] = fval * cos(phase);
		data[li*2+1] = fval * sin(phase);
	}
	return(0);
}

int pascal _export Copy2Double(int *lpdata, double *dVals, int Num)
{
	int    li;
	float  *data;

	data = (float *)*lpdata;
	for(li = 0; li <Num; li++)
	{
		dVals[li]=data[li];
	}
	return(0);
}

float pascal _export Normalize(int *lpdata, int Num, float Max)
{
	int    li;
	float  *data,max,min,adj;

	data = (float *)*lpdata;
	min = max = data[0];
	for(li = 0; li <Num; li++)
	{
		if(data[li] > max) max = data[li];
		if(data[li] < min) min = data[li];
	}
	adj = Max/(max-min);
	for(li = 0; li <Num; li++)
	{
		data[li] = data[li] * adj;
	}
	return(fabs(max-min));
}

