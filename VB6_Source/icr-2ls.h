//
// icr-2ls.h
//
//		Header file for 32 bit memory allocation dll
//
// Gordon Anderson
// Sep 2, 1995
//
int  pascal _export HugeDim(int *,int, int);
int  pascal _export GetHugeEl(int *, int,int, BYTE *);
int  pascal _export SetHugeEl(int *, int,int, BYTE *);
int  pascal _export HugeErase(int);
int  pascal _export HugeLoadInt(int *, int Num, int hLoadFile, int pos,int ByteOrder);
int  pascal _export HugeLoadLong(int *, int Num, int hLoadFile,int pos,int ByteOrder);
int  pascal _export HugeLoad24(int *, int Num, int hLoadFile,int pos,int ByteOrder);
int  pascal _export HugeLoadFloat(int *, int Num, int hLoadFile, int pos);
int  pascal _export HugeLoadSunFloat(int *, int Num, int hLoadFile, int pos, int Ftype);
int  pascal _export HugeSaveInt(int *, int Num, int hLoadFile, int pos);
int  pascal _export HugeSaveFloat(int *, int Num, int hLoadFile, int pos);
int  pascal _export HugeExtract(int *, float *fVals, float *max, float *min, int Start, int Stop, int maxN, int option);
int  pascal _export SearchMW(int *C, int *H,int *N, int *O,int *S,double Mass,double MonoMass);
#define MAXBLOCKS    8
#define BLOCKSIZE    (0x800000L)

#define HA_OK               			0
#define HA_WM32_Invalid_Func       	(-1)
#define HA_WM32_Invalid_Flags      	(-2)
#define HA_WM32_Invalid_Arg        	(-3)
#define HA_WM32_Insufficient_Sels  	(-4)
#define HA_WM32_Insufficient_Mem   	(-5)
#define HA_OUTOFMEMORY      			(-6)
#define HA_TOMANYARRAYS     			(-7)
#define HA_BADELEMENTSIZE   			(-8)
#define HA_SUBSCRIPT        			(-9)
#define HA_BADARRAY         			(-10)
#define HA_FILEOPENERROR    			(-11)
#define HA_FILEWRITEERROR   			(-12)
#define HA_FILEREADERROR    			(-13)
