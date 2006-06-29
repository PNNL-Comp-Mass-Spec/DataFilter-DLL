#using <mscorlib.dll>
using namespace System;

_declspec (dllimport) __stdcall int savgol(float c[], int np, int nl,int nr, int ld, int m) ; 

namespace SAVGOL
{
	public __gc class SAVGOL
	{
	public:
		int savgol_wrapper(float c __gc[], int np, int nl,int nr, int ld, int m)
		{
			int size = c->Count ; 
			float *c_data = new float [size] ; 
			for (int i = 0 ; i < size ; i++)
				c_data[i] = c[i] ; 
			int status = savgol(c_data, np, nl, nr, ld, m) ; 
			for (int i = 0 ; i < size ; i++)
				c[i] = c_data[i] ; 

			return status ; 
		}
	} ; 
}