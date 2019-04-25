using System;

namespace DataFilter
{
    public class ButterworthFilter
    {

        /// <summary>
        /// Butterworth Filter
        /// </summary>
        /// <param name="zeroBased1DArray"></param>
        /// <param name="indexStart"></param>
        /// <param name="indexEnd"></param>
        /// <returns></returns>
        /// <remarks>
        /// From the XPRESS software, written by Jimmy Eng
        /// http://cvs.sourceforge.net/viewcvs.py/sashimi/XPRESS-RAMP/addxpress3.c?rev=1.13&view=markup
        /// </remarks>
        public bool FilterData(float[] zeroBased1DArray, int indexStart, int indexEnd)
        {
            bool processingDataSubset;

            double[] tmpFilter;

            // Define 5th order Butterworth filter w/ cut-off frequency
            // of 0.15 where 1.0 corresponds to half the sample rate.

            const int FILTER_SIZE = 6;
            var a = new[] {1.0, -3.4789, 5.0098, -3.6995, 1.3942, -0.2138};
            var b = new[] {0.0004, 0.0018, 0.0037, 0.0037, 0.0018, 0.0004};

            if (zeroBased1DArray == null || zeroBased1DArray.Length <= 0)
                return false;

            // Copy data from zeroBased1DArray to tmpFilter, only copying a certain range if indexStart and indexEnd are >= 0
            if (indexStart < 0 ||
                indexEnd < 0 ||
                indexEnd < indexStart)
            {
                processingDataSubset = false;
                tmpFilter = new double[zeroBased1DArray.Length - 1];
                zeroBased1DArray.CopyTo(tmpFilter, 0);
            }
            else
            {
                processingDataSubset = true;
                tmpFilter = new double[indexEnd - indexStart];
                for (var i = indexStart; i <= indexEnd; i++)
                {
                    tmpFilter[i - indexStart] = zeroBased1DArray[i];
                }
            }

            var dataCount = tmpFilter.Length;

            // Pass MS profile through IIR low pass filter:
            // y(n) = b(1)*x(n) + b(2)*x(n-1) + ... + b(nb+1)*x(n-nb) -
            //        a(2)*y(n-1) - ... - a(na+1)*y(n-na)

            var filteredData = new double[dataCount - 1];
            for (var i = 0; i < dataCount; i++)
            {
                filteredData[i] = b[0] * tmpFilter[i];

                for (var j = 1; j < FILTER_SIZE; j++)
                {
                    if (i - j >= 0)
                    {
                        filteredData[i] += b[j] * tmpFilter[i - j];
                        filteredData[i] -= a[j] * filteredData[i - j];
                    }
                }
            }

            // Filtered data is reversed and re-filtered resulting
            // in zero-phase distortion and double the filter order.

            Array.Reverse(filteredData);
            filteredData.CopyTo(tmpFilter, 0);

            filteredData = new double[dataCount - 1];
            for (var i = 0; i < dataCount; i++)
            {
                filteredData[i] = b[0] * tmpFilter[i];
                for (var j = 1; j < FILTER_SIZE; j++)
                {
                    if (i - j >= 0)
                    {
                        filteredData[i] += b[j] * tmpFilter[i - j];
                        filteredData[i] -= a[j] * filteredData[i - j];
                    }
                }
            }

            // Filtered data is reversed again
            Array.Reverse(filteredData);

            // Update zeroBased1DArray with the filtered data
            if (processingDataSubset)
            {
                for (var i = 0; i < dataCount; i++)
                {
                    zeroBased1DArray[i + indexStart] = (float)filteredData[i];
                }
            }
            else
            {
                for (var i = 0; i < dataCount; i++)
                {
                    zeroBased1DArray[i] = (float)filteredData[i];
                }
            }

            return true;

        }

        // /*
        //  * C-based filter
        //  */
        // void FILTER_MS(double *dOrigMS,
        //         double *filteredData)
        // {
        //    int  i,
        //         iArraySize=MAX_MS_SCAN*sizeof(double);
        //    double tmpFilter[MAX_MS_SCAN];
        //
        //    /*
        //     * Defines 5th order butterworth filter w/ cut-off frequency
        //     * of 0.15 where 1.0 corresponse to half the sample rate.
        //     */
        //    double a[FILTER_SIZE]={1.0000, -3.4789, 5.0098, -3.6995, 1.3942, -0.2138},
        //           b[FILTER_SIZE]={0.0004, 0.0018, 0.0037, 0.0037, 0.0018, 0.0004};
        //
        //    memset(filteredData, 0, iArraySize);
        //    memcpy(tmpFilter, dOrigMS, sizeof(tmpFilter));
        //
        //    /*
        //     * Pass MS profile through IIR low pass filter:
        //     * y(n) = b(1)*x(n) + b(2)*x(n-1) + ... + b(nb+1)*x(n-nb)
        //     *      - a(2)*y(n-1) - ... - a(na+1)*y(n-na)
        //     */
        //    for (i=0; i<MAX_MS_SCAN; i++)
        //    {
        //       int ii;
        //
        //       filteredData[i]=b[0]*tmpFilter[i];
        //       for (ii=1;ii<FILTER_SIZE;ii++)
        //       {
        //          if ((i-ii)>=0)
        //          {
        //             filteredData[i] += b[ii]*tmpFilter[i-ii];
        //             filteredData[i] -= a[ii]*filteredData[i-ii];
        //          }
        //       }
        //    }
        //
        //    /*
        //     * Filtered sequence is reversed and re-filtered resulting
        //     * in zero-phase distortion and double the filter order.
        //     */
        //    for (i=0; i<MAX_MS_SCAN; i++)
        //       tmpFilter[i]=filteredData[MAX_MS_SCAN-1-i];
        //
        //    memset(filteredData, 0, iArraySize);
        //    for (i=0; i<MAX_MS_SCAN; i++)
        //    {
        //       int ii;
        //
        //       filteredData[i]=b[0]*tmpFilter[i];
        //       for (ii=1;ii<FILTER_SIZE;ii++)
        //       {
        //          if ((i-ii)>=0)
        //          {
        //             filteredData[i] += b[ii]*tmpFilter[i-ii];
        //             filteredData[i] -= a[ii]*filteredData[i-ii];
        //          }
        //       }
        //    }
        //
        //    /*
        //     * Filtered sequence is reversed again
        //     */
        //    for (i=0; i<MAX_MS_SCAN; i++)
        //       tmpFilter[i]=filteredData[MAX_MS_SCAN-1-i];
        //
        //    memcpy(filteredData, tmpFilter, iArraySize);
        //
        // } /*FILTER_MS*/
        //

    }
}