using System;
using System.Collections.Generic;

namespace DataFilter
{
    /// <summary>
    /// This class includes a Butterworth filter (works well), a Moving Window Average filter,
    /// and a Savitzky Golay filter (doesn't work for polynomialDegree > 0)
    /// </summary>
    /// <remarks>
    /// Butterworth filters ported from the XPRESS software, written by Jimmy Eng
    /// http://cvs.sourceforge.net/viewcvs.py/sashimi/XPRESS-RAMP/addxpress3.c?rev=1.13&view=markup
    /// Coefficients for various sampling rates obtained using MatLab (courtesy of Deep Jaitly, PNNL)
    ///
    /// Savitzky Golay functions were originally written by Gordon Anderson for use in ICR-2LS (1995)
    /// The core of the Savitzky Golay filter comes from Numerical recipes in C
    /// Ported from VB 6 to VB.NET by Matthew Monroe in October 2003
    /// Also uses a .NET managed C++ wrapper written by Deep Jaitly to call the savgol function in the icr2ls32.dll file
    ///
    /// Matthew Monroe ported the C code to C# in September 2011, removing the need to use SAVGOL.dll or icr2ls32.dll
    /// </remarks>
    public class DataFilter
    {
        const int BUTTERWORTH_FILTER_ORDER = 5;

        static double[,] AC;
        static double[,] BC;
        static bool mArraysInitialized;

        // This function is a static function since GetButterworthCoefficientsFifthOrder is static
        private static void AddCoeffs(int coeffIndex, int filterOrder, IReadOnlyList<double> newA, IReadOnlyList<double> newB)
        {
            for (var i = 0; i <= filterOrder; i++)
            {
                AC[coeffIndex, i] = newA[i];
                BC[coeffIndex, i] = newB[i];
            }
        }

        /// <summary>
        /// Butterworth filter
        /// </summary>
        /// <param name="zeroBased1DArray"></param>
        /// <param name="indexStart"></param>
        /// <param name="indexEnd"></param>
        /// <param name="samplingFrequency">
        /// Defines the cut-off frequency where 1.0 corresponds to half the sample rate
        /// Can be between 0.01 and 0.99
        /// </param>
        /// <returns></returns>
        public bool ButterworthFilter(double[] zeroBased1DArray, int indexStart, int indexEnd, double samplingFrequency = 0.25)
        {

            bool processingDataSubset;

            double[] tmpFilter;

            //
            // Define 5th order Butterworth filter
            //

            GetButterworthCoefficientsFifthOrder(samplingFrequency, out var a, out var b);

            if (zeroBased1DArray == null || zeroBased1DArray.Length <= 0)
                return false;

            // Copy data from zeroBased1DArray to tmpFilter, only copying a certain range if indexStart and indexEnd are >= 0
            if (indexStart == 0 && indexEnd == zeroBased1DArray.Length - 1 ||
                indexStart < 0 ||
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

                for (var j = 1; j <= BUTTERWORTH_FILTER_ORDER; j++)
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
                for (var j = 1; j <= BUTTERWORTH_FILTER_ORDER; j++)
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
                    zeroBased1DArray[i + indexStart] = filteredData[i];
                }
            }
            else
            {
                for (var i = 0; i < dataCount; i++)
                {
                    zeroBased1DArray[i] = filteredData[i];
                }
            }

            return true;

        }

        /// <summary>
        /// Store the Butterworth coefficients for a 5th order filter
        /// </summary>
        /// <param name="samplingFrequency">
        ///defines the cut-off frequency where 1.0 corresponds to half the sample rate
        /// can be between 0.01 and 0.99
        /// </param>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns></returns>
        private static bool GetButterworthCoefficientsFifthOrder(double samplingFrequency, out double[] a, out double[] b)
        {

            const int FREQ_LEVEL_COUNT = 99;

            a = new double[BUTTERWORTH_FILTER_ORDER];
            b = new double[BUTTERWORTH_FILTER_ORDER];

            if (!mArraysInitialized)
            {
                AC = new double[FREQ_LEVEL_COUNT - 1, BUTTERWORTH_FILTER_ORDER];
                BC = new double[FREQ_LEVEL_COUNT - 1, BUTTERWORTH_FILTER_ORDER];

                // The following define the filter coefficients for sample rates of 0.01 tfo 0.99, in steps of 0.01

                AddCoeffs(0, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.8983, 9.5985, -9.4053, 4.6085, -0.90333 }, new[] { 0.00000000090929, 0.0000000045464, 0.0000000090929, 0.0000000090929, 0.0000000045464, 0.00000000090929 });
                AddCoeffs(1, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.7967, 9.2072, -8.8404, 4.2458, -0.81598 }, new[] { 0.000000027689, 0.00000013844, 0.00000027689, 0.00000027689, 0.00000013844, 0.000000027689 });
                AddCoeffs(2, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.695, 8.8261, -8.304, 3.9099, -0.73703 }, new[] { 0.00000020024, 0.0000010012, 0.0000020024, 0.0000020024, 0.0000010012, 0.00000020024 });
                AddCoeffs(3, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.5934, 8.4551, -7.7949, 3.5989, -0.66565 }, new[] { 0.00000080424, 0.0000040212, 0.0000080424, 0.0000080424, 0.0000040212, 0.00000080424 });
                AddCoeffs(4, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.4918, 8.0941, -7.3121, 3.311, -0.60112 }, new[] { 0.000002341, 0.000011705, 0.00002341, 0.00002341, 0.000011705, 0.000002341 });
                AddCoeffs(5, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.3903, 7.7429, -6.8543, 3.0447, -0.54275 }, new[] { 0.0000055603, 0.000027802, 0.000055603, 0.000055603, 0.000027802, 0.0000055603 });
                AddCoeffs(6, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.2888, 7.4015, -6.4207, 2.7983, -0.48996 }, new[] { 0.00001148, 0.000057401, 0.0001148, 0.0001148, 0.000057401, 0.00001148 });
                AddCoeffs(7, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.1873, 7.0697, -6.01, 2.5704, -0.44221 }, new[] { 0.000021396, 0.00010698, 0.00021396, 0.00021396, 0.00010698, 0.000021396 });
                AddCoeffs(8, BUTTERWORTH_FILTER_ORDER, new[] { 1, -4.0859, 6.7476, -5.6213, 2.3598, -0.39901 }, new[] { 0.000036884, 0.00018442, 0.00036884, 0.00036884, 0.00018442, 0.000036884 });
                AddCoeffs(9, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.9845, 6.4349, -5.2536, 2.1651, -0.35993 }, new[] { 0.000059796, 0.00029898, 0.00059796, 0.00059796, 0.00029898, 0.000059796 });
                AddCoeffs(10, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.8833, 6.1315, -4.9061, 1.9853, -0.32457 }, new[] { 0.000092253, 0.00046126, 0.00092253, 0.00092253, 0.00046126, 0.000092253 });
                AddCoeffs(11, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.7821, 5.8375, -4.5777, 1.8193, -0.29258 }, new[] { 0.00013664, 0.00068318, 0.0013664, 0.0013664, 0.00068318, 0.00013664 });
                AddCoeffs(12, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.6809, 5.5526, -4.2678, 1.666, -0.26365 }, new[] { 0.00019557, 0.00097786, 0.0019557, 0.0019557, 0.00097786, 0.00019557 });
                AddCoeffs(13, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.5799, 5.2767, -3.9753, 1.5246, -0.23747 }, new[] { 0.00027193, 0.0013596, 0.0027193, 0.0027193, 0.0013596, 0.00027193 });
                AddCoeffs(14, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.4789, 5.0098, -3.6995, 1.3942, -0.2138 }, new[] { 0.00036878, 0.0018439, 0.0036878, 0.0036878, 0.0018439, 0.00036878 });
                AddCoeffs(15, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.378, 4.7518, -3.4397, 1.274, -0.19239 }, new[] { 0.00048944, 0.0024472, 0.0048944, 0.0048944, 0.0024472, 0.00048944 });
                AddCoeffs(16, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.2772, 4.5025, -3.1951, 1.1633, -0.17303 }, new[] { 0.00063738, 0.0031869, 0.0063738, 0.0063738, 0.0031869, 0.00063738 });
                AddCoeffs(17, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.1765, 4.2618, -2.9649, 1.0613, -0.15553 }, new[] { 0.00081629, 0.0040814, 0.0081629, 0.0081629, 0.0040814, 0.00081629 });
                AddCoeffs(18, BUTTERWORTH_FILTER_ORDER, new[] { 1, -3.0759, 4.0297, -2.7485, 0.96744, -0.13972 }, new[] { 0.00103, 0.0051501, 0.0103, 0.0103, 0.0051501, 0.00103 });
                AddCoeffs(19, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.9754, 3.806, -2.5453, 0.88113, -0.12543 }, new[] { 0.0012826, 0.0064129, 0.012826, 0.012826, 0.0064129, 0.0012826 });
                AddCoeffs(20, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.875, 3.5907, -2.3544, 0.80179, -0.11253 }, new[] { 0.0015782, 0.0078908, 0.015782, 0.015782, 0.0078908, 0.0015782 });
                AddCoeffs(21, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.7747, 3.3836, -2.1755, 0.72892, -0.10087 }, new[] { 0.0019211, 0.0096054, 0.019211, 0.019211, 0.0096054, 0.0019211 });
                AddCoeffs(22, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.6745, 3.1847, -2.0078, 0.66202, -0.090358 }, new[] { 0.0023158, 0.011579, 0.023158, 0.023158, 0.011579, 0.0023158 });
                AddCoeffs(23, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.5744, 2.9939, -1.8507, 0.60067, -0.080871 }, new[] { 0.0027669, 0.013835, 0.027669, 0.027669, 0.013835, 0.0027669 });
                AddCoeffs(24, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.4744, 2.811, -1.7038, 0.54443, -0.072316 }, new[] { 0.0032792, 0.016396, 0.032792, 0.032792, 0.016396, 0.0032792 });
                AddCoeffs(25, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.3745, 2.636, -1.5664, 0.49294, -0.064605 }, new[] { 0.0038575, 0.019288, 0.038575, 0.038575, 0.019288, 0.0038575 });
                AddCoeffs(26, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.2747, 2.4689, -1.4381, 0.44583, -0.057658 }, new[] { 0.0045069, 0.022534, 0.045069, 0.045069, 0.022534, 0.0045069 });
                AddCoeffs(27, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.175, 2.3095, -1.3184, 0.40277, -0.051402 }, new[] { 0.0052324, 0.026162, 0.052324, 0.052324, 0.026162, 0.0052324 });
                AddCoeffs(28, BUTTERWORTH_FILTER_ORDER, new[] { 1, -2.0754, 2.1577, -1.2067, 0.36346, -0.045773 }, new[] { 0.0060394, 0.030197, 0.060394, 0.060394, 0.030197, 0.0060394 });
                AddCoeffs(29, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.9759, 2.0135, -1.1026, 0.32762, -0.040709 }, new[] { 0.0069332, 0.034666, 0.069332, 0.069332, 0.034666, 0.0069332 });
                AddCoeffs(30, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.8765, 1.8768, -1.0057, 0.29498, -0.036157 }, new[] { 0.0079194, 0.039597, 0.079194, 0.079194, 0.039597, 0.0079194 });
                AddCoeffs(31, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.7772, 1.7475, -0.91547, 0.2653, -0.032066 }, new[] { 0.0090036, 0.045018, 0.090036, 0.090036, 0.045018, 0.0090036 });
                AddCoeffs(32, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.6779, 1.6256, -0.83154, 0.23836, -0.028392 }, new[] { 0.010192, 0.050959, 0.10192, 0.10192, 0.050959, 0.010192 });
                AddCoeffs(33, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.5788, 1.511, -0.75347, 0.21395, -0.025092 }, new[] { 0.01149, 0.057449, 0.1149, 0.1149, 0.057449, 0.01149 });
                AddCoeffs(34, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.4797, 1.4037, -0.68086, 0.19188, -0.02213 }, new[] { 0.012904, 0.064518, 0.12904, 0.12904, 0.064518, 0.012904 });
                AddCoeffs(35, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.3807, 1.3035, -0.61332, 0.17199, -0.01947 }, new[] { 0.01444, 0.0722, 0.1444, 0.1444, 0.0722, 0.01444 });
                AddCoeffs(36, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.2817, 1.2105, -0.55047, 0.15411, -0.017082 }, new[] { 0.016105, 0.080524, 0.16105, 0.16105, 0.080524, 0.016105 });
                AddCoeffs(37, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.1829, 1.1246, -0.49193, 0.1381, -0.014935 }, new[] { 0.017905, 0.089526, 0.17905, 0.17905, 0.089526, 0.017905 });
                AddCoeffs(38, BUTTERWORTH_FILTER_ORDER, new[] { 1, -1.0841, 1.0457, -0.43735, 0.12382, -0.013004 }, new[] { 0.019848, 0.099239, 0.19848, 0.19848, 0.099239, 0.019848 });
                AddCoeffs(39, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.98533, 0.97385, -0.38636, 0.11116, -0.011264 }, new[] { 0.02194, 0.1097, 0.2194, 0.2194, 0.1097, 0.02194 });
                AddCoeffs(40, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.88664, 0.90893, -0.33861, 0.10002, -0.0096912 }, new[] { 0.024188, 0.12094, 0.24188, 0.24188, 0.12094, 0.024188 });
                AddCoeffs(41, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.788, 0.85095, -0.29377, 0.090295, -0.0082657 }, new[] { 0.0266, 0.133, 0.266, 0.266, 0.133, 0.0266 });
                AddCoeffs(42, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.6894, 0.79985, -0.25149, 0.081905, -0.0069673 }, new[] { 0.029184, 0.14592, 0.29184, 0.29184, 0.14592, 0.029184 });
                AddCoeffs(43, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.59084, 0.75563, -0.21145, 0.074777, -0.0057777 }, new[] { 0.031948, 0.15974, 0.31948, 0.31948, 0.15974, 0.031948 });
                AddCoeffs(44, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.49232, 0.71825, -0.17331, 0.068849, -0.0046793 }, new[] { 0.0349, 0.1745, 0.349, 0.349, 0.1745, 0.0349 });
                AddCoeffs(45, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.39382, 0.6877, -0.13676, 0.06407, -0.0036557 }, new[] { 0.038048, 0.19024, 0.38048, 0.38048, 0.19024, 0.038048 });
                AddCoeffs(46, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.29534, 0.66395, -0.10147, 0.060396, -0.0026909 }, new[] { 0.041401, 0.20701, 0.41401, 0.41401, 0.20701, 0.041401 });
                AddCoeffs(47, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.19689, 0.64699, -0.067122, 0.057795, -0.0017699 }, new[] { 0.044969, 0.22485, 0.44969, 0.44969, 0.22485, 0.044969 });
                AddCoeffs(48, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.098441, 0.63683, -0.033404, 0.056244, -0.00087777 }, new[] { 0.048761, 0.2438, 0.48761, 0.48761, 0.2438, 0.048761 });
                AddCoeffs(49, BUTTERWORTH_FILTER_ORDER, new[] { 1, -0.00000000000000046491, 0.63344, -0.00000000000000020438, 0.055728, -3.0935E-18 }, new[] { 0.052786, 0.26393, 0.52786, 0.52786, 0.26393, 0.052786 });
                AddCoeffs(50, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.098441, 0.63683, 0.033404, 0.056244, 0.00087777 }, new[] { 0.057056, 0.28528, 0.57056, 0.57056, 0.28528, 0.057056 });
                AddCoeffs(51, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.19689, 0.64699, 0.067122, 0.057795, 0.0017699 }, new[] { 0.06158, 0.3079, 0.6158, 0.6158, 0.3079, 0.06158 });
                AddCoeffs(52, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.29534, 0.66395, 0.10147, 0.060396, 0.0026909 }, new[] { 0.06637, 0.33185, 0.6637, 0.6637, 0.33185, 0.06637 });
                AddCoeffs(53, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.39382, 0.6877, 0.13676, 0.06407, 0.0036557 }, new[] { 0.071437, 0.35719, 0.71437, 0.71437, 0.35719, 0.071437 });
                AddCoeffs(54, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.49232, 0.71825, 0.17331, 0.068849, 0.0046793 }, new[] { 0.076794, 0.38397, 0.76794, 0.76794, 0.38397, 0.076794 });
                AddCoeffs(55, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.59084, 0.75563, 0.21145, 0.074777, 0.0057777 }, new[] { 0.082452, 0.41226, 0.82452, 0.82452, 0.41226, 0.082452 });
                AddCoeffs(56, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.6894, 0.79985, 0.25149, 0.081905, 0.0069673 }, new[] { 0.088426, 0.44213, 0.88426, 0.88426, 0.44213, 0.088426 });
                AddCoeffs(57, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.788, 0.85095, 0.29377, 0.090295, 0.0082657 }, new[] { 0.094727, 0.47364, 0.94727, 0.94727, 0.47364, 0.094727 });
                AddCoeffs(58, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.88664, 0.90893, 0.33861, 0.10002, 0.0096912 }, new[] { 0.10137, 0.50686, 1.0137, 1.0137, 0.50686, 0.10137 });
                AddCoeffs(59, BUTTERWORTH_FILTER_ORDER, new[] { 1, 0.98533, 0.97385, 0.38636, 0.11116, 0.011264 }, new[] { 0.10837, 0.54187, 1.0837, 1.0837, 0.54187, 0.10837 });
                AddCoeffs(60, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.0841, 1.0457, 0.43735, 0.12382, 0.013004 }, new[] { 0.11575, 0.57874, 1.1575, 1.1575, 0.57874, 0.11575 });
                AddCoeffs(61, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.1829, 1.1246, 0.49193, 0.1381, 0.014935 }, new[] { 0.12351, 0.61757, 1.2351, 1.2351, 0.61757, 0.12351 });
                AddCoeffs(62, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.2817, 1.2105, 0.55047, 0.15411, 0.017082 }, new[] { 0.13169, 0.65843, 1.3169, 1.3169, 0.65843, 0.13169 });
                AddCoeffs(63, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.3807, 1.3035, 0.61332, 0.17199, 0.01947 }, new[] { 0.14028, 0.7014, 1.4028, 1.4028, 0.7014, 0.14028 });
                AddCoeffs(64, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.4797, 1.4037, 0.68086, 0.19188, 0.02213 }, new[] { 0.14932, 0.7466, 1.4932, 1.4932, 0.7466, 0.14932 });
                AddCoeffs(65, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.5788, 1.511, 0.75347, 0.21395, 0.025092 }, new[] { 0.15882, 0.79411, 1.5882, 1.5882, 0.79411, 0.15882 });
                AddCoeffs(66, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.6779, 1.6256, 0.83154, 0.23836, 0.028392 }, new[] { 0.16881, 0.84403, 1.6881, 1.6881, 0.84403, 0.16881 });
                AddCoeffs(67, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.7772, 1.7475, 0.91547, 0.2653, 0.032066 }, new[] { 0.1793, 0.89649, 1.793, 1.793, 0.89649, 0.1793 });
                AddCoeffs(68, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.8765, 1.8768, 1.0057, 0.29498, 0.036157 }, new[] { 0.19032, 0.95158, 1.9032, 1.9032, 0.95158, 0.19032 });
                AddCoeffs(69, BUTTERWORTH_FILTER_ORDER, new[] { 1, 1.9759, 2.0135, 1.1026, 0.32762, 0.040709 }, new[] { 0.20189, 1.0094, 2.0189, 2.0189, 1.0094, 0.20189 });
                AddCoeffs(70, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.0754, 2.1577, 1.2067, 0.36346, 0.045773 }, new[] { 0.21403, 1.0702, 2.1403, 2.1403, 1.0702, 0.21403 });
                AddCoeffs(71, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.175, 2.3095, 1.3184, 0.40277, 0.051402 }, new[] { 0.22678, 1.1339, 2.2678, 2.2678, 1.1339, 0.22678 });
                AddCoeffs(72, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.2747, 2.4689, 1.4381, 0.44583, 0.057658 }, new[] { 0.24016, 1.2008, 2.4016, 2.4016, 1.2008, 0.24016 });
                AddCoeffs(73, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.3745, 2.636, 1.5664, 0.49294, 0.064605 }, new[] { 0.2542, 1.271, 2.542, 2.542, 1.271, 0.2542 });
                AddCoeffs(74, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.4744, 2.811, 1.7038, 0.54443, 0.072316 }, new[] { 0.26894, 1.3447, 2.6894, 2.6894, 1.3447, 0.26894 });
                AddCoeffs(75, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.5744, 2.9939, 1.8507, 0.60067, 0.080871 }, new[] { 0.28439, 1.422, 2.8439, 2.8439, 1.422, 0.28439 });
                AddCoeffs(76, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.6745, 3.1847, 2.0078, 0.66202, 0.090358 }, new[] { 0.30061, 1.503, 3.0061, 3.0061, 1.503, 0.30061 });
                AddCoeffs(77, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.7747, 3.3836, 2.1755, 0.72892, 0.10087 }, new[] { 0.31761, 1.5881, 3.1761, 3.1761, 1.5881, 0.31761 });
                AddCoeffs(78, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.875, 3.5907, 2.3544, 0.80179, 0.11253 }, new[] { 0.33545, 1.6773, 3.3545, 3.3545, 1.6773, 0.33545 });
                AddCoeffs(79, BUTTERWORTH_FILTER_ORDER, new[] { 1, 2.9754, 3.806, 2.5453, 0.88113, 0.12543 }, new[] { 0.35416, 1.7708, 3.5416, 3.5416, 1.7708, 0.35416 });
                AddCoeffs(80, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.0759, 4.0297, 2.7485, 0.96744, 0.13972 }, new[] { 0.37379, 1.869, 3.7379, 3.7379, 1.869, 0.37379 });
                AddCoeffs(81, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.1765, 4.2618, 2.9649, 1.0613, 0.15553 }, new[] { 0.39438, 1.9719, 3.9438, 3.9438, 1.9719, 0.39438 });
                AddCoeffs(82, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.2772, 4.5025, 3.1951, 1.1633, 0.17303 }, new[] { 0.41597, 2.0799, 4.1597, 4.1597, 2.0799, 0.41597 });
                AddCoeffs(83, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.378, 4.7518, 3.4397, 1.274, 0.19239 }, new[] { 0.43862, 2.1931, 4.3862, 4.3862, 2.1931, 0.43862 });
                AddCoeffs(84, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.4789, 5.0098, 3.6995, 1.3942, 0.2138 }, new[] { 0.46238, 2.3119, 4.6238, 4.6238, 2.3119, 0.46238 });
                AddCoeffs(85, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.5799, 5.2767, 3.9753, 1.5246, 0.23747 }, new[] { 0.48731, 2.4366, 4.8731, 4.8731, 2.4366, 0.48731 });
                AddCoeffs(86, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.6809, 5.5526, 4.2678, 1.666, 0.26365 }, new[] { 0.51347, 2.5673, 5.1347, 5.1347, 2.5673, 0.51347 });
                AddCoeffs(87, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.7821, 5.8375, 4.5777, 1.8193, 0.29258 }, new[] { 0.54091, 2.7046, 5.4091, 5.4091, 2.7046, 0.54091 });
                AddCoeffs(88, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.8833, 6.1315, 4.9061, 1.9853, 0.32457 }, new[] { 0.56971, 2.8486, 5.6971, 5.6971, 2.8486, 0.56971 });
                AddCoeffs(89, BUTTERWORTH_FILTER_ORDER, new[] { 1, 3.9845, 6.4349, 5.2536, 2.1651, 0.35993 }, new[] { 0.59994, 2.9997, 5.9994, 5.9994, 2.9997, 0.59994 });
                AddCoeffs(90, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.0859, 6.7476, 5.6213, 2.3598, 0.39901 }, new[] { 0.63167, 3.1584, 6.3167, 6.3167, 3.1584, 0.63167 });
                AddCoeffs(91, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.1873, 7.0697, 6.01, 2.5704, 0.44221 }, new[] { 0.66499, 3.3249, 6.6499, 6.6499, 3.3249, 0.66499 });
                AddCoeffs(92, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.2888, 7.4015, 6.4207, 2.7983, 0.48996 }, new[] { 0.69997, 3.4999, 6.9997, 6.9997, 3.4999, 0.69997 });
                AddCoeffs(93, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.3903, 7.7429, 6.8543, 3.0447, 0.54275 }, new[] { 0.73672, 3.6836, 7.3672, 7.3672, 3.6836, 0.73672 });
                AddCoeffs(94, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.4918, 8.0941, 7.3121, 3.311, 0.60112 }, new[] { 0.77532, 3.8766, 7.7532, 7.7532, 3.8766, 0.77532 });
                AddCoeffs(95, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.5934, 8.4551, 7.7949, 3.5989, 0.66565 }, new[] { 0.81588, 4.0794, 8.1588, 8.1588, 4.0794, 0.81588 });
                AddCoeffs(96, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.695, 8.8261, 8.304, 3.9099, 0.73703 }, new[] { 0.8585, 4.2925, 8.585, 8.585, 4.2925, 0.8585 });
                AddCoeffs(97, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.7967, 9.2072, 8.8404, 4.2458, 0.81598 }, new[] { 0.90331, 4.5166, 9.0331, 9.0331, 4.5166, 0.90331 });
                AddCoeffs(98, BUTTERWORTH_FILTER_ORDER, new[] { 1, 4.8983, 9.5985, 9.4053, 4.6085, 0.90333 }, new[] { 0.95044, 4.7522, 9.5044, 9.5044, 4.7522, 0.95044 });

                mArraysInitialized = true;
            }

            var coeffIndex = (int)(samplingFrequency * 100) - 1;
            if (coeffIndex < 0)
                coeffIndex = 4;

            if (coeffIndex > 98)
                coeffIndex = 94;

            try
            {
                for (var i = 0; i <= BUTTERWORTH_FILTER_ORDER; i++)
                {
                    a[i] = AC[coeffIndex, i];
                    b[i] = BC[coeffIndex, i];
                }

                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error in GetButterworthCoefficientsFifthOrder: " + ex.Message);
                return false;
            }

        }

        /// <summary>
        /// Moving window average filter
        /// </summary>
        /// <param name="zeroBased1DArray"></param>
        /// <param name="indexStart"></param>
        /// <param name="indexEnd"></param>
        /// <param name="windowWidthPoints"></param>
        /// <param name="errorMessage"></param>
        /// <returns></returns>
        public bool MovingWindowAverage(
            double[] zeroBased1DArray,
            int indexStart,
            int indexEnd,
            int windowWidthPoints,
            out string errorMessage)
        {
            int numPointsLeft;
            int numPointsRight;

            // Define the distance to examine left and right of each point
            if (windowWidthPoints < 3)
                windowWidthPoints = 3;

            if (windowWidthPoints % 2 == 0)
            {
                // Even number of points
                numPointsLeft = windowWidthPoints / 2;
                numPointsRight = numPointsLeft - 1;
            }
            else
            {
                // Odd Number of points
                numPointsLeft = (int)(Math.Floor(windowWidthPoints / 2.0));
                numPointsRight = numPointsLeft;
            }

            // const bool USE_SAVITZKY_GOLAY = false;
            // if (USE_SAVITZKY_GOLAY)
            // {
            //     success = SavitzkyGolayFilter(zeroBased1DArray, indexStart, indexEnd, numPointsLeft, numPointsRight, 0, True, errorMessage);
            // }

            // Perform the moving average locally

            try
            {
                var smoothedData = new double[zeroBased1DArray.Length - 1];
                zeroBased1DArray.CopyTo(smoothedData, 0);

                for (var currentIndex = indexStart; currentIndex <= indexEnd; currentIndex++)
                {
                    var start = currentIndex - numPointsLeft;
                    var end = currentIndex + numPointsRight;
                    if (start < indexStart)
                        start = indexStart;

                    if (end > indexEnd)
                        end = indexEnd;

                    double total = 0;
                    for (var i = start; i <= end; i++)
                    {
                        total += zeroBased1DArray[i];
                    }

                    smoothedData[currentIndex] = total / (end - start + 1);

                }

                // Copy the smoothed data back into zeroBased1DArray
                smoothedData.CopyTo(zeroBased1DArray, 0);

                errorMessage = string.Empty;
                return true;
            }
            catch (Exception ex)
            {
                errorMessage = "Error in MovingWindowAverage: " + ex.Message;
                return false;
            }
        }

        /// <summary>
        /// Savitzky Golay Filter
        /// </summary>
        /// <param name="zeroBased1DArray"></param>
        /// <param name="indexStart"></param>
        /// <param name="indexEnd"></param>
        /// <param name="numPointsLeft">Should normally be equivalent to numPointsRight, and is usually an odd number like 3, 5, 7, etc.</param>
        /// <param name="numPointsRight"></param>
        /// <param name="polynomialDegree">Normally 2 or 4; if polynomialDegree is odd, it is actually decremented by 1 to give an even number</param>
        /// <param name="errorMessage"></param>
        /// <param name="correctIntensityValues">When true, a correction factor of 1.6 is applied to the smoothed data points</param>
        /// <returns>True if success, False if error</returns>
        /// <remarks>
        /// A Polynomial Degree of 0 will result in behavior identical to a moving average filter
        ///
        /// When using a polynomial degree above 0, the smoothed intensity values are lower than the input values
        ///
        /// Example call:
        /// objFilter.SavitzkyGolayFilter(dblData, 0, dataCount-1, 3, 3, 4, out var errorMessage)
        /// </remarks>
        public bool SavitzkyGolayFilter(
            double[] zeroBased1DArray,
            int indexStart,
            int indexEnd,
            int numPointsLeft,
            int numPointsRight,
            short polynomialDegree,
            out string errorMessage,
            bool correctIntensityValues = true)
        {

            if (numPointsLeft < 1 || numPointsRight < 1)
            {
                errorMessage = "numPointsLeft and numPointsRight should be >= 1";
                return false;
            }

            if (polynomialDegree % 2 == 1)
            {
                polynomialDegree -= 1;
            }

            if (polynomialDegree < 0)
                polynomialDegree = 0;

            while (numPointsLeft + numPointsRight < polynomialDegree && polynomialDegree > 1)
            {
                // Decrement the polynomialDegree by 2
                polynomialDegree -= 2;
            }

            // if (numPointsLeft + numPointsRight < polynomialDegree)
            // {
            //     errorMessage = "numPointsLeft + numPointsRight should be >= polynomialDegree";
            //     return false;
            // }

            var numPointsTotal = numPointsLeft + numPointsRight + 1;
            var c = new double[numPointsTotal];

            var objSavGol = new NRSavGol();
            objSavGol.savgol(c, numPointsTotal, numPointsLeft, numPointsRight, 0, polynomialDegree);

            // now un wrap the coefficients ...
            var n = numPointsRight * 2;
            if (numPointsLeft > numPointsRight)
            {
                n = numPointsLeft * 2;
            }

            var CC = new double[n];
            for (var i = 0; i <= numPointsLeft; i++)
            {
                CC[(int)(Math.Floor(n / 2.0 - i))] = c[i + 1];
            }

            for (var i = 1; i <= numPointsRight; i++)
            {
                CC[(int)(Math.Floor(n / 2.0 + i))] = c[n - i];
            }

            SavitzkyGolayWork(zeroBased1DArray, indexStart, indexEnd, CC, polynomialDegree, correctIntensityValues);

            errorMessage = string.Empty;
            return true;
        }

        private void SavitzkyGolayWork(
            double[] zeroBased1DArray,
            int indexStart,
            int indexEnd,
            IReadOnlyList<double> c,
            short polynomialDegree,
            bool correctIntensityValues)
        {
            double correctionFactor;

            if (polynomialDegree <= 1 || !correctIntensityValues)
                correctionFactor = 1.0;
            else
                correctionFactor = 1.6;

            var width = (int)(Math.Floor((c.Count - 1) / 2.0));

            if (indexStart > indexEnd)
            {
                // Swap the indices
                var temp = indexEnd;
                indexEnd = indexStart;
                indexStart = temp;
            }

            // Reserve space for Y()
            var Y = new double[width * 2 + 1];

            // Reserve space for a temporary buffer to hold the results of the smooth
            var tempBuffer = new double[zeroBased1DArray.Length - 1];

            // Copy data from input array to temporary buffer
            zeroBased1DArray.CopyTo(tempBuffer, 0);

            for (var i = indexStart; i < indexEnd; i++)
            {
                var copyStart = i - width;
                var copyEnd = i + width + 1;

                if (copyStart < indexStart)
                    copyStart = indexStart;

                if (copyEnd >= indexEnd)
                    copyEnd = indexEnd - 1;

                for (var srcIndex = copyStart; srcIndex <= copyEnd; srcIndex++)
                {
                    Y[srcIndex - copyStart] = zeroBased1DArray[srcIndex];
                }

                var n = copyEnd - copyStart;


                if (n == c.Count)
                {
                    var total = 0.0;
                    for (var j = 0; j < n; j++)
                    {
                        total += Y[j] * c[j];
                    }

                    tempBuffer[i] = total * correctionFactor;
                }
            }

            // Copy data from temporary buffer back to input array
            tempBuffer.CopyTo(zeroBased1DArray, 0);

        }

    }
}