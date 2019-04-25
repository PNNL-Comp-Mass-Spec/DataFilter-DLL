using System;

/*
 * This class implements the savgol savitzky-golay code from Numerical Recipes in C
 * Ported to C# in September 2011 by Matthew Monroe at PNNL
 */

namespace DataFilter
{
    public class NRSavGol {

        private const double TINY = 1.0e-20;

        // Linear equation solution, LU decomposition
        private void ludcmp(double[][] a, int n, int []indx) {
            int i, imax = 0;
            int j, k;
            double dum, sum;
            double big, temp;
            // double d = 1.0;

            // double *vv,*vector();
            // void nrerror(),free_vector();

            // vv=vector(1,n);
            double[] vv = new double[n + 1];

            for (i = 1; i <= n; i++) {
                big = 0.0f;

                for (j = 1; j <= n; j++)
                    if ((temp = Math.Abs(a[i][j])) > big)
                        big = temp;

                if (big == 0.0)
                    throw new Exception("Singular matrix in routine LUDCMP");

                vv[i] = 1.0 / big;
            }

            for (j = 1; j <= n; j++) {
                for (i = 1; i < j; i++) {
                    sum = a[i][j];

                    for (k = 1; k < i; k++)
                        sum -= a[i][k] * a[k][j];

                    a[i][j] = sum;
                }

                big = 0.0;
                for (i = j; i <= n; i++) {
                    sum = a[i][j];
                    for (k = 1; k < j; k++)
                        sum -= a[i][k] * a[k][j];

                    a[i][j] = sum;

                    if ((dum = vv[i] * Math.Abs(sum)) >= big) {
                        big = dum;
                        imax = i;
                    }
                }
                if (j != imax) {
                    for (k = 1; k <= n; k++) {
                        dum = a[imax][k];
                        a[imax][k] = a[j][k];
                        a[j][k] = dum;
                    }
                    // d = -(d);
                    vv[imax] = vv[j];
                }
                indx[j] = imax;
                if (a[j][j] == 0.0)
                    a[j][j] = TINY;

                if (j != n) {
                    dum = 1.0 / (a[j][j]);
                    for (i = j + 1; i <= n; i++)
                        a[i][j] *= dum;
                }
            }

        }

        // Linear equation solution, back substitution
        private void lubksb(double[][] a, int n, int[] indx, double[] b) {
            int i, ii = 0, ip, j;
            double sum;

            for (i = 1; i <= n; i++) {
                ip = (int)indx[i];
                sum = b[ip];
                b[ip] = b[i];

                // if (ii)
                if (ii > 0)
                    for (j = ii; j <= i - 1; j++)
                        sum -= a[i][j] * b[j];

                else {
                    // if (sum)
                    if (sum > 0)
                        ii = i;
                }

                b[i] = sum;
            }

            for (i = n; i >= 1; i--) {
                sum = b[i];
                for (j = i + 1; j <= n; j++)
                    sum -= a[i][j] * b[j];

                b[i] = sum / a[i][i];
            }
        }


        public int savgol(double[] c, int np, int nl, int nr, int ld, int m) {

            int imj, ipj, j, k, kk, mm;		 // ,*indx;
            double fac;				 // ,**a,*b;
            double sum;

            if (np < nl + nr + 1 || nl < 0 || nr < 0 || ld > m || nl + nr < m)
                return (-1);

            // indx=ivector(1,m+1);
            // a=matrix(1,m+1,1,m+1);
            // b=vector(1,m+1);

            // Note that arrays indx, a, and b do not utilize indx[0], a[0][x], or b[0]
            // This code starts at index 1

            int[] indx = new int[m + 2];
            double[][] a = new double[m + 2][];
            double[] b = new double[m + 2];

            // Initialize 2D array a
            for (int i = 0; i < a.GetLength(0); i++) {
                a[i] = new double[m + 2];
            }

            for (ipj = 0; ipj <= (m << 1); ipj++) {
                // sum = (ipj ? 0.0 : 1.0);
                if (ipj > 0)
                    sum = 0;
                else
                    sum = 1;

                for (k = 1; k <= nr; k++)
                    sum += Math.Pow((double)k, (double)ipj);

                for (k = 1; k <= nl; k++)
                    sum += Math.Pow((double)-k, (double)ipj);

                mm = Math.Min(ipj, 2 * m - ipj);

                for (imj = -mm; imj <= mm; imj += 2)
                    a[1 + (ipj + imj) / 2][1 + (ipj - imj) / 2] = sum;
            }

            ludcmp(a, m + 1, indx);

            for (j = 1; j <= m + 1; j++)
                b[j] = 0.0;

            b[ld + 1] = 1.0;

            lubksb(a, m + 1, indx, b);

            for (kk = 1; kk <= np; kk++)
                c[kk] = 0.0;

            for (k = -nl; k <= nr; k++) {
                sum = b[1];
                fac = 1.0;
                for (mm = 1; mm <= m; mm++)
                    sum += b[mm + 1] * (fac *= k);

                kk = ((np - k) % np) + 1;
                c[kk] = sum;
            }

            return 0;
        }
    }
}
