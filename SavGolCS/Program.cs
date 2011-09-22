using System;
using System.Collections.Generic;
using System.Text;

namespace SavGolCS {
	class Program {

		static void Main(string[] args) {

			double[] c = new double[30];
			int np = 0;
			int nl = 0;
			int nr = 0;
			int ld = 0;
			int m = 0;

			NRSavGol sg = new NRSavGol();

			sg.savgol(c, np, nl, nr, ld, m);

		}
	}
}
