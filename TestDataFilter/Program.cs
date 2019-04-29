using System;
using System.IO;

namespace TestDataFilter
{
    class Program
    {
        static void Main(string[] args)
        {
            TestFilter(1000, 3, 3, 0, 5, 1);
        }

        private static void TestFilter(int dataPointCount = 100,
            int numPointsLeft = 3,
            int numPointsRight = 3,
            short polynomialDegree = 0,
            int amplitude = 5,
            int noiseLevel = 1)
        {

            var dataSG = new double[dataPointCount];

            var filter = new DataFilter.DataFilter();

            if (amplitude < 1) amplitude = 1;
            if (noiseLevel < 1) noiseLevel = 1;

            var rand = new Random(314);

            for (var i = 0; i < dataPointCount; i++)
            {
                dataSG[i] = amplitude * (Math.Sin(i / dataPointCount * 4) + rand.NextDouble() / (amplitude / 10.0) * noiseLevel);
                if (i > 0.4 * dataPointCount && i < 0.7 * dataPointCount)
                {
                    dataSG[i] = dataSG[i] * Math.Abs(i - 0.55 * dataPointCount) * 2;
                }
            }

            var dataUnsmoothed = new double[dataPointCount];
            var dataButterworth = new double[dataPointCount];

            dataSG.CopyTo(dataUnsmoothed, 0);
            dataSG.CopyTo(dataButterworth, 0);

            filter.SavitzkyGolayFilter(dataSG, 0, dataPointCount - 1, numPointsLeft, numPointsRight, polynomialDegree, out _);
            filter.ButterworthFilter(dataButterworth, 0, dataPointCount - 1);

            var iterator = 1;
            var filePath = string.Empty;
            StreamWriter writer = null;
            var success = false;

            while (!success && iterator < 100)
            {
                try
                {
                    filePath = @"..\DataSmoothTest" + iterator + ".txt";
                    writer = new StreamWriter(new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.Read));
                    success = true;
                }
                catch
                {
                    iterator += 1;
                }
            }

            Console.WriteLine("");
            var headerLine = string.Format("{0,-15}{1,-25}{2,-25}", "Unsmoothed", "SavitzkyGolayFilter", "ButterworthFilter");
            var headerLineTabs = string.Format("{0}\t{1}\t{2}", "Unsmoothed", "SavitzkyGolayFilter", "ButterworthFilter");

            Console.WriteLine(headerLine);
            writer?.WriteLine(headerLineTabs);

            for (var i = 0; i < dataPointCount; i++)
            {
                var outLine = string.Format("{0,-15:F3}{1,-25:F3}{2,-25:F3}", dataUnsmoothed[i], dataSG[i], dataButterworth[i]);
                var outLineTabs = string.Format("{0:F3}\t{1:F3}\t{2:F3}", dataUnsmoothed[i], dataSG[i], dataButterworth[i]);

                Console.WriteLine(outLine);
                writer?.WriteLine(outLineTabs);
            }

            if (writer != null)
            {
                writer.Close();
                Console.WriteLine();
                Console.WriteLine("Note: data has been written to file " + filePath);
            }
            else
            {
                Console.WriteLine("Data was not written to a file");
            }

            System.Threading.Thread.Sleep(2000);
        }

    }
}
