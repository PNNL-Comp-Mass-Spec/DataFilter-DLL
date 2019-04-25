using System;
using NUnit.Framework;

namespace DataFilterTest
{
    public class Tests
    {

        [Test]
        [TestCase(100, 3, 3, 0, 5, 1, true, 314)]
        public void TestFilters(
            int dataPointCount,
            int numPointsLeft,
            int numPointsRight,
            short polynomialDegree,
            int amplitude,
            int noiseLevel,
            bool randomize,
            int randomSeed)
        {

            var dblData = new double[dataPointCount];

            var objFilter = new DataFilter.DataFilter();

            if (amplitude < 1) amplitude = 1;
            if (noiseLevel < 1) noiseLevel = 1;

            var rand = new Random(randomSeed);

            Console.WriteLine("Source Data");
            for (var i = 0; i < dataPointCount; i++)
            {
                dblData[i] = amplitude * Math.Sin(i / dataPointCount * 4);
                if (randomize)
                {
                    dblData[i] += rand.NextDouble() / (amplitude / 10.0) * noiseLevel;
                }

                if (i > 0.4 * dataPointCount && i < 0.7 * dataPointCount)
                {
                    dblData[i] *= Math.Abs(i - 0.55 * dataPointCount) * 2;
                }

                Console.WriteLine(dblData[i]);
            }

            var dblDataCopy = new double[dataPointCount];
            dblData.CopyTo(dblDataCopy, 0);

            var success = objFilter.SavitzkyGolayFilter(
                dblData, 0, dataPointCount - 1,
                numPointsLeft, numPointsRight,
                polynomialDegree, out var errorMessage);

            if (!success)
            {
                Assert.Fail(errorMessage);
            }

            objFilter.ButterworthFilter(dblDataCopy, 0, dataPointCount - 1);

            Console.WriteLine();
            Console.WriteLine("SavGolayFilter\tButterworthFilter");
            for (var i = 0; i < dataPointCount; i++)
            {
                Console.WriteLine(dblData[i] + "\t" + dblDataCopy[i]);
            }

        }
    }
}