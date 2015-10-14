import scala.io.Source
import scala.math

// Cumulative Square Root Frequency Stratification in Scala
object Stratify {

  // Return minimum of dataset of arbitrary size
  def min(data: Array[Double]): Double = {
    data.reduce(math.min(_, _))
  }

  // Return maximum of dataset of arbitrary size
  def max(data: Array[Double]): Double = {
    data.reduce(math.max(_, _))
  }

  /**
    * Stratification method:
    * Inputs:
    * data: Array of numbers.
    * numStrata: Number of strata to create.
    * breaks: Number of breaks to use for stratification. >= 2 (for min/max)
    *
    * Outputs:
    * 2-Dimensional Array representing stratification result.
    * Columns:
    * |lower|upper|frequency|csqrtf|stratum|
    */
  def stratify(data:Array[Double],
    numStrata:Int,
    breaks:Int): Array[Array[Double]] = {

    assert(numStrata >= 1, {println("numStrata >= 1")})
    assert(breaks >= 2, {println("breaks >= 2")})

    val table = Array.ofDim[Double](breaks-1, 5)
    val minBreak = math.floor(min(data))
    val maxBreak = math.ceil(max(data))
    table(0)(0) = minBreak
    table(table.length-1)(1) = maxBreak
    val intervalIncr = (maxBreak-minBreak+1)/breaks

    var lower = minBreak
    for (bin <- 0.until(table.length)) {
      table(bin)(0) = lower
      table(bin)(1) = lower + intervalIncr
      lower = table(bin)(1)
    }

    for (num <- data) {
      table(math.floor((num-minBreak)/intervalIncr).toInt)(2) += 1
    }

    table(0)(3) = math.sqrt(table(0)(2))
    // calculate cumulative sqrt of frequency
    for (bin <- 1.until(table.length)) {
      table(bin)(3) = table(bin-1)(3) + math.sqrt(table(bin)(2))
    }

    val maxCumFreq = table(table.length-1)(3)
    val cutPoints = Array.ofDim[Double](numStrata-1)
    for (stratum <- 1.until(numStrata)) {
      cutPoints(stratum-1) = maxCumFreq/numStrata * stratum
    }

    // stratify using 'cutPoints'
    var lastDiff = maxCumFreq
    var stratum = 1
    var stoppedBin = 0
    for (bin <- 0.until(table.length) if stratum != numStrata) {
      val diff = math.abs(table(bin)(3) - cutPoints(stratum-1))
      if (diff <= lastDiff) {
        lastDiff = diff
      } else {
        lastDiff = maxCumFreq
        stratum += 1
      }
      table(bin)(4) = stratum
      stoppedBin = bin
    }

    for (bin <- stoppedBin.until(table.length)) {
      table(bin)(4) = numStrata
    }

    table
  }

  def main(args: Array[String]) {
    // TODO: transform into unit tests
    // test data consists of 10000 numbers from normal distribution with
    // mean 20 and standard deviation 3
    val data = Array.ofDim[Double](10000)
    var index = 0
    for (line <-
      Source.fromURL(getClass.getResource("/rnorm10000mean20sd3.txt")).getLines()) {
      for (num <- line.split(" ")) {
        data(index) = num.toDouble
        index += 1
      }
    }

    println("Testing on 10000 numbers from normal dist with mean 20 and sd 3")
    // 5-column row |lower|upper|frequency|csqrtf|stratum|
    val expectedTable = Array(
      Array(9.0, 10.0, 2.0, 1.4142, 1.0),
      Array(10.0, 11.0, 10.0, 4.5764, 1.0),
      Array(11.0, 12.0, 22.0, 9.2669, 1.0),
      Array(12.0, 13.0, 53.0, 16.5470, 1.0),
      Array(13.0, 14.0, 132.0, 28.0361, 1.0),
      Array(14.0, 15.0, 249.0, 43.8158, 1.0),
      Array(15.0, 16.0, 410.0, 64.0643, 1.0),
      Array(16.0, 17.0, 681.0, 90.1603, 1.0),
      Array(17.0, 18.0, 929.0, 120.6398, 2.0),
      Array(18.0, 19.0, 1167.0, 154.8011, 2.0),
      Array(19.0, 20.0, 1408.0, 192.3245, 2.0),
      Array(20.0, 21.0, 1250.0, 227.6798, 3.0),
      Array(21.0, 22.0, 1133.0, 261.3399, 3.0),
      Array(22.0, 23.0, 950.0, 292.1619, 3.0),
      Array(23.0, 24.0, 694.0, 318.5058, 4.0),
      Array(24.0, 25.0, 444.0, 339.5771, 4.0),
      Array(25.0, 26.0, 252.0, 355.4516, 4.0),
      Array(26.0, 27.0, 113.0, 366.0818, 4.0),
      Array(27.0, 28.0, 61.0, 373.8920, 4.0),
      Array(28.0, 29.0, 29.0, 379.2772, 4.0),
      Array(29.0, 30.0, 8.0, 382.1056, 4.0),
      Array(30.0, 31.0, 3.0, 383.8377, 4.0)
    )
    val actualTable = stratify(data, 4, 23)

    for (row <- 0.until(actualTable.length)) {
      for (column <- 0.until(actualTable(row).length)) {
        assert(math.abs(actualTable(row)(column) - expectedTable(row)(column)) <= 0.01,
          {println("actualTable(%d)(%d)=%.2f!=%.2f=expectedTable(%d)(%d)".
            format(row, column, actualTable(row)(column),
              expectedTable(row)(column),row, column))})
      }
    }
    println("All tests passed!")
  }
}

