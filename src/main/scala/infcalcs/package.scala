/** Information theory calculations.
  *
  * This package implements calculation of information-theoretic quantities, in
  * particular estimates of mutual information and channel capacity for
  * continuous variables.
  *
  * For information on building, testing, and using the code, see the README
  * file.
  *
  * Configuration parameters can be specified in a 
  *
  * The steps in the channel capacity calculation are outlined below. For more
  * details on the theory underlying the approach taken, see the supplementary
  * information for Suderman, Bachman et al. (2014).
  *
  *  - In the top-level main function, [[EstCC]], command-line arguments are
  *    parsed and the data and configuration parameters are loaded.
  *
  *  - Because the channel capacity depends on the input distribution, various
  *    input weights are generated to determine which input weightings yield the
  *    highest mutual information between input and output. Input weights are
  *    generated using the functions [[EstimateCC.uniWeight]] and
  *    [[EstimateCC.biWeight]], which allow weighting of the input distribution
  *    according to unimodal and bimodal Gaussian distributions, respectively.
  *
  *  - Mutual information is calculated for each proposed input weighting
  *    by the function [[EstimateCC.calcWithWeightsMult]].
  *
  *  - For each weighting, the algorithm tries a wide variety of bin
  *    numbers/sizes to arrive at an estimate that is not biased by the bin size.
  *    The ranges of input and output bins are specified by configuration
  *    parameters.
  *
  *  - For each unique combination of input/output bin sizes, the algorithm
  *    builds the contingency tables for the raw data as well as for randomly
  *    selected subsamples of the data. These contingency tables are generated
  *    by [[EstimateMI.buildDataMult]].
  *
  *  - After calculating the mutual information for each contingency table
  *    (implemented in [[ContTable.mutualInformation]]) the unbiased mutual
  *    information is estimated by performing a linear regression of the mutual
  *    information of each subsampled dataset against the inverse sample size;
  *    the intercept of the linear regression is the unbiased mutual
  *    information estimate. The regression calculation is performed by
  *    [[EstimateMI.calcMultRegs]].
  *
  *  - Because increasing the number of bins can artifactually inflate estimates
  *    of MI, identical MI calculations are performed on shuffled datasets of
  *    varying sizes. The function [[EstimateMI.optMIMult]] then selects the MI
  *    estimate that maximizes the MI estimate for real data while keeping the
  *    the MI estimate for randomized data below the cutoff specified in the
  *    configuration.
  *
  *  - [[EstimateCC.getResultsMult]] then reports the channel capacity estimate
  *    as the maximum mutual information estimate obtained for all of the input
  *    weightings tested.
  */
package object infcalcs {

  /** A two-tuple. */
  type Pair[T] = (T, T)

  /** Input-output (dose-response) data. The first list contains the inputs,
    * the second list contains the outputs.
    */
  type DRData = Pair[List[Double]]

  /** Weight vector. The first entry is a list of weights across all of the
    * input bins. The second entry is a string that describes the type of
    * weighting.
    */
  type Weight = (List[Double], String)

  /** Data to be used for linear regression for a single randomization.
    *
    * Entries in the tuple:
    *  - inverse sample sizes
    *  - list of subsampled contingency tables
    *  - list of randomized contingency tables (a single randomization for each
    *    subsample size)
    *  - a list of string labels for output and logging purposes, one for each
    *    subsample size.
    */
  type RegData =
    (List[Double], List[ConstructedTable], List[ConstructedTable], List[String])

  /** Data to be used for linear regressions over all randomizations.
    *
    * Tuple containing entries as for [[RegData]], but the third entry contains
    * a list over all of the randomizations.
    */
  type RegDataMult =
    (List[Double], List[ConstructedTable], List[List[ConstructedTable]],
      List[String])

  type Prt = List[List[Double]]

  /** Tuple containing configuration parameters.
    *
    * The first entry contains parameters that have list values; the second
    * entry, parameters that have numerical values; the third entry, parameters
    * that have string values.
    */
  type Parameters =
    (Map[String, Option[List[Double]]], Map[String, Int], Map[String, String])
}
