import infcalcs.tables.ContTable

/** Information theory calculations.
  *
  * This package implements calculation of information-theoretic quantities, in
  * particular estimates of mutual information and channel capacity for
  * continuous variables.
  *
  * For information on building, testing, and using the code, see the README
  * file.
  *
  * The steps in the channel capacity calculation are outlined below. For more
  * details on the theory underlying the approach taken, see the supplementary
  * information for Suderman, Bachman et al. (2014).
  *
  * - In the top-level main function, [[infcalcs.EstCC]], command-line arguments are
  * parsed and the data and configuration parameters are loaded.
  *
  * - Because the channel capacity depends on the input distribution, various
  * input weights are generated to determine which input weightings yield the
  * highest mutual information between input and output. Input weights are
  * generated using the functions [[infcalcs.EstimateCC.uniWeight]] and
  * [[infcalcs.EstimateCC.biWeight]], which allow weighting of the input distribution
  * according to unimodal and bimodal Gaussian distributions, respectively.
  *
  * - Mutual information is calculated for each proposed input weighting
  * by the function [[infcalcs.EstimateCC.calcWithWeightsMult]].
  *
  * - For each weighting, the algorithm tries a wide variety of bin
  * numbers/sizes to arrive at an estimate that is not biased by the bin size.
  * The ranges of input and output bins are specified by configuration
  * parameters.
  *
  * - For each unique combination of input/output bin sizes, the algorithm
  * builds the contingency tables for the raw data as well as for randomly
  * selected subsamples of the data. These contingency tables are generated
  * by [[infcalcs.EstimateMI.buildRegData]].
  *
  * - After calculating the mutual information for each contingency table
  * (implemented in [[ContTable.mutualInformation]]) the unbiased mutual
  * information is estimated by performing a linear regression of the mutual
  * information of each subsampled dataset against the inverse sample size;
  * the intercept of the linear regression is the unbiased mutual
  * information estimate. The regression calculation is performed by
  * [[infcalcs.EstimateMI.calcMultRegs]].
  *
  * - Because increasing the number of bins can artifactually inflate estimates
  * of MI, identical MI calculations are performed on shuffled datasets of
  * varying sizes. The function [[infcalcs.EstimateMI.optMIMult]] then selects the MI
  * estimate that maximizes the MI estimate for real data while keeping the
  * the MI estimate for randomized data below the cutoff specified in the
  * configuration.
  *
  * - [[infcalcs.EstimateCC.getResultsMult]] then reports the channel capacity estimate
  * as the maximum mutual information estimate obtained for all of the input
  * weightings tested.
  */
package object infcalcs {

  /** A two-tuple. */
  type Pair[T] = (T, T)

  /** An n-tuple (for clarity)  */
  type NTuple[T] = Vector[T]

}
