### DESCRIPTION

Code to estimate the channel capacity between a set of (signal, response)
ordered pairs

![Build status](https://travis-ci.org/ryants/EstCC.svg?branch=master)

### DEPENDENCIES

Built using scala v2.11, sbt v0.13.5, and java v1.7.0_51. In order to build
an executable JAR file, run `sbt one-jar` in the project's root directory. 
Additional information on this plugin can be found at https://github.com/sbt/sbt-onejar

This code also contains a number of methods from the [Colt Project](http://acs.lbl.gov/software/colt/).  This 
dependency is included in the build.sbt file.

### USAGE

Upon successful compilation, the one-jar executable is located in 
target/scala-2.1x/ and can be copied/renamed to any other directory for use:

i.e. `cp target/scala-2.1x/estcc_2.1x-0.1-SNAPSHOT-one-jar.jar /my/working/dir/MyEstCC.jar`

Given a plaintext file with data organized into whitespace-delimited columns,
the channel capacity can be estimated using the following command:

`java -jar MyEstCC.jar -d datafile -p paramfile`

where the 'paramfile' contains calculation parameters modified from 
their default values and 'datafile' contains the whitespace-delimited columns
of data

A full description of the options can be seen in the usage text:

`java -jar MyEstCC.jar --help`

### CONFIGURATION

Default channel capacity calculation parameters are present in both the 
`src/main/scala/infcalcs/InfConfig.scala` file and the example `params.txt` 
file, which contains all possible parameters and their associated default 
values formatted for input to the executable JAR file. As mentioned 
previously, these values can be changed upon introduction of a parameter 
file.  The parameter file is formatted with two tab-delimited columns,
so that the parameter string is in the first column and the parameter value
in one of 5 possible (parameter-dependent) formats is in the second column:

##### List parameters:
- 2 or 3 comma-delimited numbers: 'minimum','maximum','increment' where 
   'maximum' is included and 'increment' is optional, defaulting to 1 
   (i.e. 0,10,2 produces the list: List(0.0, 2.0, 4.0, 6.0, 8.0, 10.0), 
   and 4,8 produces: List(4.0, 5.0, 6.0, 7.0, 8.0))
- a sequence of space-delimited numbers (i.e. 0 2 4 6 8 10 produces 
  List(0.0, 2.0, 4.0, 6.0, 8.0, 10.0)).

##### Numeric parameters:
- a single number

##### String parameters:  
- a string with no whitespace characters

##### Signal/Response (SR) parameters:
Similar to List parameters are SR parameters which employ the same syntax
and are used to define signal or response values that are known quantities, or
to define a range of bins with which to discretize signal or response space.
If the either signal or response distribution is multi-dimensional, enter the
dimension-specific parameter values in the order that they are specified in the
column List parameters:

```
signalVals1    0,10,2
signalVals2    3 4 7
...
signalValsN    1,4

responseBins1    2 3 4
responseBins2    2,10,2
...
responseBinsM   4
```
 
**Note that for assigning bin numbers, "Values" take precedence over "Bins."** This
means that if both `signalBins` and `signalValues` are specified, the calculation
will use `signalValues` instead of the various numbers of bins defined with
`signalBins`. Either bins or values must be specified for both signal and response and
the number of parameters must match the number of dimensions specified by the `signalColumns`
and `responseColumns` parameters, otherwise the program will either fail to 
execute or produce incorrect results.  For example, if there are 4 signal
columns in your data file, you must either specify `signalVals[1-4]` or `signalBins[1-4]`

### OUTPUT

The output is recorded in a series of files containing the estimated mutual
information for a particular signal distribution. Each file is identified by 
an index and the number of signal bins (e.g. `out_47_s19.dat`) with the 
exception of the uniform signal distribution (`out_unif_s19.dat`). Contained
in each space-delimited file is the number of signal bins, the number of 
response bins, the estimated mutual information, its 95% confidence interval 
and the estimated mutual information and confidence interval for a series of 
randomizations of the data set.

An information file (`out_info.dat`) will also be generated, listing the estimations for a range
of values from the parameters producing the estimated channel capacity.  These
include:

- Marginal entropy of the signal ("signalEntropy")
- Marginal entropy of the response ("responseEntropy")
- Conditional entropy of the signal given the response ("condSignalEntropy")
- Conditional entropy of the response given the signal ("condResponseEntropy")
- Mutual information ("mutualInformation")
- The ratio of mutual information to marginal entropy of the signal ("transferEfficiency")
