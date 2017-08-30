### DESCRIPTION

Code to estimate the channel capacity between a set of (signal, response)
ordered pairs

![Build status](https://travis-ci.org/ryants/EstCC.svg?branch=master)

### DEPENDENCIES

Built using scala v2.11, sbt v0.13.5, and java v1.7.0_51. In order to build
an executable JAR file, run `sbt one-jar` in the project's root directory. 
Additional information on this plugin can be found at https://github.com/sbt/sbt-onejar

### USAGE

Upon successful compilation, the one-jar executable is located in 
target/scala-2.1x/ and can be copied/renamed to any other directory for use:

i.e. `cp target/scala-2.1x/estcc_2.1x-0.1-SNAPSHOT-one-jar.jar /my/working/dir/MyEstCC.jar`

Given some data set, the channel capacity can be estimated using the following
command:

`java -jar MyEstCC.jar -d datafile -p paramfile`

where the optional 'paramfile' contains calculation parameters modified from 
their default values and 'datafile' contains the whitespace-delimited columns
of data

A full description of the options can be seen in the usage text:

`java -jar MyEstCC.jar --help`

### CONFIGURATION

All channel capacity calculation parameters are present in the 
`src/main/scala/infcalcs/InfConfig.scala` file with default values
and the sample parameter file.  The file `params.txt` contains a possible
parameter configuration for input to the executable JAR file. The parameter 
file is formatted with two tab-delimited columns, so that the parameter string
is in the first column and the parameter value in one of 5 possible 
(parameter-dependent) formats is in the second column.  Any other text is discarded.

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
and are used to define signal or response values that are known quantities.
If the either signal or response distribution is multi-dimensional, enter the
dimension-specific parameter values in the order that they are specified in the
column List parameters:

```
signalVals1    0,10,2
signalVals2    3 4 7
...
signalValsN    1,4
```
 
**Note that for assigning bin numbers, the presence of values take precedence over bin spacing** 
This means that if any `signalValsN` is specified, the calculation will use `signalValues` 
instead of the tuning the number of signal bins defined with `sigBinSpacing`. The number of 
parameters must match the number of dimensions specified by the `signalColumns`
and `responseColumns` parameters, otherwise the program will either fail to 
execute or produce incorrect results.  For example, if there are 4 signal
columns in your data file, you must either specify `signalVals[1-4]` or `signalBins[1-4]`

**Note also that certain bin number configurations can result in a calculation failure**
To avoid calculation artifacts resulting from the subsampling procedure, the program
implements a hard constraint on the upper limit of bins.  The number of bins in a particular
dimension must be less than the number of unique values in that dimension times the 
smallest fraction in the subsampling procedure.  A BinConfigurationException will be 
thrown if this criterion is not met at the outset of the calculation.  This can be 
avoided by specifying the set of values explicitly using the signalVals* or 
responseVals* parameters or by setting the sigBinSpacing or respBinSpacing parameters 
to be lower than the default setting of 4 (except in extreme cases).

### OUTPUT

The output is recorded in a series of files containing the estimated mutual
information for a particular signal distribution. Each file is identified by 
a weight index and a signal bin index, respectively (e.g. `out_47_19.dat`).
Contained in each space-delimited file is the number of signal bins, the number of 
response bins, the estimated mutual information, its 95% confidence interval 
and the estimated mutual information and confidence interval for a series of 
randomizations of the data set.

An information file (`out_info.dat`) will also be generated, listing the 
estimations for a range of values from the parameters producing the estimated
channel capacity.  These include:

- Marginal entropy of the signal ("signalEntropy")
- Marginal entropy of the response ("responseEntropy")
- Conditional entropy of the signal given the response ("condSignalEntropy")
- Conditional entropy of the response given the signal ("condResponseEntropy")
- Mutual information ("mutualInformation")
- The ratio of mutual information to marginal entropy of the signal ("transferEfficiency")
