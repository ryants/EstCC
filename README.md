### DESCRIPTION

Code to estimate the channel capacity between a set of (signal, response)
ordered pairs

### DEPENDENCIES

Built using scala v2.11.0, sbt v0.13.2, and java v1.7.0_51. In order to build
an executable JAR file, run `sbt one-jar` in the project's root directory
to generate the JAR file.  Additional information on this plugin can be found 
at https://github.com/sbt/sbt-onejar

This code also contains a number of methods from the [Colt Project](http://acs.lbl.gov/software/colt/).  This 
dependency is included in the build.sbt file.

### USAGE

Upon successful compilation, the one-jar executable is located in 
target/scala-2.1x/ and can be copied/renamed to any other directory for use:

i.e. `cp target/scala-2.1x/estcc_2.1x-0.1-SNAPSHOT-one-jar.jar /my/working/dir/MyEstCC.jar`

Given a plaintext file with data organized into whitespace-delimited columns,
the channel capacity can be estimated using the following command:

`java -jar MyEstCC.jar datafile (paramfile)`

where the optional 'paramfile' contains calculation parameters modified from 
their default values and 'datafile' contains the whitespace-delimited columns
of data

### CONFIGURATION

Default channel capacity calculation parameters are present in both the 
`src/main/scala/infcalcs/InfConfig.scala` file and the example `params.txt` 
file.  As mentioned previously, these values can be changed upon introduction 
of a parameter file.  The parameter file should be formatted in two tab-
delimited columns, with the parameter string as shown in the 'InfConfig.scala' 
file in the first column. The second column will contain the parameter value 
in one of 5 possible formats depending on the nature of the parameter:

##### List parameters:
1. 2 or 3 comma-delimited numbers: 'minimum','maximum','increment' where 
   'maximum' is included and 'increment' is optional, defaulting to 1 
   (i.e. 0,10,2 produces the list: List(0.0, 2.0, 4.0, 6.0, 8.0, 10.0), 
   and 4,8 produces: List(4.0, 5.0, 6.0, 7.0, 8.0))
2. a sequence of space-delimited numbers (i.e. 0 2 4 6 8 10 produces the 
   same list as in option 1.
3. "None" to indicate the absence of a list (only applicable for 
   response/signal value or bin parameters

##### Integer parameters:
4. a single number

##### String parameters:  
5. a string with no whitespace characters

Parameters can only accommodate one of these option (unless the parameter requires
a list, in which case it can accommodate options 1. or 2.) and the program will
throw an Exception if, for example, the string "hello" is passed to the parameter
governing the number of data randomizations to consider. The file `params.txt`
contains all possible parameters and their associated default values

### OUTPUT

The output is recorded in a series of files containing the estimated mutual
information for a particular signal distribution (uniform, unimodal, or
bimodal). Each file is identified by the signal distribution type (n, u, or
b) as well as the number of signal bins and an index tracking the particular
signal distribution (i.e. `out_n_0.dat` or `out_u_s19_5.dat`). Contained
in each space-delimited file is the number of signal bins, the number of response bins, the
estimated mutual information, its 95% confidence interval and the 
estimated mutual information and confidence interval for a series of randomizations
of the data set.
