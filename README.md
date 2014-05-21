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

`java -jar MyEstCC.jar datafile.dat column1 column2`

where 'datafile.dat' contains the whitespace-delimited columns and column1 
and column2 are integers denoting the signal and response data columns (where
 the first column in the file is 0)

### CONFIGURATION

Channel capacity calculation parameters are present in the 
`src/main/scala/infcalcs/InfConfig.scala` file 
