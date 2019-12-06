# Raven Okanagan Hydrology Model

This repository contains the framework (code and scripts) for configuring, building, and running the Raven hydrological model (http://raven.uwaterloo.ca/) for Okanagan-specific configurations.  This framework includes both Raven code itself, and additional code and scripts to develop model input, peform model calibrations (using Ostrich, http://www.eng.buffalo.edu/~lsmatott/Ostrich/OstrichMain.html).

### Prerequisites

To use this framework you will need familiarity with the following tools:
-> GIT (version control software)
-> R (scientific computing language)
-> C++ compiling software and the make utility (for Linux users)
-> netCDF data format

For more substantial applications (such as the default application supplied here) you would also benefit from access to larger computing facilities (e.g. local cluster or cloud-based systems) and knowledge of the Linux operating system.

### Installing

1) Use GIT to clone to your local environment.

2) Build the Raven executable locally from the supplied source code, or access an appropriate pre-built Raven executable from the Raven website.

3) Install R and appropriate R packages.

4) Download appropriate Ostrich executable from Ostrich website.

5) Configure paths to model source directory, and input and output directories.

### Testing

Raven as supplied by default here includes a suite of self-contained tests.  We recommend performing these tests to ensure Raven itself is working.  For Okanagan-specific applications, please contact Authors as listed below for specific guidance on possible testing procedures.

## Contributing and Versioning

The Okanagan Basin Water Board welcomes contributions to this framework.  To ensure that contributions are integrated into the framework, we require all contributors to use GIT to develop code/scripts/etc., and contribute this to the official project vit git pull requests.  We also encourage all interested users to contact primary Authors who will be happy to help with code and framework setup and sharing.

Tested, operational framework versions will be released periodically as official tags on the master branch of this GIT account.  Use of any other branches for operational hydrological  modelling is done at your own risk.

## Authors



## License

This model and source code is released under the GPU V2 license.  

## Acknowledgements


