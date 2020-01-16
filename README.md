# Okanagan Hydrologic Model Environment

This repository contains codes, scripts and tools necessary for developing hydrologic models within the Okanagan Hydrologic Model Environment (OHME).  This environment is intended for application with the [Raven Hydrological Model Framework](http://raven.uwaterloo.ca/) for Okanagan-specific hydrologic applications.  It also includes access to Raven code itself, tools to develop model input, Raven model calibration capabilities using [Ostrich](http://www.eng.buffalo.edu/~lsmatott/Ostrich/OstrichMain.html), and output visualization methods.  Finally, it also includes resources to assist with OHME application on [Google Cloud Platform](https://cloud.google.com/) resources.

### Prerequisites

To use OHME effectively you would benefit from familiarity with the following tools, which must be enabled on the platform where you are running OHME:

- [Git](https://git-scm.com/) version control software

- [R](https://www.r-project.org/) scientific computing software

- [netCDF](https://www.unidata.ucar.edu/software/netcdf/) data management software

- [C++](https://en.wikipedia.org/wiki/C%2B%2B) compiling software and appropriate operating system compiling tools (e.g. [make](https://www.gnu.org/software/make/) and [gcc](https://gcc.gnu.org/)) if building Raven or Ostrich from source code.

For more substantial applications of OHME (such as the default application supplied here, which is designed to carry out full hydrologic calibrations over 19 individual Okanagan watersheds) you will also likely benefit from access to larger computing facilities (e.g. local cluster or cloud-based systems) and knowledge of the Linux operating system.  OHME is currently designed to operate on Google Cloud Platform.  However, for small applications or testing, OHME could be configured to operate on smaller systems, including personal computers.

### Installing

- Use Git to clone OHME from this site to your local environment.  Please contact OHME Administrators prior to cloning to ensure you are accessing the latest stable OHME release tag.

- Build the Raven executable locally from Raven source code, or access an appropriate pre-built Raven executable from the Raven website.

- Build the Ostrich executable locally from the supplied source code, or access an appropriate pre-built Ostrich executable from the Ostrich website.

- Install R and appropriate R packages (found in supplied R code headers).

- Download OHME input and calibration data from [OHME data library](https://console.cloud.google.com/storage/browser/ohme-data-library).  You will need a basic, free Google account to access this location.

- Correctly configure paths to model source directory, and input and output directories.

- Please contact OHME Administrators as listed below for specific guidance on OHME configuration.

### Testing

Raven developers have included a suite of [self-contained Raven tests](available at http://raven.uwaterloo.ca/Downloads.html).  We recommend performing these tests to ensure Raven itself is working, before executing the full OHME infrastructure.  For Okanagan-specific applications, please contact OHME Administrators as listed below for specific guidance on testing procedures.

## Versioning

Tested, operational OHME versions will be released periodically as official tags on the Release branch of this Git account.  Use of any other branches for operational hydrological  modelling is done at your own risk.

## Contributing

The Okanagan Basin Water Board welcomes contributions to OHME.  To ensure that contributions are integrated into the framework, we require all contributors to use Git to develop code/scripts/etc., and contribute this to the official project vit git pull requests to OHME Administrators.  We also encourage all interested users to contact OHME Administrators prior to initiation of extensive 3rd party development.

## Administrators

The primary administrators of OHME are:

-[Lawrence Bird](birdl@ae.ca)

-[Jeremy Fyke](fykej@ae.ca)

## License

Distribution and modification of open-source OHME code is released under the [Artistic License 2.0](https://opensource.org/licenses/artistic-license-2.0).  

