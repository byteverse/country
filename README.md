# country

[![Build Status](https://travis-ci.org/andrewthad/country.svg?branch=master&status=started)](https://travis-ci.org/github/andrewthad/country)

This is a mega-repo with two projects:

- `country`: A library for dealing with countries, country codes, etc.
- `country-code-generation`: An executable that generates some of the
  source code in `country`. It converts both `countries.csv` and
  `aliases.txt` into lists of tuples.

#### Build Instructions

Some of source code in the `country` library is generated. To generate
this code, the `country-code-generation` application reads the top-level
`aliases.txt` and `countries.csv` file and outputs source Haskell. If
neither of these two files have been modified, then it is not necessary
to run `country-code-generation`. A `cabal.project` file is used to
build two targets. From the project root, run:

    cabal build country-code-generation
    /home/jdoe/path/to/projects/country/.../country-code-generation
    cabal build country
