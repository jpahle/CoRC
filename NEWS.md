# CoRC 0.6.1.9000

* Fix system compatility with various linux distros.

  CoRC used to crash on various untested linux distros.
  Compatibility has now been confirmed with Ubuntu (16.04, 18.04, 20.04), CentOS (6, 7, 8), and Fedora (30, 32).

* Use 'readr' package for write out of experimental data, to prevent erroneous conversion of ´NaN´ to ´NA´.
* Minor fixes and code cleanups.

# CoRC 0.6.1 (2019-10-24)

* Remove an undocumented dependency on a Visual Studio runtime on windows.

# CoRC 0.6.0 (2019-10-06)

* Rebase on COPASI release 4.27 sources.
* Require at least R version 3.2.0.
* Base the linux binaries on the manylinux2010 docker container.
* Add parameter `soft_error` to `runTimeCourse()` to allow for readout of incomplete result tables for failed time courses.

# CoRC 0.5.0 (2019-04-05)

* Rebase on COPASI release 4.25 sources.
* Add a `preserve_concentrations` argument to `setCompartments()` to preserve species concentrations instead of particle numbers on adjustment of compartment sizes.
* Fix a bug where clearing initial expressions in setter functions prevented them from adjusting initial values.
* Enable for `getCopasi()` to fall back on github as a download mirror.
* Minor fixes and code cleanups.

# CoRC 0.4.0 (2018-08-01)

* Rebase on COPASI release 4.24 sources.
* Fix sigma point method for cases other than two dependent variables.
* Use `parallel::parLapplyLB()` for parallel processing as it was fixed in R version 3.5.
* Minor fixes and code cleanups.

# CoRC 0.3.0 (2018-04-19)

* Rebase on COPASI release 4.23 sources.
* Prepare for R version 3.5.
* Add unit system.
* Allow `NaN`, `Inf` and `-Inf` for many values.
* Add `getVersion()` for retrieving the version of the COPASI backend.

# CoRC 0.2.1 (2018-03-01)

* Breaking: Rename first argument of `newReaction()` from `scheme` to `reaction`.
* Rebase on latest COPASI 4.23 sources.
    * Fixes rare crashes.
* Minor fixes.

# CoRC 0.2.0 (2018-02-19)

* Rebase on latest COPASI development sources.
* Add functions to handle SBML models as strings (`loadSBMLFromString()`, `saveSBMLToString()`).
* Allow for currently running tasks to be terminated interactively (issue #3).
* Fix loadSBML function failing for local files.
* Fix installation on UNIX systems.
* Minor fixes and improvements.

# CoRC 0.1.0 (2017-12-04)

* Rebase on latest COPASI development sources.
* Add model event functioniality (`event()`, `event_strict()`, `getEvents()`, `setEvents()`, `newEvent()`, `deleteEvent()`).
* The package now generally uses `""` instead of `NA` to denote unset or empty expressions.
* Allow expressions to be automatically coerced from finite numeric and logical values. E.g. `getValue(1)` instead of `getValue("1")`.
* Fix crashes on entity deletion. Deletion functions now delete recursively.
* Generally warn if model fails to compile.
* Fix errors with argument `type` of `setCompartments()` and `setGlobalQuantities()`.
* Use new Copasi XML parser.

# CoRC 0.0.1 (2017-11-15)

* Initial public beta release
