# CoRC 0.2.1.9002

* Rebase on COPASI release 4.23 sources.
* Prepare for R version 3.5.
* Add unit system.
* Allow `NaN`, `Inf` and `-Inf` for many values.
* Add `getVersion()` for retrieving the version of the COPASI backend.

# CoRC 0.2.1 (2018-03-01)

* Breaking: Rename first argument of `newReaction()` from `scheme` to `reaction`.
* Rebase on latest COPASI 4.23 sources.
    * Fixes rare crashes.
* Minor fixes

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
