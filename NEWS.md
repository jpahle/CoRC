# CoRC 0.1.0.9000

* Fix installation on UNIX systems

# CoRC 0.1.0

* Rebase on latest COPASI development sources.
* Add model event functioniality (`event()`, `event_strict()`, `getEvents()`, `setEvents()`, `newEvent()`, `deleteEvent()`).
* The package now generally uses `""` instead of `NA` to denote unset or empty expressions.
* Allow expressions to be automatically coerced from finite numeric and logical values. E.g. `getValue(1)` instead of `getValue("1")`.
* Fix crashes on entity deletion. Deletion functions now delete recursively.
* Generally warn if model fails to compile.
* Fix errors with argument `type` of `setCompartments()` and `setGlobalQuantities()`.
* Use new Copasi XML parser.

# CoRC 0.0.1

* Initial public beta release
