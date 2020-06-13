## Compiler Utilities: Logging

This library collects together various modules for tracking the performance
of a compiler.

### Modules

The `PhaseTimer` module supports a hierarchy of timers for tracking the
time spent in compiler phases.

The `Log` module implements a log file for compiler diagnostic messages.
It includes helper functions for reporting phase timings and for checking
IR invariants.

The `Stats` module implements a hierarchy of integer counters for tracking
information about a program (*e.g.*, the number of times a particular
optimization is performed).

### Dependencies

The `Log` module depends on the `Controls`, and the `PhaseTimer` and `Stats`
modules depend on the `JSON` modules from the
**SML/NJ Library**.  Add

````
$/controls-lib.cm
$/json-lib.cm
````

to your CM file, or

````
$(SML_LIB)/smlnj-lib/Controls/controls-lib.mlb
$(SML_LIB)/smlnj-lib/JSON/json-lib.mlb
````

to your MLB file, to include the necessary prerequisites.
