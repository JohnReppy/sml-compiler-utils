## Compiler Utilities: Logging

The `PhaseTimer` module supports a hierarchy of timers for tracking the
time spent in compiler phases.

The `Log` module implements a log file for compiler diagnostic messages.
It includes helper functions for reporting phase timings and for checking
IR invariants.

Note that the `Log` module depends on the `Controls` module from the
**SML/NJ Library**.  Add `$/controls-lib.cm` to your CM file (or
`$(SML_LIB)/smlnj-lib/Controls/controls-lib.mlb` to your MLB file)
to include the necessary prerequisites.
