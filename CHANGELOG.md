# Changelog for scalashogi

## v14.0.0

- Removed `Divider`, `Division` and `Variant.divisionSensibleVariants`.
- `History` no longer stores `lastUsi` and `plies`; both are now derived from `usis`, and `startedAtStep` is derived from the new `stepShifted` flag. Removed `withLastUsi`, `Position.withPlies`.
- `History` now has structural equality, including position hashes.
- `Hand` constructor is now private; `Hand(...)` and `copy` drop non-positive counts, and counts are clamped to zero when read and in CSA export.
- `Position.check`, `checkSquares`, `possibleDropDests`, `status`, `hasMoveDestinations` and `hasDropDestinations` are no longer `lazy val`s.
- `Replay` now fails with a new `Replay.Result.InvalidSfen` when the initial SFEN cannot be parsed, keeps the initial SFEN in the replayed history, and `Replay.positions` returns a `List` (empty on invalid SFEN). Renamed `failures` to `failure` in `Replay.Result.Incomplete`. Added `Position.fromInitialSfen`.
- Fixed chushogi last lion capture inconsistencies in SFEN parsing, position hashes and attack checks.
- Added left 5-piece, left 7-piece and right 7-piece handicaps, matching shogiops.
- KIF parser fixes: move numbers and step numbers (including nested variations in unnumbered KIF), variation termination, missing termination results, header lines after an invalid one, values mistaken for comments, and full-width `０` in numbers.
- KIF export now writes `手数＝` for positions with a later starting step, termination steps for special chushogi results, and the correct winner for perpetual check.
- CSA now accepts unknown endings without discarding the moves, treats `MAX_MOVES` as a draw, and uses the parsed side to move when exporting.
- KIF and CSA parsers return an error instead of throwing on stack overflow, and handle line breaks and whitespace more consistently.
