# sml-score

[![CI](https://github.com/sjqtentacles/sml-score/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-score/actions/workflows/ci.yml)

A musical **score data model** in pure Standard ML, built on
[`sml-music`](https://github.com/sjqtentacles/sml-music) (pitches, scales,
chords, transposition) and [`sml-midi`](https://github.com/sjqtentacles/sml-midi)
(event model + Standard MIDI File writer). Build notes, chords and scales into
parts, do exact-rational duration arithmetic, analyse a score (beats / ticks /
seconds), and render it to a byte-identical `.mid` file.

No FFI, no threads, no clock, no randomness: the same inputs always produce the
same outputs — and the same `.mid` bytes — under **MLton** and **Poly/ML**.
Durations are exact rationals of quarter-note beats, so dotted notes and tuplets
(a triplet eighth is exactly `1/3` of a beat) map to integer MIDI ticks without
rounding error.

- **`Score.Rat`** — a tiny exact rational (reduced `int * int`): `add`, `sub`,
  `mul`, `divide`, `compare`, `toReal`, `toString`.
- **Note values** — `whole`, `half`, `quarter`, `eighth`, `sixteenth`,
  `thirtysecond`, plus `dotted`, `doubleDotted`, `tuplet`, `triplet`.
- **Model** — a `slot` is a `Rest` or a `Tone` (one or many simultaneous
  pitches); a `part` is a channel + program + ordered slots; a `score` adds
  tempo, time signature and PPQ over a list of parts.
- **Build from `sml-music`** — `chordOf`, `scaleMelody`, `transposePart`,
  `transposeScore`.
- **Analyse** — `partBeats`, `scoreBeats`, `scoreTicks`, `scoreSeconds`,
  `noteCount`.
- **Render to MIDI** — `absEvents`, `partTrack`, `toSmf`, `toBytes` (a complete
  format-1 SMF: a conductor track with tempo + time signature, then one track
  per part), with deterministic ordering (offs before ons, then by note).

## API

```sml
structure Score : sig
  structure Rat : sig
    type t
    val rat : int * int -> t            (* reduces; den <> 0 *)
    val fromInt : int -> t
    val num : t -> int  val den : t -> int
    val add : t * t -> t  val sub : t * t -> t
    val mul : t * t -> t  val divide : t * t -> t
    val compare : t * t -> order  val eq : t * t -> bool
    val toReal : t -> real  val toString : t -> string
  end
  type rational = Rat.t

  val whole : rational  val half : rational  val quarter : rational
  val eighth : rational  val sixteenth : rational  val thirtysecond : rational
  val dotted : rational -> rational  val doubleDotted : rational -> rational
  val tuplet : int -> int -> rational -> rational  (* n in the time of m *)
  val triplet : rational -> rational

  val beatsToTicks : int -> rational -> int        (* ppq -> beats -> ticks *)
  val ticksToBeats : int -> int -> rational

  datatype slot =
      Rest of rational
    | Tone of { pitches : Music.pitch list, dur : rational, vel : int }
  type part = { name : string, channel : int, program : int, slots : slot list }
  type timeSig = { num : int, den : int }
  type score = { tempo : int, timeSig : timeSig, ppq : int, parts : part list }

  val note : Music.pitch -> rational -> int -> slot
  val rest : rational -> slot
  val chordSlot : Music.pitch list -> rational -> int -> slot
  val chordOf : Music.pitch -> Music.chordType -> rational -> int -> slot
  val scaleMelody : Music.pitch -> Music.scaleType -> rational -> int -> slot list
  val transposePart : int -> part -> part
  val transposeScore : int -> score -> score

  val partBeats : part -> rational  val scoreBeats : score -> rational
  val partTicks : int -> part -> int  val scoreTicks : score -> int
  val scoreSeconds : score -> real  val noteCount : score -> int

  val absEvents : int -> part -> (int * Midi.event) list
  val partTrack : int -> part -> Midi.track
  val toSmf : score -> Midi.smf  val toBytes : score -> string

  val eventStatusData : Midi.event -> int * int list
  val durLabel : rational -> string  val renderText : part -> string
  val checksum : string -> LargeInt.int   (* Adler-32 *)
end
```

## Example

```sml
val c4 = valOf (Music.parseNote "C4")

(* exact durations -> ticks at 480 PPQ *)
val 480 = Score.beatsToTicks 480 Score.quarter
val 720 = Score.beatsToTicks 480 (Score.dotted Score.quarter)
val 160 = Score.beatsToTicks 480 (Score.triplet Score.eighth)

(* a C major triad as a half-note chord, and a scale as a melody *)
val triad = Score.chordOf c4 Music.MajorTriad Score.half 90
val scale = Score.scaleMelody c4 Music.Major Score.eighth 70

val melody = { name = "lead", channel = 0, program = 0,
               slots = [ Score.note c4 Score.quarter 80 ] }
val sc = { tempo = 120, timeSig = { num = 4, den = 4 }, ppq = 480,
           parts = [ melody ] }
val bytes = Score.toBytes sc          (* a complete .mid file as a string *)
```

Running [`examples/demo.sml`](examples/demo.sml) with `make example` prints:

```
Note values at PPQ = 480:
  whole = 4 beat -> 1920 ticks
  half = 2 beat -> 960 ticks
  quarter = 1 beat -> 480 ticks
  eighth = 1/2 beat -> 240 ticks
  dotted-quarter = 3/2 beat -> 720 ticks
  triplet-eighth = 1/3 beat -> 160 ticks

C major triad (sml-music Music.chord):
  pitches = C4 E4 G4  (MIDI 60,64,67)
  transpose up a major third -> E4 G#4 B4

Melody (C4 E4 G4, quarters) as MIDI delta events:
  ABC-lite: C4:q E4:q G4:q
  d=0 status=144 data=[60,80]
  d=480 status=128 data=[60,0]
  d=0 status=144 data=[64,80]
  d=480 status=128 data=[64,0]
  d=0 status=144 data=[67,80]
  d=480 status=128 data=[67,0]

Score analysis:
  parts        = 2
  total beats  = 4
  total ticks  = 1920
  duration     = 2.000 s @ 120 BPM
  note count   = 9

Wrote assets/demo.mid:
  format       = 1 (conductor + 2 parts)
  byte length  = 179
  adler32      = 2483105709
```

The committed [`assets/demo.mid`](assets/demo.mid) is a real Standard MIDI File
(179 bytes) that opens in any DAW or MIDI player.

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo (writes assets/demo.mid)
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-score
smlpkg sync
```

This pulls the `sml-music` and `sml-midi` dependencies. Reference
`lib/github.com/sjqtentacles/sml-score/score.mlb` from your own `.mlb`
(MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Layout

```
sml.pkg                                       smlpkg manifest (music + midi)
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
lib/github.com/sjqtentacles/
  sml-music/music.sig  music.sml              vendored dependency
  sml-midi/midi.sig    midi.sml               vendored dependency
  sml-score/
    score.sig    SCORE signature
    score.sml    Score implementation
    sources.mlb  ordered source list (deps first)
    score.mlb    public basis
examples/
  demo.sml       score -> MIDI walkthrough
  (assets/demo.mid is written by `make example`)
test/
  harness.sml    shared assertion harness
  test.sml       rational / tick / transpose / event / .mid vectors (51 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

51 deterministic checks: exact rational reduction and arithmetic; note-value to
tick conversion at PPQ 480 (quarter = 480, dotted-quarter = 720, three
triplet-eighths sum to one quarter = 480); transposition via `sml-music`
(C4 + a major third = E4); chord and scale construction (C major triad =
`[60,64,67]`); the score-to-MIDI delta event list for a three-note melody;
stable event ordering (note-offs before note-ons at a shared tick, then by
note); score analysis (beats / ticks / seconds / note count); and a complete
`.mid` byte length + Adler-32 checksum. Run `make all-tests` to verify identical
output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
