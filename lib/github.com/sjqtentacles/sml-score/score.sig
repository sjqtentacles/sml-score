(* score.sig

   A musical score data model in pure Standard ML, built on sml-music
   (pitches, scales, chords, transposition) and sml-midi (event model and
   Standard MIDI File writer).

   Durations are exact rationals measured in quarter-note beats: a quarter
   note is `quarter` = 1, a half note is 2, an eighth is 1/2, and so on.
   Dotted values and tuplets are exact (a triplet eighth is 1/3 of a beat).
   Given a ticks-per-quarter-note resolution (PPQ), a duration maps to an
   exact integer number of MIDI ticks.

   No FFI, threads, clock or randomness: identical inputs always produce
   identical outputs (and byte-identical .mid files) under MLton and
   Poly/ML. *)

signature SCORE =
sig
  (* ---- exact rational arithmetic (reduced, positive denominator) ---- *)
  structure Rat :
  sig
    type t
    exception DivZero
    val rat      : int * int -> t      (* construct & reduce; den <> 0 *)
    val fromInt  : int -> t
    val num      : t -> int            (* signed numerator of reduced form *)
    val den      : t -> int            (* positive denominator of reduced form *)
    val add      : t * t -> t
    val sub      : t * t -> t
    val mul      : t * t -> t
    val divide   : t * t -> t
    val compare  : t * t -> order
    val eq       : t * t -> bool
    val toReal   : t -> real
    val toString : t -> string         (* "3/2", or "2" when integral *)
  end

  type rational = Rat.t

  (* ---- note values, in quarter-note beats ---- *)
  val whole        : rational
  val half         : rational
  val quarter      : rational
  val eighth       : rational
  val sixteenth    : rational
  val thirtysecond : rational
  val dotted       : rational -> rational              (* d * 3/2 *)
  val doubleDotted : rational -> rational              (* d * 7/4 *)
  val tuplet       : int -> int -> rational -> rational (* n in the time of m *)
  val triplet      : rational -> rational              (* tuplet 3 2 *)

  (* ---- duration <-> ticks ---- *)
  val beatsToTicks : int -> rational -> int            (* ppq -> beats -> ticks *)
  val ticksToBeats : int -> int -> rational            (* ppq -> ticks -> beats *)

  (* ---- model ---- *)
  datatype slot =
      Rest of rational
    | Tone of { pitches : Music.pitch list, dur : rational, vel : int }

  type part = { name : string, channel : int, program : int, slots : slot list }
  type timeSig = { num : int, den : int }
  type score = { tempo : int, timeSig : timeSig, ppq : int, parts : part list }

  (* ---- slot constructors ---- *)
  val note      : Music.pitch -> rational -> int -> slot
  val rest      : rational -> slot
  val chordSlot : Music.pitch list -> rational -> int -> slot

  (* ---- construction from sml-music ---- *)
  val chordOf     : Music.pitch -> Music.chordType -> rational -> int -> slot
  val scaleMelody : Music.pitch -> Music.scaleType -> rational -> int -> slot list
  val transposePart  : int -> part -> part
  val transposeScore : int -> score -> score

  (* ---- analysis ---- *)
  val slotDur      : slot -> rational
  val partBeats    : part -> rational
  val scoreBeats   : score -> rational    (* length of the longest part *)
  val partTicks    : int -> part -> int
  val scoreTicks   : score -> int
  val scoreSeconds : score -> real
  val noteCount    : score -> int         (* total sounding notes (chord = many) *)

  (* ---- MIDI rendering (built on sml-midi) ---- *)
  (* absolute-tick note-on/off events for a part, stably ordered by
     (tick, off-before-on, note). *)
  val absEvents : int -> part -> (int * Midi.event) list
  (* delta-time track of note-on/off events only (no meta). *)
  val partTrack : int -> part -> Midi.track
  val toSmf     : score -> Midi.smf
  val toBytes   : score -> string

  (* ---- inspection / rendering ---- *)
  val eventStatusData : Midi.event -> int * int list  (* status byte, data bytes *)
  val durLabel   : rational -> string                 (* "q", "q.", "1/3", ... *)
  val renderText : part -> string                     (* ABC-lite single line *)
  val checksum   : string -> LargeInt.int             (* Adler-32 *)
end
