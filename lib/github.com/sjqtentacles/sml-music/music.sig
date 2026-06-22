(* music.sig

   Music theory in pure Standard ML: pitches in scientific pitch notation,
   MIDI conversion, equal-tempered and just frequencies, intervals, scales,
   chords, and key signatures.

   A `pitch` is a spelled note: a letter (C..B), a signed accidental
   (positive = sharps, negative = flats, so +2 is a double-sharp), and an
   octave in scientific pitch notation where middle C is `C4` = MIDI 60 and
   A4 = MIDI 69. Spelling is preserved through parsing/printing (`Bb5` stays
   `Bb5`, not `A#5`).

   No FFI, threads, clock or randomness: the same inputs always produce the
   same outputs under MLton and Poly/ML. Real-valued results (frequencies,
   cents) are equal-temperament with A4 = 440 Hz (12-TET); compare them with
   an epsilon, never `=`. *)

signature MUSIC =
sig
  type pitch

  (* ---- construction / parsing / naming ---- *)
  (* parse scientific pitch notation: "C4", "F#3", "Bb5", "C-1", "Dbb0". *)
  val parseNote      : string -> pitch option
  val noteName       : pitch -> string      (* e.g. "F#3" (round-trips parseNote) *)
  val pitchClassName : pitch -> string      (* name without octave, e.g. "F#" *)
  val toMidi         : pitch -> int          (* C4 = 60, A4 = 69 *)
  val fromMidi       : int -> pitch          (* canonical sharp spelling *)
  val pitchClass     : pitch -> int          (* 0..11, C = 0 *)
  val octave         : pitch -> int

  (* ---- frequencies (A4 = 440 Hz) ---- *)
  val frequency    : pitch -> real           (* = freqEqual *)
  val freqEqual    : pitch -> real           (* 12-TET equal temperament *)
  val freqJust     : pitch -> pitch -> real  (* freqJust tonic p: 5-limit just *)
  val centsBetween : pitch -> pitch -> real  (* cents from first to second *)

  (* ---- intervals ---- *)
  val interval   : pitch * pitch -> int      (* signed semitones, second - first *)
  val transpose  : pitch -> int -> pitch     (* up by n semitones (sharp spelling) *)
  val octaveUp   : pitch -> pitch            (* +1 octave, keeps spelling *)
  val octaveDown : pitch -> pitch

  (* named interval sizes, in semitones *)
  val unison         : int
  val minorSecond    : int
  val majorSecond    : int
  val minorThird     : int
  val majorThird     : int
  val perfectFourth  : int
  val tritone        : int
  val perfectFifth   : int
  val minorSixth     : int
  val majorSixth     : int
  val minorSeventh   : int
  val majorSeventh   : int
  val octaveInterval : int

  (* ---- scales ---- *)
  datatype scaleType =
      Major | NaturalMinor | HarmonicMinor | MelodicMinor
    | Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    | MajorPentatonic | MinorPentatonic | Chromatic | WholeTone
  (* one octave of the scale (no repeated top note); diatonic scales are
     spelled with consecutive letters (so F major contains Bb, not A#). *)
  val scale : pitch -> scaleType -> pitch list

  (* ---- chords ---- *)
  datatype chordType =
      MajorTriad | MinorTriad | DimTriad | AugTriad
    | Maj7 | Min7 | Dom7 | Dim7 | HalfDim7
  (* root-position chord, spelled in stacked thirds. *)
  val chord     : pitch -> chordType -> pitch list
  (* invert ns k: move the bottom k notes up an octave (k taken mod length). *)
  val invert    : pitch list -> int -> pitch list
  (* identify a chord (any inversion) by name, e.g. SOME "C major". *)
  val chordName : pitch list -> string option

  (* ---- keys ---- *)
  (* major-key signature: +n sharps, or -n flats (C major = 0). *)
  val keySignature   : pitch -> int
  val relativeMinor  : pitch -> pitch        (* down a minor third *)
  val relativeMajor  : pitch -> pitch        (* up a minor third *)
  (* twelve pitches ascending by perfect fifths starting from the argument. *)
  val circleOfFifths : pitch -> pitch list
end
