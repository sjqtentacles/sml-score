(* Tests for sml-score: exact-rational durations, tick conversion, transpose
   via sml-music, score->MIDI event rendering, and deterministic .mid bytes.
   Reference values are computed by hand / from the MIDI + music specs. *)

structure Tests =
struct
  open Harness
  structure R = Score.Rat

  fun pitch s = valOf (Music.parseNote s)

  fun ratStr r = R.toString r
  fun checkRat name (expected, actual) = checkString name (expected, ratStr actual)

  (* render a (delta, event) as "delta status [d0,d1,...]" *)
  fun evStr (delta, ev) =
    let
      val (status, data) = Score.eventStatusData ev
    in
      Int.toString delta ^ " " ^ Int.toString status
      ^ " [" ^ String.concatWith "," (List.map Int.toString data) ^ "]"
    end

  fun fmtReal n r =
    let val s = Real.fmt (StringCvt.FIX (SOME n)) r
    in if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s end

  val ppq = 480

  fun runAll () =
    let
      val () = section "Rat: construction / reduction"
      val () = checkRat "2/4 -> 1/2" ("1/2", R.rat (2, 4))
      val () = checkRat "6/3 -> 2" ("2", R.rat (6, 3))
      val () = checkRat "sign moves to num" ("-1/2", R.rat (1, ~2))
      val () = checkInt "num of 3/2" (3, R.num (R.rat (3, 2)))
      val () = checkInt "den of 3/2" (2, R.den (R.rat (3, 2)))

      val () = section "Rat: arithmetic"
      val () = checkRat "1/2 + 1/3 = 5/6" ("5/6", R.add (R.rat (1,2), R.rat (1,3)))
      val () = checkRat "3/4 - 1/4 = 1/2" ("1/2", R.sub (R.rat (3,4), R.rat (1,4)))
      val () = checkRat "2/3 * 3/4 = 1/2" ("1/2", R.mul (R.rat (2,3), R.rat (3,4)))
      val () = checkRat "1/2 / 1/4 = 2"   ("2", R.divide (R.rat (1,2), R.rat (1,4)))
      val () = checkBool "compare 1/3 < 1/2" (true, R.compare (R.rat(1,3), R.rat(1,2)) = LESS)
      val () = checkBool "eq 2/4 1/2" (true, R.eq (R.rat(2,4), R.rat(1,2)))
      val () = checkRaises "rat div by zero" (fn () => R.rat (1, 0))

      val () = section "Note values (quarter-note beats)"
      val () = checkRat "whole = 4" ("4", Score.whole)
      val () = checkRat "half = 2" ("2", Score.half)
      val () = checkRat "quarter = 1" ("1", Score.quarter)
      val () = checkRat "eighth = 1/2" ("1/2", Score.eighth)
      val () = checkRat "sixteenth = 1/4" ("1/4", Score.sixteenth)
      val () = checkRat "dotted quarter = 3/2" ("3/2", Score.dotted Score.quarter)
      val () = checkRat "double-dotted quarter = 7/4" ("7/4", Score.doubleDotted Score.quarter)
      val () = checkRat "triplet eighth = 1/3" ("1/3", Score.triplet Score.eighth)

      val () = section "Duration -> ticks (PPQ = 480)"
      val () = checkInt "quarter = 480" (480, Score.beatsToTicks ppq Score.quarter)
      val () = checkInt "half = 960" (960, Score.beatsToTicks ppq Score.half)
      val () = checkInt "whole = 1920" (1920, Score.beatsToTicks ppq Score.whole)
      val () = checkInt "eighth = 240" (240, Score.beatsToTicks ppq Score.eighth)
      val () = checkInt "sixteenth = 120" (120, Score.beatsToTicks ppq Score.sixteenth)
      val () = checkInt "dotted quarter = 720" (720, Score.beatsToTicks ppq (Score.dotted Score.quarter))
      val () = checkInt "triplet eighth = 160" (160, Score.beatsToTicks ppq (Score.triplet Score.eighth))
      (* three triplet-eighths sum to one quarter (480 ticks) *)
      val tripSum = R.add (R.add (Score.triplet Score.eighth, Score.triplet Score.eighth),
                           Score.triplet Score.eighth)
      val () = checkRat "3 x triplet-eighth = 1 (quarter)" ("1", tripSum)
      val () = checkInt "3 x triplet-eighth ticks = 480" (480, Score.beatsToTicks ppq tripSum)
      val () = checkRat "ticksToBeats 480 720 = 3/2" ("3/2", Score.ticksToBeats ppq 720)

      val () = section "Transpose via sml-music"
      val c4 = pitch "C4"
      val e4 = Music.transpose c4 Music.majorThird
      val () = checkInt "C4 + M3 -> MIDI 64" (64, Music.toMidi e4)
      val () = checkString "C4 + M3 -> E4" ("E4", Music.noteName e4)
      val partC = { name = "p", channel = 0, program = 0,
                    slots = [ Score.note c4 Score.quarter 80 ] }
      val partE = Score.transposePart Music.majorThird partC
      val () = checkInt "transposePart raises pitch"
                 (64, (case #slots partE of
                          [Score.Tone { pitches = [p], ... }] => Music.toMidi p
                        | _ => ~1))

      val () = section "Chord / scale construction (sml-music)"
      val cmaj = Score.chordOf c4 Music.MajorTriad Score.half 90
      val () = checkIntList "C major triad MIDI = [60,64,67]"
                 ([60,64,67],
                  (case cmaj of
                       Score.Tone { pitches, ... } => List.map Music.toMidi pitches
                     | _ => []))
      val cMajScale = Score.scaleMelody c4 Music.Major Score.eighth 70
      val () = checkInt "C major scale has 7 notes" (7, List.length cMajScale)
      val () = checkIntList "C major scale MIDI"
                 ([60,62,64,65,67,69,71],
                  List.map (fn Score.Tone { pitches = [p], ... } => Music.toMidi p
                             | _ => ~1) cMajScale)

      val () = section "Score -> MIDI event list (3-note melody)"
      val melody = { name = "lead", channel = 0, program = 0,
                     slots = [ Score.note (pitch "C4") Score.quarter 80,
                               Score.note (pitch "E4") Score.quarter 80,
                               Score.note (pitch "G4") Score.quarter 80 ] }
      val trk = Score.partTrack ppq melody
      val () = checkStringList "melody delta track (delta status [data])"
                 ([ "0 144 [60,80]",   "480 128 [60,0]",
                    "0 144 [64,80]",   "480 128 [64,0]",
                    "0 144 [67,80]",   "480 128 [67,0]" ],
                  List.map evStr trk)

      val () = section "Absolute events: stable ordering (off before on)"
      val repeated = { name = "r", channel = 0, program = 0,
                       slots = [ Score.note (pitch "C4") Score.quarter 80,
                                 Score.note (pitch "C4") Score.quarter 80 ] }
      val abs = Score.absEvents ppq repeated
      val () = checkStringList "off precedes on at the shared tick"
                 ([ "0 NoteOn 60", "480 NoteOff 60", "480 NoteOn 60", "960 NoteOff 60" ],
                  List.map (fn (t, ev) =>
                               Int.toString t ^ " " ^
                               (case ev of
                                    Midi.NoteOn _ => "NoteOn"
                                  | Midi.NoteOff _ => "NoteOff"
                                  | _ => "?") ^ " " ^
                               (case ev of
                                    Midi.NoteOn { note, ... } => Int.toString note
                                  | Midi.NoteOff { note, ... } => Int.toString note
                                  | _ => "?")) abs)

      val () = section "Chord absEvents ordering by note"
      val chordPart = { name = "c", channel = 0, program = 0,
                        slots = [ Score.chordSlot [pitch "G4", pitch "C4", pitch "E4"]
                                                  Score.quarter 90 ] }
      val () = checkStringList "chord note-ons ascending by note"
                 ([ "0 144 [60,90]", "0 144 [64,90]", "0 144 [67,90]",
                    "480 128 [60,0]", "0 128 [64,0]", "0 128 [67,0]" ],
                  List.map evStr (Score.partTrack ppq chordPart))

      val () = section "Analysis"
      val score3 = { tempo = 120, timeSig = { num = 4, den = 4 }, ppq = ppq,
                     parts = [ melody ] }
      val () = checkRat "scoreBeats melody = 3" ("3", Score.scoreBeats score3)
      val () = checkInt "scoreTicks melody = 1440" (1440, Score.scoreTicks score3)
      val () = checkInt "noteCount = 3" (3, Score.noteCount score3)
      (* 4 quarters at 120 BPM = 2.0 s; here 3 quarters = 1.5 s *)
      val () = checkString "scoreSeconds 3q @120 = 1.500" ("1.500", fmtReal 3 (Score.scoreSeconds score3))
      val four = { tempo = 120, timeSig = { num = 4, den = 4 }, ppq = ppq,
                   parts = [ { name = "p", channel = 0, program = 0,
                               slots = [ Score.note c4 Score.whole 80 ] } ] }
      val () = checkString "scoreSeconds whole @120 = 2.000" ("2.000", fmtReal 3 (Score.scoreSeconds four))

      val () = section "noteCount counts chord members"
      val () = checkInt "chord triad counts as 3"
                 (3, Score.noteCount { tempo = 120, timeSig = { num = 4, den = 4 },
                                       ppq = ppq, parts = [ chordPart ] })

      val () = section "MIDI file bytes deterministic"
      val bytes = Score.toBytes score3
      val () = checkBool "starts with MThd" (true, String.isPrefix "MThd" bytes)
      val () = checkInt "format-1, 2 tracks (conductor + 1 part)"
                 (2, Midi.ntracks (Midi.parse bytes))
      (* canonical byte length & Adler-32 checksum captured from the build *)
      val () = checkInt "byte length" (104, String.size bytes)
      val () = checkString "checksum" ("1544428533", LargeInt.toString (Score.checksum bytes))

      val () = section "renderText (ABC-lite)"
      val () = checkString "melody text"
                 ("C4:q E4:q G4:q", Score.renderText melody)
      val () = checkString "rest + dotted + triplet labels"
                 ("z:h C4:q. E4:e3",
                  Score.renderText
                    { name = "x", channel = 0, program = 0,
                      slots = [ Score.rest Score.half,
                                Score.note c4 (Score.dotted Score.quarter) 80,
                                Score.note e4 (Score.triplet Score.eighth) 80 ] })
    in
      Harness.run ()
    end

  val run = runAll
end
