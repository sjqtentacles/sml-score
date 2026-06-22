(* demo.sml - build a small score on top of sml-music and render it to MIDI
   with sml-midi. Deterministic: identical stdout and identical assets/demo.mid
   bytes on every run and under both MLton and Poly/ML. *)

structure R = Score.Rat

fun p s = valOf (Music.parseNote s)

fun fmtReal n r =
  let val s = Real.fmt (StringCvt.FIX (SOME n)) r
  in if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s end

val ppq = 480

(* ---- note-value tick math ---- *)
val () = print ("Note values at PPQ = " ^ Int.toString ppq ^ ":\n")
val durs = [ ("whole", Score.whole), ("half", Score.half),
             ("quarter", Score.quarter), ("eighth", Score.eighth),
             ("dotted-quarter", Score.dotted Score.quarter),
             ("triplet-eighth", Score.triplet Score.eighth) ]
val () =
  List.app
    (fn (name, d) =>
       print ("  " ^ name ^ " = " ^ R.toString d ^ " beat -> "
              ^ Int.toString (Score.beatsToTicks ppq d) ^ " ticks\n"))
    durs

(* ---- chord + transpose via sml-music ---- *)
val () = print "\nC major triad (sml-music Music.chord):\n"
val triad = Music.chord (p "C4") Music.MajorTriad
val () = print ("  pitches = "
                ^ String.concatWith " " (List.map Music.noteName triad)
                ^ "  (MIDI "
                ^ String.concatWith "," (List.map (Int.toString o Music.toMidi) triad)
                ^ ")\n")
val () = print ("  transpose up a major third -> "
                ^ String.concatWith " "
                    (List.map (fn q => Music.noteName (Music.transpose q Music.majorThird)) triad)
                ^ "\n")

(* ---- a tiny melody and its MIDI event list ---- *)
val melody = { name = "lead", channel = 0, program = 0,
               slots = [ Score.note (p "C4") Score.quarter 80,
                         Score.note (p "E4") Score.quarter 80,
                         Score.note (p "G4") Score.quarter 80 ] }
val () = print "\nMelody (C4 E4 G4, quarters) as MIDI delta events:\n"
val () = print ("  ABC-lite: " ^ Score.renderText melody ^ "\n")
val () =
  List.app
    (fn (delta, ev) =>
       let val (status, data) = Score.eventStatusData ev
       in print ("  d=" ^ Int.toString delta
                 ^ " status=" ^ Int.toString status
                 ^ " data=[" ^ String.concatWith "," (List.map Int.toString data) ^ "]\n")
       end)
    (Score.partTrack ppq melody)

(* ---- a 2-part score: chords under the melody ---- *)
val chords = { name = "chords", channel = 1, program = 0,
               slots = [ Score.chordOf (p "C4") Music.MajorTriad Score.half 70,
                         Score.chordOf (p "G3") Music.MajorTriad Score.half 70 ] }
val sc = { tempo = 120, timeSig = { num = 4, den = 4 }, ppq = ppq,
           parts = [ melody, chords ] }

val () = print "\nScore analysis:\n"
val () = print ("  parts        = " ^ Int.toString (List.length (#parts sc)) ^ "\n")
val () = print ("  total beats  = " ^ R.toString (Score.scoreBeats sc) ^ "\n")
val () = print ("  total ticks  = " ^ Int.toString (Score.scoreTicks sc) ^ "\n")
val () = print ("  duration     = " ^ fmtReal 3 (Score.scoreSeconds sc) ^ " s @ "
                ^ Int.toString (#tempo sc) ^ " BPM\n")
val () = print ("  note count   = " ^ Int.toString (Score.noteCount sc) ^ "\n")

(* ---- render to a Standard MIDI File ---- *)
val bytes = Score.toBytes sc
val () =
  let val out = BinIO.openOut "assets/demo.mid"
  in BinIO.output (out, Byte.stringToBytes bytes); BinIO.closeOut out end
val () = print "\nWrote assets/demo.mid:\n"
val () = print ("  format       = 1 (conductor + " ^ Int.toString (List.length (#parts sc)) ^ " parts)\n")
val () = print ("  byte length  = " ^ Int.toString (String.size bytes) ^ "\n")
val () = print ("  adler32      = " ^ LargeInt.toString (Score.checksum bytes) ^ "\n")
