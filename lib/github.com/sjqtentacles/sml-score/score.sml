(* score.sml - a musical score model on top of sml-music and sml-midi.

   Durations are exact rationals of quarter-note beats; PPQ maps them to MIDI
   ticks. Rendering produces an sml-midi `smf` (format 1: one conductor track
   carrying tempo/time-signature, then one track per part) which sml-midi's
   `write` serializes to a byte-identical .mid file under both compilers. *)

structure Score :> SCORE =
struct

  (* ---------------- rationals ---------------- *)
  structure Rat =
  struct
    type t = int * int            (* (num, den), reduced, den > 0 *)
    exception DivZero

    fun gcd (a, 0) = a
      | gcd (a, b) = gcd (b, a mod b)

    fun reduce (n, d) =
      if d = 0 then raise DivZero
      else
        let
          val s = if d < 0 then ~1 else 1
          val n = n * s
          val d = d * s
          val g = gcd (abs n, d)
          val g = if g = 0 then 1 else g
        in
          (n div g, d div g)
        end

    fun rat (n, d) = reduce (n, d)
    fun fromInt n = (n, 1)
    fun num (n, _) = n
    fun den (_, d) = d
    fun add ((a, b), (c, d)) = reduce (a * d + c * b, b * d)
    fun sub ((a, b), (c, d)) = reduce (a * d - c * b, b * d)
    fun mul ((a, b), (c, d)) = reduce (a * c, b * d)
    fun divide ((a, b), (c, d)) =
      if c = 0 then raise DivZero else reduce (a * d, b * c)
    fun compare ((a, b), (c, d)) = Int.compare (a * d, c * b)
    fun eq (x, y) = compare (x, y) = EQUAL
    fun toReal (n, d) = Real.fromInt n / Real.fromInt d
    (* leading '-' (not SML's '~') for negatives, matching the house style *)
    fun intStr n = if n < 0 then "-" ^ Int.toString (~n) else Int.toString n
    fun toString (n, d) = if d = 1 then intStr n
                          else intStr n ^ "/" ^ Int.toString d
  end

  type rational = Rat.t

  (* ---------------- note values (in quarter-note beats) ---------------- *)
  val whole        = Rat.rat (4, 1)
  val half         = Rat.rat (2, 1)
  val quarter      = Rat.rat (1, 1)
  val eighth       = Rat.rat (1, 2)
  val sixteenth    = Rat.rat (1, 4)
  val thirtysecond = Rat.rat (1, 8)

  fun dotted d = Rat.mul (d, Rat.rat (3, 2))
  fun doubleDotted d = Rat.mul (d, Rat.rat (7, 4))
  (* n notes in the time of m: scale each by m/n *)
  fun tuplet n m d = Rat.mul (d, Rat.rat (m, n))
  fun triplet d = tuplet 3 2 d

  (* ---------------- ticks ---------------- *)
  (* ticks = beats * ppq ; exact when ppq is divisible by the denominator. *)
  fun beatsToTicks ppq r =
    (Rat.num r * ppq) div (Rat.den r)
  fun ticksToBeats ppq t = Rat.rat (t, ppq)

  (* ---------------- model ---------------- *)
  datatype slot =
      Rest of rational
    | Tone of { pitches : Music.pitch list, dur : rational, vel : int }

  type part = { name : string, channel : int, program : int, slots : slot list }
  type timeSig = { num : int, den : int }
  type score = { tempo : int, timeSig : timeSig, ppq : int, parts : part list }

  fun note p d v = Tone { pitches = [p], dur = d, vel = v }
  fun rest d = Rest d
  fun chordSlot ps d v = Tone { pitches = ps, dur = d, vel = v }

  (* ---------------- construction from sml-music ---------------- *)
  fun chordOf root ct d v = Tone { pitches = Music.chord root ct, dur = d, vel = v }

  fun scaleMelody root st d v =
    List.map (fn p => Tone { pitches = [p], dur = d, vel = v })
             (Music.scale root st)

  fun mapPitches f slot =
    case slot of
        Rest d => Rest d
      | Tone { pitches, dur, vel } =>
          Tone { pitches = List.map f pitches, dur = dur, vel = vel }

  fun transposePart n ({ name, channel, program, slots } : part) : part =
    { name = name, channel = channel, program = program,
      slots = List.map (mapPitches (fn p => Music.transpose p n)) slots }

  fun transposeScore n ({ tempo, timeSig, ppq, parts } : score) : score =
    { tempo = tempo, timeSig = timeSig, ppq = ppq,
      parts = List.map (transposePart n) parts }

  (* ---------------- analysis ---------------- *)
  fun slotDur (Rest d) = d
    | slotDur (Tone { dur, ... }) = dur

  fun partBeats ({ slots, ... } : part) =
    List.foldl (fn (s, acc) => Rat.add (acc, slotDur s)) (Rat.fromInt 0) slots

  fun scoreBeats ({ parts, ... } : score) =
    List.foldl (fn (p, acc) =>
                   let val b = partBeats p
                   in if Rat.compare (b, acc) = GREATER then b else acc end)
               (Rat.fromInt 0) parts

  fun partTicks ppq p = beatsToTicks ppq (partBeats p)
  fun scoreTicks (sc as { ppq, ... } : score) = beatsToTicks ppq (scoreBeats sc)

  (* seconds = beats * 60 / BPM (beats are quarter notes; BPM = quarters/min). *)
  fun scoreSeconds (sc as { tempo, ... } : score) =
    Rat.toReal (scoreBeats sc) * 60.0 / Real.fromInt tempo

  fun noteCount ({ parts, ... } : score) =
    List.foldl
      (fn ({ slots, ... } : part, acc) =>
          List.foldl (fn (Rest _, a) => a
                       | (Tone { pitches, ... }, a) => a + List.length pitches)
                     acc slots)
      0 parts

  (* ---------------- MIDI rendering ---------------- *)
  (* stable merge sort by a key comparator *)
  fun mergeSort cmp xs =
    let
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            (case cmp (x, y) of
                 GREATER => y :: merge (x :: xs, ys)
               | _ => x :: merge (xs, y :: ys))   (* LESS/EQUAL keep x first: stable *)
      fun split (xs) =
        let
          fun go ([], a, b) = (List.rev a, List.rev b)
            | go ([x], a, b) = (List.rev (x :: a), List.rev b)
            | go (x :: y :: t, a, b) = go (t, x :: a, y :: b)
        in go (xs, [], []) end
    in
      case xs of
          [] => []
        | [x] => [x]
        | _ => let val (a, b) = split xs
               in merge (mergeSort cmp a, mergeSort cmp b) end
    end

  (* a raw timed event before delta computation *)
  (* kind: 0 = note-off (sorts first at a tick), 1 = note-on *)
  type timed = { tick : int, kind : int, note : int, ev : Midi.event }

  fun toneEvents ppq chan { tick0 } slot =
    case slot of
        Rest _ => []
      | Tone { pitches, dur, vel } =>
          let
            val d = beatsToTicks ppq dur
          in
            List.concat
              (List.map
                 (fn p =>
                    let val m = Music.toMidi p
                    in [ { tick = tick0, kind = 1, note = m,
                           ev = Midi.NoteOn { chan = chan, note = m, vel = vel } },
                         { tick = tick0 + d, kind = 0, note = m,
                           ev = Midi.NoteOff { chan = chan, note = m, vel = 0 } } ]
                    end)
                 pitches)
          end

  fun collectTimed ppq chan slots =
    let
      fun go (_, []) = []
        | go (t, s :: rest) =
            let val d = beatsToTicks ppq (slotDur s)
            in toneEvents ppq chan { tick0 = t } s @ go (t + d, rest) end
    in
      go (0, slots)
    end

  fun timedCmp (a : timed, b : timed) =
    case Int.compare (#tick a, #tick b) of
        EQUAL =>
          (case Int.compare (#kind a, #kind b) of
               EQUAL => Int.compare (#note a, #note b)
             | o2 => o2)
      | o1 => o1

  fun absEvents ppq ({ channel, slots, ... } : part) =
    let
      val timed = mergeSort timedCmp (collectTimed ppq channel slots)
    in
      List.map (fn t => (#tick t, #ev t)) timed
    end

  fun toDeltas pairs =
    let
      fun go (_, []) = []
        | go (prev, (tick, ev) :: rest) = (tick - prev, ev) :: go (tick, rest)
    in
      go (0, pairs)
    end

  fun partTrack ppq part = toDeltas (absEvents ppq part)

  fun conductorTrack ({ tempo, timeSig = { num, den }, ... } : score) =
    let
      val usPerQuarter = 60000000 div tempo
    in
      [ (0, Midi.Meta (Midi.TrackName "Conductor")),
        (0, Midi.Meta (Midi.TimeSignature
                         { num = num, den = den, clocks = 24, notes32 = 8 })),
        (0, Midi.Meta (Midi.SetTempo usPerQuarter)),
        (0, Midi.Meta Midi.EndOfTrack) ]
    end

  fun partToTrack ppq ({ name, channel, program, slots } : part) =
    let
      val head =
        [ (0, Midi.Meta (Midi.TrackName name)),
          (0, Midi.ProgramChange { chan = channel, program = program }) ]
      val body = partTrack ppq { name = name, channel = channel,
                                 program = program, slots = slots }
    in
      head @ body @ [ (0, Midi.Meta Midi.EndOfTrack) ]
    end

  fun toSmf (sc as { ppq, parts, ... } : score) =
    { format = 1,
      division = ppq,
      tracks = conductorTrack sc :: List.map (partToTrack ppq) parts }

  fun toBytes sc = Midi.write (toSmf sc)

  (* ---------------- inspection / rendering ---------------- *)
  fun eventStatusData ev =
    case ev of
        Midi.NoteOff { chan, note, vel } => (0x80 + chan, [note, vel])
      | Midi.NoteOn { chan, note, vel } => (0x90 + chan, [note, vel])
      | Midi.KeyPressure { chan, note, pressure } => (0xA0 + chan, [note, pressure])
      | Midi.ControlChange { chan, ctrl, value } => (0xB0 + chan, [ctrl, value])
      | Midi.ProgramChange { chan, program } => (0xC0 + chan, [program])
      | Midi.ChannelPressure { chan, pressure } => (0xD0 + chan, [pressure])
      | Midi.PitchBend { chan, value } => (0xE0 + chan, [value mod 128, value div 128])
      | Midi.SysEx _ => (0xF0, [])
      | Midi.Meta _ => (0xFF, [])

  (* label common note values; fall back to the fraction *)
  fun durLabel d =
    let
      val baseLabels =
        [ (whole, "w"), (half, "h"), (quarter, "q"),
          (eighth, "e"), (sixteenth, "s"), (thirtysecond, "t") ]
      fun findBase [] = NONE
        | findBase ((b, lbl) :: rest) =
            if Rat.eq (d, b) then SOME lbl
            else if Rat.eq (d, dotted b) then SOME (lbl ^ ".")
            else if Rat.eq (d, triplet b) then SOME (lbl ^ "3")
            else findBase rest
    in
      case findBase baseLabels of
          SOME s => s
        | NONE => Rat.toString d
    end

  fun renderText ({ slots, ... } : part) =
    let
      fun render (Rest d) = "z:" ^ durLabel d
        | render (Tone { pitches, dur, ... }) =
            let
              val names =
                case pitches of
                    [p] => Music.noteName p
                  | ps => "[" ^ String.concatWith "" (List.map Music.noteName ps) ^ "]"
            in
              names ^ ":" ^ durLabel dur
            end
    in
      String.concatWith " " (List.map render slots)
    end

  (* Adler-32 checksum; combined value kept exact via LargeInt. *)
  fun checksum s =
    let
      val n = String.size s
      fun go (i, a, b) =
        if i >= n then (a, b)
        else
          let
            val a' = (a + Char.ord (String.sub (s, i))) mod 65521
            val b' = (b + a') mod 65521
          in go (i + 1, a', b') end
      val (a, b) = go (0, 1, 0)
    in
      LargeInt.fromInt b * 65536 + LargeInt.fromInt a
    end

end
