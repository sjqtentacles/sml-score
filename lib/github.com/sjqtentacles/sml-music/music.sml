(* music.sml - music theory primitives in pure Standard ML.

   Pitches are spelled (letter + signed accidental + octave). Letters are
   indexed 0..6 in the musical alphabet starting at C: C D E F G A B. This
   makes "next letter" arithmetic (for diatonic scale and chord spelling) a
   simple mod-7 step that also tracks octave wraps at C. *)

structure Music :> MUSIC =
struct

  type pitch = { letter : int, acc : int, oct : int }

  (* letter index 0..6 = C D E F G A B *)
  val letterChars = "CDEFGAB"
  val letterSemis = Vector.fromList [0, 2, 4, 5, 7, 9, 11]   (* semitone of natural letter *)

  fun letterSemi li = Vector.sub (letterSemis, li)
  fun letterChar li = String.sub (letterChars, li)

  fun letterIndex c =
    case c of
        #"C" => SOME 0 | #"D" => SOME 1 | #"E" => SOME 2 | #"F" => SOME 3
      | #"G" => SOME 4 | #"A" => SOME 5 | #"B" => SOME 6 | _ => NONE

  fun mk (letter, acc, oct) = { letter = letter, acc = acc, oct = oct }

  fun octave ({ oct, ... } : pitch) = oct

  fun toMidi ({ letter, acc, oct } : pitch) =
    12 * (oct + 1) + letterSemi letter + acc

  fun pitchClass p = (toMidi p) mod 12

  (* ---- naming ---- *)
  fun accString acc =
    if acc > 0 then String.implode (List.tabulate (acc, fn _ => #"#"))
    else if acc < 0 then String.implode (List.tabulate (~acc, fn _ => #"b"))
    else ""

  fun octString oct =
    if oct < 0 then "-" ^ Int.toString (~oct) else Int.toString oct

  fun pitchClassName ({ letter, acc, ... } : pitch) =
    String.str (letterChar letter) ^ accString acc

  fun noteName (p as { oct, ... } : pitch) =
    pitchClassName p ^ octString oct

  (* canonical sharp spelling for a pitch class 0..11 *)
  val sharpLetter = Vector.fromList [0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6] (* letter index *)
  val sharpAcc    = Vector.fromList [0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0] (* accidental *)

  fun fromMidi m =
    let
      val pc = m mod 12
      val oct = (m div 12) - 1
    in
      mk (Vector.sub (sharpLetter, pc), Vector.sub (sharpAcc, pc), oct)
    end

  (* strict integer parse: optional - / ~ then digits only *)
  fun parseIntStrict s =
    let
      val cs = String.explode s
      val (neg, ds) =
        case cs of
            (#"-" :: t) => (true, t)
          | (#"~" :: t) => (true, t)
          | _ => (false, cs)
    in
      if not (List.null ds) andalso List.all Char.isDigit ds then
        let val mag = List.foldl (fn (c, a) => a * 10 + (Char.ord c - Char.ord #"0")) 0 ds
        in SOME (if neg then ~mag else mag) end
      else NONE
    end

  fun parseNote s =
    case String.explode s of
        [] => NONE
      | (l :: rest) =>
          (case letterIndex l of
               NONE => NONE
             | SOME li =>
                 let
                   fun accs (#"#" :: r) a = accs r (a + 1)
                     | accs (#"b" :: r) a = accs r (a - 1)
                     | accs r a = (r, a)
                   val (r2, acc) = accs rest 0
                 in
                   case parseIntStrict (String.implode r2) of
                       SOME oct => SOME (mk (li, acc, oct))
                     | NONE => NONE
                 end)

  (* ---- frequencies ---- *)
  fun freqEqual p = 440.0 * Math.pow (2.0, Real.fromInt (toMidi p - 69) / 12.0)
  val frequency = freqEqual

  (* 5-limit just-intonation ratios per semitone within an octave *)
  val justNum = Vector.fromList [1, 16, 9, 6, 5, 4, 45, 3, 8, 5, 9, 15]
  val justDen = Vector.fromList [1, 15, 8, 5, 4, 3, 32, 2, 5, 3, 5,  8]

  fun freqJust tonic p =
    let
      val base = freqEqual tonic
      val semis = toMidi p - toMidi tonic
      val oct = semis div 12          (* floor division (SML) *)
      val step = semis mod 12         (* 0..11 *)
      val ratio = Real.fromInt (Vector.sub (justNum, step))
                  / Real.fromInt (Vector.sub (justDen, step))
    in
      base * ratio * Math.pow (2.0, Real.fromInt oct)
    end

  fun centsBetween a b =
    1200.0 * Math.ln (freqEqual b / freqEqual a) / Math.ln 2.0

  (* ---- intervals ---- *)
  fun interval (a, b) = toMidi b - toMidi a
  fun transpose p n = fromMidi (toMidi p + n)
  fun octaveUp ({ letter, acc, oct } : pitch) = mk (letter, acc, oct + 1)
  fun octaveDown ({ letter, acc, oct } : pitch) = mk (letter, acc, oct - 1)

  val unison = 0
  val minorSecond = 1
  val majorSecond = 2
  val minorThird = 3
  val majorThird = 4
  val perfectFourth = 5
  val tritone = 6
  val perfectFifth = 7
  val minorSixth = 8
  val majorSixth = 9
  val minorSeventh = 10
  val majorSeventh = 11
  val octaveInterval = 12

  (* ---- scales ---- *)
  datatype scaleType =
      Major | NaturalMinor | HarmonicMinor | MelodicMinor
    | Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    | MajorPentatonic | MinorPentatonic | Chromatic | WholeTone

  fun scaleOffsets st =
    case st of
        Major           => [0,2,4,5,7,9,11]
      | Ionian          => [0,2,4,5,7,9,11]
      | NaturalMinor    => [0,2,3,5,7,8,10]
      | Aeolian         => [0,2,3,5,7,8,10]
      | HarmonicMinor   => [0,2,3,5,7,8,11]
      | MelodicMinor    => [0,2,3,5,7,9,11]
      | Dorian          => [0,2,3,5,7,9,10]
      | Phrygian        => [0,1,3,5,7,8,10]
      | Lydian          => [0,2,4,6,7,9,11]
      | Mixolydian      => [0,2,4,5,7,9,10]
      | Locrian         => [0,1,3,5,6,8,10]
      | MajorPentatonic => [0,2,4,7,9]
      | MinorPentatonic => [0,3,5,7,10]
      | Chromatic       => [0,1,2,3,4,5,6,7,8,9,10,11]
      | WholeTone       => [0,2,4,6,8,10]

  fun isHeptatonic st =
    case st of
        MajorPentatonic => false | MinorPentatonic => false
      | Chromatic => false | WholeTone => false | _ => true

  (* spell a sequence given a per-note letter step from the root letter. *)
  fun spellByLetters root letterSteps offs =
    let
      val L0 = #letter (root : pitch)
      val rootMidi = toMidi root
      val rootOct = #oct root
      fun build (_, [], _) = []
        | build (i, ls :: lss, off :: offrest) =
            let
              val li = L0 + ls
              val letter = li mod 7
              val octBump = li div 7
              val oct = rootOct + octBump
              val target = rootMidi + off
              val nat = 12 * (oct + 1) + letterSemi letter
              val acc = target - nat
            in
              mk (letter, acc, oct) :: build (i + 1, lss, offrest)
            end
        | build _ = []
    in
      build (0, letterSteps, offs)
    end

  fun scale root st =
    let val offs = scaleOffsets st
    in
      if isHeptatonic st then
        spellByLetters root (List.tabulate (List.length offs, fn i => i)) offs
      else
        List.map (transpose root) offs
    end

  (* ---- chords ---- *)
  datatype chordType =
      MajorTriad | MinorTriad | DimTriad | AugTriad
    | Maj7 | Min7 | Dom7 | Dim7 | HalfDim7

  fun chordOffsets ct =
    case ct of
        MajorTriad => [0,4,7]
      | MinorTriad => [0,3,7]
      | DimTriad   => [0,3,6]
      | AugTriad   => [0,4,8]
      | Maj7       => [0,4,7,11]
      | Min7       => [0,3,7,10]
      | Dom7       => [0,4,7,10]
      | Dim7       => [0,3,6,9]
      | HalfDim7   => [0,3,6,10]

  fun chord root ct =
    let
      val offs = chordOffsets ct
      val letterSteps = List.tabulate (List.length offs, fn i => 2 * i)
    in
      spellByLetters root letterSteps offs
    end

  fun invert notes k =
    let
      val n = List.length notes
    in
      if n = 0 then []
      else
        let
          val k = k mod n
          val front = List.take (notes, k)
          val back = List.drop (notes, k)
        in
          back @ List.map octaveUp front
        end
    end

  (* sort + dedup a small int list (insertion sort) *)
  fun insSorted (x, []) = [x]
    | insSorted (x, y :: ys) =
        if x < y then x :: y :: ys
        else if x = y then y :: ys
        else y :: insSorted (x, ys)
  fun sortUniq xs = List.foldr insSorted [] xs

  val chordShapes =
    [ ([0,4,7],    "major")
    , ([0,3,7],    "minor")
    , ([0,3,6],    "diminished")
    , ([0,4,8],    "augmented")
    , ([0,4,7,11], "major7")
    , ([0,3,7,10], "minor7")
    , ([0,4,7,10], "dominant7")
    , ([0,3,6,9],  "diminished7")
    , ([0,3,6,10], "half-diminished7") ]

  fun lookupShape offs =
    case List.find (fn (k, _) => k = offs) chordShapes of
        SOME (_, name) => SOME name
      | NONE => NONE

  fun chordName notes =
    let
      fun tryRoot r =
        let
          val rpc = pitchClass r
          val offs = sortUniq (List.map (fn p => (pitchClass p - rpc) mod 12) notes)
        in
          case lookupShape offs of
              SOME q => SOME (pitchClassName r ^ " " ^ q)
            | NONE => NONE
        end
      fun firstSome [] = NONE
        | firstSome (p :: t) =
            (case tryRoot p of SOME x => SOME x | NONE => firstSome t)
    in
      firstSome notes
    end

  (* ---- keys ---- *)
  fun keySignature root =
    List.foldl (fn (p : pitch, a) => a + #acc p) 0 (scale root Major)

  fun relativeMinor root = transpose root (~3)
  fun relativeMajor root = transpose root 3

  fun circleOfFifths root =
    List.tabulate (12, fn i => transpose root (7 * i))
end
