(* midi.sml - Standard MIDI File (SMF) codec in pure Standard ML.

   Bytes are `string`s, one character per byte (0..255). Integers in the file
   are big-endian. Writing always emits explicit status bytes; parsing expands
   running status. *)

structure Midi :> MIDI =
struct

  exception Malformed of string

  (* ---------------- byte helpers ---------------- *)
  fun byte n = String.str (Char.chr (n mod 256))
  fun u16 n = byte (n div 256) ^ byte n
  fun u24 n = byte (n div 65536) ^ byte (n div 256) ^ byte n
  fun u32 n = byte (n div 16777216) ^ byte (n div 65536) ^ byte (n div 256) ^ byte n

  fun ordAt (s, i) = Char.ord (String.sub (s, i))

  fun rdU16 (s, i) = ordAt (s, i) * 256 + ordAt (s, i + 1)
  fun rdU24 (s, i) = ordAt (s, i) * 65536 + ordAt (s, i + 1) * 256 + ordAt (s, i + 2)
  fun rdU32 (s, i) =
    ordAt (s, i) * 16777216 + ordAt (s, i + 1) * 65536
    + ordAt (s, i + 2) * 256 + ordAt (s, i + 3)

  fun signedByte b = if b >= 128 then b - 256 else b
  fun byteOfSigned n = byte ((n + 256) mod 256)

  fun pow2 0 = 1 | pow2 n = 2 * pow2 (n - 1)
  fun ilog2 n = if n <= 1 then 0 else 1 + ilog2 (n div 2)

  fun toHex s =
    let
      val digits = "0123456789ABCDEF"
      fun hx c =
        let val n = Char.ord c
        in String.str (String.sub (digits, n div 16))
           ^ String.str (String.sub (digits, n mod 16))
        end
    in
      String.concatWith " " (List.map hx (String.explode s))
    end

  (* ---------------- VLQ ---------------- *)
  fun writeVLQ n =
    if n < 0 then raise Malformed "writeVLQ: negative"
    else
      let
        (* seven-bit groups, most significant first *)
        fun groups 0 = [0]
          | groups x =
              let fun go 0 = [] | go y = go (y div 128) @ [y mod 128]
              in go x end
        val gs = groups n
        val k = List.length gs
        fun emit (_, []) = ""
          | emit (i, g :: rest) =
              byte (if i < k - 1 then g + 128 else g) ^ emit (i + 1, rest)
      in
        emit (0, gs)
      end

  fun readVLQ s i =
    let
      fun go (i, acc, count) =
        if i >= String.size s orelse count >= 5 then NONE
        else
          let
            val b = ordAt (s, i)
            val acc' = acc * 128 + (b mod 128)
          in
            if b >= 128 then go (i + 1, acc', count + 1)
            else SOME (acc', i + 1)
          end
    in
      go (i, 0, 0)
    end

  (* ---------------- event model ---------------- *)
  datatype meta =
      SequenceNumber of int
    | Text of string
    | Copyright of string
    | TrackName of string
    | InstrumentName of string
    | Lyric of string
    | Marker of string
    | CuePoint of string
    | ChannelPrefix of int
    | EndOfTrack
    | SetTempo of int
    | SMPTEOffset of string
    | TimeSignature of { num : int, den : int, clocks : int, notes32 : int }
    | KeySignature of { sf : int, minor : bool }
    | SequencerSpecific of string
    | UnknownMeta of int * string

  datatype event =
      NoteOff        of { chan : int, note : int, vel : int }
    | NoteOn         of { chan : int, note : int, vel : int }
    | KeyPressure    of { chan : int, note : int, pressure : int }
    | ControlChange  of { chan : int, ctrl : int, value : int }
    | ProgramChange  of { chan : int, program : int }
    | ChannelPressure of { chan : int, pressure : int }
    | PitchBend      of { chan : int, value : int }
    | SysEx          of string
    | Meta           of meta

  type track = (int * event) list
  type smf = { format : int, division : int, tracks : track list }

  fun ntracks ({ tracks, ... } : smf) = List.length tracks

  (* ---------------- meta encode ---------------- *)
  (* returns (typeByte, payload) *)
  fun encMeta m =
    case m of
        SequenceNumber n => (0x00, u16 n)
      | Text s           => (0x01, s)
      | Copyright s      => (0x02, s)
      | TrackName s      => (0x03, s)
      | InstrumentName s => (0x04, s)
      | Lyric s          => (0x05, s)
      | Marker s         => (0x06, s)
      | CuePoint s       => (0x07, s)
      | ChannelPrefix n  => (0x20, byte n)
      | EndOfTrack       => (0x2F, "")
      | SetTempo t       => (0x51, u24 t)
      | SMPTEOffset raw  => (0x54, raw)
      | TimeSignature { num, den, clocks, notes32 } =>
          (0x58, byte num ^ byte (ilog2 den) ^ byte clocks ^ byte notes32)
      | KeySignature { sf, minor } =>
          (0x59, byteOfSigned sf ^ byte (if minor then 1 else 0))
      | SequencerSpecific raw => (0x7F, raw)
      | UnknownMeta (t, raw)  => (t, raw)

  (* ---------------- event encode ---------------- *)
  fun encEvent ev =
    case ev of
        NoteOff { chan, note, vel } => byte (0x80 + chan) ^ byte note ^ byte vel
      | NoteOn { chan, note, vel }  => byte (0x90 + chan) ^ byte note ^ byte vel
      | KeyPressure { chan, note, pressure } =>
          byte (0xA0 + chan) ^ byte note ^ byte pressure
      | ControlChange { chan, ctrl, value } =>
          byte (0xB0 + chan) ^ byte ctrl ^ byte value
      | ProgramChange { chan, program } => byte (0xC0 + chan) ^ byte program
      | ChannelPressure { chan, pressure } => byte (0xD0 + chan) ^ byte pressure
      | PitchBend { chan, value } =>
          byte (0xE0 + chan) ^ byte (value mod 128) ^ byte (value div 128)
      | SysEx data => byte 0xF0 ^ writeVLQ (String.size data) ^ data
      | Meta m =>
          let val (t, payload) = encMeta m
          in byte 0xFF ^ byte t ^ writeVLQ (String.size payload) ^ payload end

  fun encTrack (tr : track) =
    let
      val body =
        String.concat
          (List.map (fn (delta, ev) => writeVLQ delta ^ encEvent ev) tr)
    in
      "MTrk" ^ u32 (String.size body) ^ body
    end

  fun write ({ format, division, tracks } : smf) =
    let
      val header =
        "MThd" ^ u32 6 ^ u16 format ^ u16 (List.length tracks)
        ^ u16 ((division + 65536) mod 65536)
    in
      header ^ String.concat (List.map encTrack tracks)
    end

  (* ---------------- meta decode ---------------- *)
  fun decMeta (t, data) =
    let val n = String.size data in
      case t of
          0x00 => SequenceNumber (if n >= 2 then rdU16 (data, 0) else 0)
        | 0x01 => Text data
        | 0x02 => Copyright data
        | 0x03 => TrackName data
        | 0x04 => InstrumentName data
        | 0x05 => Lyric data
        | 0x06 => Marker data
        | 0x07 => CuePoint data
        | 0x20 => ChannelPrefix (if n >= 1 then ordAt (data, 0) else 0)
        | 0x2F => EndOfTrack
        | 0x51 => SetTempo (if n >= 3 then rdU24 (data, 0) else 0)
        | 0x54 => SMPTEOffset data
        | 0x58 =>
            if n >= 4 then
              TimeSignature { num = ordAt (data, 0), den = pow2 (ordAt (data, 1)),
                              clocks = ordAt (data, 2), notes32 = ordAt (data, 3) }
            else UnknownMeta (t, data)
        | 0x59 =>
            if n >= 2 then
              KeySignature { sf = signedByte (ordAt (data, 0)),
                             minor = (ordAt (data, 1) = 1) }
            else UnknownMeta (t, data)
        | 0x7F => SequencerSpecific data
        | _ => UnknownMeta (t, data)
    end

  (* ---------------- track parse (running status) ---------------- *)
  (* parse events in s[pos, endPos) ; `running` is the last channel status. *)
  fun parseEvents (s, pos, endPos, running, acc) =
    if pos >= endPos then List.rev acc
    else
      let
        val (delta, p1) =
          case readVLQ s pos of
              SOME r => r
            | NONE => raise Malformed "truncated delta-time"
        val b0 = ordAt (s, p1)
        (* determine status byte and where data begins *)
        val (status, dataPos, running') =
          if b0 >= 0x80 then (b0, p1 + 1, if b0 < 0xF0 then SOME b0 else NONE)
          else
            (case running of
                 SOME r => (r, p1, SOME r)
               | NONE => raise Malformed "running status with no prior status")
        fun d i = ordAt (s, dataPos + i)
      in
        if status >= 0x80 andalso status <= 0xEF then
          let
            val chan = status mod 16
            val hi = status div 16
            val (ev, np) =
              case hi of
                  0x8 => (NoteOff { chan = chan, note = d 0, vel = d 1 }, dataPos + 2)
                | 0x9 => (NoteOn { chan = chan, note = d 0, vel = d 1 }, dataPos + 2)
                | 0xA => (KeyPressure { chan = chan, note = d 0, pressure = d 1 }, dataPos + 2)
                | 0xB => (ControlChange { chan = chan, ctrl = d 0, value = d 1 }, dataPos + 2)
                | 0xC => (ProgramChange { chan = chan, program = d 0 }, dataPos + 1)
                | 0xD => (ChannelPressure { chan = chan, pressure = d 0 }, dataPos + 1)
                | 0xE => (PitchBend { chan = chan, value = d 1 * 128 + d 0 }, dataPos + 2)
                | _ => raise Malformed "unreachable channel message"
          in
            parseEvents (s, np, endPos, running', (delta, ev) :: acc)
          end
        else if status = 0xF0 orelse status = 0xF7 then
          let
            val (len, lp) =
              case readVLQ s dataPos of
                  SOME r => r | NONE => raise Malformed "truncated sysex length"
            val data = String.substring (s, lp, len)
          in
            parseEvents (s, lp + len, endPos, NONE, (delta, SysEx data) :: acc)
          end
        else if status = 0xFF then
          let
            val mtype = ordAt (s, dataPos)
            val (len, lp) =
              case readVLQ s (dataPos + 1) of
                  SOME r => r | NONE => raise Malformed "truncated meta length"
            val data = String.substring (s, lp, len)
            val ev = Meta (decMeta (mtype, data))
            val acc' = (delta, ev) :: acc
          in
            (* stop early on EndOfTrack, even if chunk padding follows *)
            case decMeta (mtype, data) of
                EndOfTrack => List.rev acc'
              | _ => parseEvents (s, lp + len, endPos, NONE, acc')
          end
        else raise Malformed ("unexpected status byte " ^ Int.toString status)
      end

  (* ---------------- file parse ---------------- *)
  fun parse s =
    let
      val total = String.size s
      val () = if total < 14 then raise Malformed "file too short" else ()
      val () = if String.substring (s, 0, 4) <> "MThd"
               then raise Malformed "missing MThd" else ()
      val hlen = rdU32 (s, 4)
      val format = rdU16 (s, 8)
      val nt = rdU16 (s, 10)
      val divRaw = rdU16 (s, 12)
      val division = if divRaw >= 32768 then divRaw - 65536 else divRaw
      (* header may declare a longer length than 6; skip extra bytes *)
      val firstChunk = 8 + hlen

      fun readChunks (pos, remaining, acc) =
        if remaining <= 0 then List.rev acc
        else if pos + 8 > total then List.rev acc
        else
          let
            val id = String.substring (s, pos, 4)
            val len = rdU32 (s, pos + 4)
            val bodyStart = pos + 8
            val bodyEnd = bodyStart + len
          in
            if id = "MTrk" then
              let val tr = parseEvents (s, bodyStart, bodyEnd, NONE, [])
              in readChunks (bodyEnd, remaining - 1, tr :: acc) end
            else
              (* skip unknown chunk without consuming a track slot *)
              readChunks (bodyEnd, remaining, acc)
          end

      val tracks = readChunks (firstChunk, nt, [])
    in
      { format = format, division = division, tracks = tracks }
    end
end
