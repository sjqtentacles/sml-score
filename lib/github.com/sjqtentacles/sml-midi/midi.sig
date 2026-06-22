(* midi.sig

   Standard MIDI File (SMF) reading and writing in pure Standard ML, for
   format 0 and format 1 files, plus a full channel-voice / meta event model
   and the canonical SMF variable-length quantity (VLQ) codec.

   Bytes are represented as `string` (each character is one byte, 0..255).
   Multi-byte integers are big-endian, as in the SMF specification.

   A track is a list of (delta-time, event) pairs, where the delta-time is the
   number of ticks since the previous event in the same track (the standard
   SMF representation). The number of ticks per quarter note is the file's
   `division`. Parsing transparently expands running status; writing always
   emits an explicit status byte (never running status), so a parse/write
   round-trip is structurally stable.

   No FFI, threads, clock or randomness: identical inputs always produce
   identical outputs (byte-for-byte) under MLton and Poly/ML. *)

signature MIDI =
sig
  exception Malformed of string

  (* ---- variable-length quantities (canonical SMF VLQ) ---- *)
  (* writeVLQ n: the canonical big-endian 7-bit encoding of n >= 0. *)
  val writeVLQ : int -> string
  (* readVLQ s i: decode a VLQ starting at index i, returning the value and
     the index just past it, or NONE if the bytes are truncated. *)
  val readVLQ  : string -> int -> (int * int) option

  (* ---- meta events ---- *)
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
    | SetTempo of int                 (* microseconds per quarter note *)
    | SMPTEOffset of string           (* raw payload bytes *)
    | TimeSignature of { num : int, den : int, clocks : int, notes32 : int }
    | KeySignature of { sf : int, minor : bool }   (* sf: -7..+7 *)
    | SequencerSpecific of string
    | UnknownMeta of int * string     (* type byte, raw payload *)

  (* ---- channel-voice / system events ---- *)
  datatype event =
      NoteOff        of { chan : int, note : int, vel : int }
    | NoteOn         of { chan : int, note : int, vel : int }
    | KeyPressure    of { chan : int, note : int, pressure : int }
    | ControlChange  of { chan : int, ctrl : int, value : int }
    | ProgramChange  of { chan : int, program : int }
    | ChannelPressure of { chan : int, pressure : int }
    | PitchBend      of { chan : int, value : int }   (* 0..16383, center 8192 *)
    | SysEx          of string
    | Meta           of meta

  type track = (int * event) list     (* (delta-time in ticks, event) *)
  type smf = { format : int, division : int, tracks : track list }

  (* ---- read / write whole files ---- *)
  (* parse a complete SMF (MThd + MTrk chunks); raises Malformed on bad input. *)
  val parse : string -> smf
  (* serialize an SMF to bytes (MThd header + one MTrk per track). *)
  val write : smf -> string

  (* ---- helpers ---- *)
  val toHex   : string -> string      (* space-separated uppercase hex bytes *)
  val ntracks : smf -> int
end
