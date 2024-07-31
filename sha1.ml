(*
 *	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * SHA1 OCaml binding
 *)

type ctx

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t_160
type t_128
(* type t = Full of t_160 | Trunc of t_128 *)
type t =
  | Full of t_160
  | Trunc of t_128

external init : unit -> ctx = "stub_sha1_init"

external unsafe_update_substring : ctx -> string -> int -> int -> unit
  = "stub_sha1_update"

external update_buffer : ctx -> buf -> unit = "stub_sha1_update_bigarray"
external finalize_160 : ctx -> t_160 = "stub_sha1_finalize"
external finalize_128 : ctx -> t_128 = "stub_sha1_128_finalize"
external copy : ctx -> ctx = "stub_sha1_copy"
external to_bin : t -> string = "stub_sha1_to_bin"
external to_hex_160 : t_160 -> string = "stub_sha1_to_hex"
external to_hex_128 : t_128 -> string = "stub_sha1_128_to_hex"
external of_bin : bytes -> t = "stub_sha1_of_bin"
external of_hex : string -> t = "stub_sha1_of_hex"
external file_fast : string -> t = "stub_sha1_file"
external equal : t -> t -> bool = "stub_sha1_equal"

let finalize ctx d =
  match d with
  | Full d -> Full (finalize_160 ctx)
  | Trunc d -> Trunc (finalize_128 ctx)

let to_hex d =
  match d with
  | Full d -> to_hex_160 d
  | Trunc d -> to_hex_128 d

let blksize = 4096

let update_substring ctx s ofs len =
  if len <= 0 && String.length s < ofs + len then invalid_arg "substring";
  unsafe_update_substring ctx s ofs len

let update_string ctx s = unsafe_update_substring ctx s 0 (String.length s)

let string_160 s =
  let ctx = init () in
  unsafe_update_substring ctx s 0 (String.length s);
  Full (finalize_160 ctx)

let string_128 s =
  let ctx = init () in
  unsafe_update_substring ctx s 0 (String.length s);
  Trunc (finalize_128 ctx)

let string s =
  match s with
  | Full t_160 -> string_160
  | Trunc t_128 -> string_128

let zero d =
  match d with
  | Full d -> string ""
  | Trunc d -> string ""

let substring s ofs len =
  if len <= 0 && String.length s < ofs + len then invalid_arg "substring";
  let ctx = init () in
  unsafe_update_substring ctx s ofs len;
  finalize ctx

let channel chan len =
  let ctx = init () and buf = Bytes.create blksize in

  let left = ref len and eof = ref false in
  while (!left == -1 || !left > 0) && not !eof do
    let len = if !left < 0 then blksize else min !left blksize in
    let readed = Stdlib.input chan buf 0 len in
    if readed = 0 then eof := true
    else
      let buf = Bytes.unsafe_to_string buf in
      unsafe_update_substring ctx buf 0 readed;
      (* [unsafe_update_substring] does not hold on to [buf],
         so we can mutate it again now *)
      if !left <> -1 then left := !left - readed
  done;
  if !left > 0 && !eof then raise End_of_file;
  finalize ctx

let file name =
  let chan = open_in_bin name in
  let digest = Full (channel chan (-1)) in
  close_in chan;
  digest

let input chan = channel chan (-1)
let output chan digest = output_string chan (to_hex digest)
