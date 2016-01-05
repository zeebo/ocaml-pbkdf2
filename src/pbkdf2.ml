(*
 * Copyright (c) 2016 Jeff Wendling <leterip@gmail.com>
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
 *)

module type PRF = sig
  val hLen : int
  val hash : key:string -> data:string -> string
end

module type S = sig
  val create_exn : password:string -> salt:string -> iterations:int -> out:int -> string
  val create     : password:string -> salt:string -> iterations:int -> out:int -> [ `Ok of string | `Error of string ]
end

module Make (P : PRF) = struct
  (* TODO(jeff): there has to be a better way to exponentiate integers than
   * inlining a exponentiation function every time *)

  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
      let b = pow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a)

  (* TODO(jeff): this is probably dumb because it overflows because int is in
   * at least some cases 32 bits. *)
  let max_out = ((pow 2 32) - 1) * P.hLen

  let encode_int i =
    let rec loop i accum =
      let rem = char_of_int (i mod 256) in
      Buffer.add_char accum rem;
      if i > 256 then
        loop (i / 256) accum
    in
    let buf = Buffer.create 4 in
    loop i buf;
    Buffer.contents buf

  let xor_strings a b = 
    let xor_from_b index a_char =
      char_of_int ((int_of_char b.[index]) lxor (int_of_char a_char))
    in String.mapi xor_from_b a

  let create_exn ~password ~salt ~iterations ~out =
    if out > max_out then
      invalid_arg "derived key too long"
    else
      let l = (out + P.hLen - 1) / P.hLen in
      let r = out - (l - 1) * P.hLen in
      let f = fun c i ->
        let first_data = salt ^ (encode_int 0) in
        let u = ref (P.hash ~key:password ~data:first_data) in
        for counter = 1 to c - 1 do
          u := xor_strings !u (P.hash ~key:password ~data:!u)
        done;
        !u
      in
      let buf = Buffer.create out in
      for counter = 1 to l - 1 do
        Buffer.add_string buf (f iterations counter)
      done;
      let last = f iterations l in
      Buffer.add_string buf (String.sub last 0 r);
      Buffer.contents buf

  let create ~password ~salt ~iterations ~out =
    try
      `Ok (create_exn ~password:password ~salt:salt ~iterations:iterations ~out:out)
    with
    | _ -> `Error "prolly a way to turn an exception into a string"

end