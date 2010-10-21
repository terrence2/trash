(*
 * Copyright (c) 2010, Terrence Cole.
 *
 * This file is part of Trash, Terrence's Re-Bourne Again SHell.
 *
 * Trash is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Trash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Trash.  If not, see <http://www.gnu.org/licenses/>.
 *)

let setup () =
	let termattrs = Unix.tcgetattr Unix.stdin in
		(* print initial attrs *)
		print_endline ("ICanon: " ^ (string_of_bool termattrs.Unix.c_icanon));
		print_endline ("ISig:   " ^ (string_of_bool termattrs.Unix.c_isig));
		print_endline ("Echo:   " ^ (string_of_bool termattrs.Unix.c_echo));
		print_endline ("EchoE:  " ^ (string_of_bool termattrs.Unix.c_echoe));
		print_endline ("EchoK:  " ^ (string_of_bool termattrs.Unix.c_echok));
		print_endline ("EchoNL: " ^ (string_of_bool termattrs.Unix.c_echonl));
		
		(* setup terminal attrs *)
		termattrs.Unix.c_icanon <- false;
		termattrs.Unix.c_isig <- true;
		termattrs.Unix.c_echo <- false;
		termattrs.Unix.c_echoe <- false;
		termattrs.Unix.c_echok <- false;
		termattrs.Unix.c_echonl <- false;

		(* re-apply to the terminal *)
		Unix.tcsetattr Unix.stdin Unix.TCSANOW termattrs
	;;


let terminate () = 
	ignore ();;


let emit_prompt () =
	print_string ("trash> ");;


let emit_clearline () =
	print_string ("\027[1G");  (* cursor position 0 horizontal *) 
	print_string ("\027[0K");; (* clear full line *)


let show_line (buf) =
	emit_clearline();
	emit_prompt ();
	print_string (buf);
	flush (stdout);;


let interact (string) =
	show_line ("");
	let buf = ref "" and have_line = ref false in
		while not !have_line do
			(* read a char and update buffer *)
			let next = input_char (stdin) in
			match next with
			| '\n' -> have_line := true
			| '\004' -> buf := ""
			| '\127' -> buf := String.sub !buf 0 (String.length !buf - 1)
			| x -> 
				let next_as_str = String.make 1 next in
					(*print_string("Got: " ^ next_as_str);
					  print_newline();
					 *)
					buf := !buf ^ next_as_str;
			
			(* show the current line *)
			show_line (!buf);
		done;
		
		print_newline();
		(!buf);;
	

