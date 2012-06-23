-- scenelexer.adb

-- Copyright (c) 2012, Jeremiah LaRocco jeremiah.larocco@gmail.com

-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.

-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Text_IO;

package scenelexer is
	type lexer is record
		inf : Ada.Text_IO.File_Type;
	end record;
	--type token is (invalid, camera, sphere, poly, lbrace, rbrace, position, 
	--	direction, up, number, lbrack, rbrack, IOR, Ka, Kd, Ks, Kt, Kr, quote, comma); 
	procedure initialize(l : lexer; fn : String) is
	begin
		Open(l.inf, IN_FILE, fn);
	end initialize;
	
	procedure lex(l : lexer; t : out token; lexeme : Ada.Strings.Unbounded) is
	begin
		
end scenelexer;
