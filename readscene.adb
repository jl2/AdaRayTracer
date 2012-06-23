-- readscene.adb

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

with Ada.text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with scene;
use scene;
with objects;
use objects;
with basics;
use basics;
with InputBuffer;
use InputBuffer;

function read_scene( fn : String) return Scene is
	inf : File_type;
	ib : InputBuffer;
	curWord : Unbounded_String;
	
	function get_word(ib : InputBuffer) return Unbounded_String is
		cc : Character;
		la : Character;
		wrd : Unbounded_String := "";
	begin
		loop
			cc := Next_character(ib);
			exit when ((cc /=' ') and (cc/=ASCII.HT) and (cc/=ASCII.LF) and (cc/=ASCII.CR));
		end loop;
		loop
			wrd := wrd & cc;
			cc := Next_Character(ib);
			la := look_ahead(ib);
			exit when ((la /=' ') and (la/=ASCII.HT) and (la/=ASCII.LF) and (la/=ASCII.CR));
		end loop;
		return curWord
	end get_word;
		
begin
	Create(ib, fn);
	declare
	begin
		loop
			Put_Line(To_String(get_word(ib)));
		end loop;
	exception
		when end_of_file => null;
		when others => raise;
	end
	Destroy(ib);
	return null;
end read_scene;
