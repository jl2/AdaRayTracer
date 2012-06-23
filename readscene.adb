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
