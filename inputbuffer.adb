-- inputbuffer.adb

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
with Ada.Unchecked_Deallocation;

--	type Input_Buffer is limited record
--		ib : InputBufferArray;
--		cur_Buffer : Positive := 1;
--		pos : buffer_index:= buffer_index'First;
--		infile : Ada.Text_IO.File_Type;
--	end record;

package body inputBuffer is
	package TIO renames Ada.Text_IO;

	--	This reads in buffer 'which'.
	--	It is private to this package, and therefore can not be called anywhere else but this file.
	procedure Read_Buffer(ib : in out IBuffer; which : Positive) is
	begin
		if (not ib.reached_end) then
			
		read_loop:
		for i in InputBufferArray'Range(1) loop 
			if (TIO.End_Of_File(ib.infile)) then
				ib.ib(i,which) := eof_indicator;
				ib.reached_end := true;
				ib.last_character := i;
				ib.last_buffer := which;
				exit read_loop;
			elsif (TIO.end_of_Line(ib.infile) and not ib.ignore_new_line) then
				ib.ib(i, which) := Standard.ASCII.lf;
				TIO.Skip_Line(ib.infile);
			else
				TIO.Get(ib.infile, ib.ib(i, which));
			end if;
		end loop read_loop;
		end if;
	end Read_Buffer;
	
	--	Create opens a file and fills the buffers initially
	procedure Create(ib : in out Input_Buffer; fileName : in String) is
	begin
		ib.b := new IBuffer;
		TIO.Open(ib.b.all.infile, TIO.in_file, fileName);

		ib.b.reached_end := false;
		--	Now read in the first two buffers
		Read_Buffer(ib.b.all, 1);
		if (not ib.b.all.reached_end) then
			Read_Buffer(ib.b.all, 2);
		end if;
	end Create;

	--	Destroy closes the input file.
	--	If any memory was allocated for use in the buffer,
	--	Destroy would free it.
	procedure Destroy(ib : in out Input_Buffer) is
		procedure Free is new Ada.Unchecked_Deallocation(IBuffer, IBuffer_ptr);
	begin
		TIO.Close(ib.b.infile);
		Free(ib.b);
	end Destroy;

	--	Next_Character returns the next character in the input buffer.
	--	It can be used for look ahead.
	--	It is implemented as a procedure to simplify the interface.
	function look_ahead(ib : Input_Buffer) return Character is
		nc : Character;
	begin
		nc := ib.b.ib(ib.b.pos,ib.b.cur_buffer);
		return nc;
	end look_ahead;

	--	Cur_Character this has to be a procedure because ib has to be 
	--	updated.  I made Next_Character a procedure also, so they are
	--	called with the same parameters.
	function Next_Character(ib : Input_Buffer) return Character is
		nc : Character;
	begin
		if (not ib.b.called) then
			--display_current_line(ib);
			ib.b.called := true;
		end if;
		nc := ib.b.ib(ib.b.pos, ib.b.cur_buffer);
		if ( 	nc=eof_indicator and 
				ib.b.reached_end and 
				(ib.b.last_character = (ib.b.pos) and ib.b.last_buffer = ib.b.cur_buffer)) then
			
			raise InputBuffer.end_of_File;
		end if;

		if (ib.b.pos = InputBufferArray'Last(1)) then
			ib.b.pos := 1;
			case ib.b.cur_buffer is
				when 1 =>
					ib.b.cur_buffer :=2;
					Read_Buffer(ib.b.all, 1);
				when 2 =>
					ib.b.cur_buffer :=1;
					Read_Buffer(ib.b.all, 2);
				when others => null;
			end case;
		else
			ib.b.pos := ib.b.pos+1;
		end if;
		if (nc = ASCII.lf) then
			ib.b.line_count := ib.b.line_count+1;
			ib.b.called := false;
		end if;

		return nc;
	end Next_Character;

	function line(ib : Input_buffer) return Positive is
	begin
		return ib.b.line_count;
	end line;
	
	procedure display_current_line(ib : in Input_buffer) is
		j : buffer_index;
		cb : Positive := ib.b.cur_buffer;
	begin
		TIO.New_Line;
		TIO.Put("Line"&Natural'Image(line(ib)) & " : ");
		j := ib.b.pos;
		while (ib.b.ib(j, cb)/=ASCII.lf) loop
			TIO.Put(ib.b.ib(j, cb));
			if ((ib.b.reached_end) and
				 ((cb = ib.b.last_buffer) and 
				  (j = ib.b.last_character-1))) then exit;
			else
				if (j = buffer_index'Last) then
					j:=buffer_index'First;
					if (cb = 1) then
						cb:=2;
					else
						cb:=1;
					end if;
				else
					j := j+1;
				end if;
			end if;
		end loop;
		TIO.New_Line;
	end display_current_line;
	
end InputBuffer;
