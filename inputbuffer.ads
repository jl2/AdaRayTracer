-- inputbuffer.ads

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


--	This package implements an input buffer to be used for the compiler
--	project.  It will be used primarily by the lexical analyzer.

--	I tested it really quickly with a few buffer sizes from 2 to 2000 and
--	haven't had a problem.

with Ada.Text_IO;

package InputBuffer is

	--	This is a limited private type.
	--	This means input_buffers can't be assigned to each other and 
	--	cannot be compared for equality, which doesn't make too much
	--	sense anyway.
	type Input_Buffer is limited private;

	--	Create opens a file and fills the buffers initially
	procedure Create(ib : in out Input_Buffer; fileName : in String);

	--	Destroy closes the input file.
	--	If any memory was allocated for use in the buffer,
	--	Destroy would free it.
	procedure Destroy(ib : in out Input_Buffer);
	
	function look_ahead(ib : in Input_Buffer) return Character;
	function Next_Character(ib : Input_Buffer) return character;
	
	procedure display_current_Line(ib : in Input_Buffer);
	function line( ib : in Input_Buffer) return Positive;

	--	This exception is thrown when the end of the input file is reached.
	end_of_file : exception;

private
	
	type buffer_index is new Positive range 1..100;
	num_buffers : constant :=2;
	type InputBufferArray is array (buffer_index'Range,
		1..num_buffers) of Character;

	type IBuffer is record
		ib : InputBufferArray;
		cur_Buffer : Positive := 1;
		pos : buffer_index:= buffer_index'First;
		infile : Ada.Text_IO.File_Type;
		reached_end : Boolean := true;
		ignore_new_line : boolean := false;
		last_character : buffer_index;
		last_buffer : Positive;
		line_count : Positive := 1;
		called : boolean := false;
	end record;

	type IBuffer_ptr is access all Ibuffer;

	type Input_Buffer is limited record
		b : IBuffer_ptr;
	end record;

	eof_indicator : Character := '@'; --Standard.ASCII.nul;

end InputBuffer;
	
