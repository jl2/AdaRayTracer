-- gridtest.adb

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

with Ada.Text_Io;
use Ada.Text_IO;

with Basics;
use Basics;

with grid;
use grid;


procedure gridtest is
	gp : grid_ptr;
begin
	Put_Line("Creating a 1024x1024x1024 grid");
	gp := create_grid(512, 512, 512, -8.0, -8.0,-8.0,8.0,8.0,8.0);
	Put_Line("Grid successfully created.");
end gridtest;
	
