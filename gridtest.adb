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
	
