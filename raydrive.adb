with Ada.Text_IO;

with Basics;
use Basics;
with Scene;
use Scene;
with RayTrace;
use RayTrace;
with Image;
use Image;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Calendar;
use Ada.Calendar;

with GNAT.OS_LIB;
use GNAT.OS_LIB;

procedure raydrive is
	s : Scene.Scene;
	ist : ISect;
	img : Image_ptr;--.Image(1..160, 1..120);
	rc : Ray;

	package TIO renames Ada.Text_IO;
	package NFIO is new TIO.Float_IO(Norm_Float);
	package IIO is new TIO.Integer_IO(Natural);
	use IIO;
	use TIO;
	ct, nt : Time;
	pcolor : Color;
	
begin
	if (Argument_Count/=1) then
		Put_Line("No scen file specified.");
		OS_Exit(1);
	else
		read_scene(s, Argument(1));
	end if;
	
	
	TIO.Put_Line("Now computing image...");
	ct := Clock;
	img := new Image.Image(1..get_width(s), 1..get_height(s));

	for j in 1..Height(img) loop
		for i in 1..Width(img) loop
			rc := get_ray(s, i, j);
			ist := trace(s, rc);

			if ( ist.hit) then
				shade(s, ist, pcolor);
				Set_Pixel(img, i,j,pcolor);
			else
				Set_pixel(img, i,j, Background(s));
			end if;
		end loop;
	end loop;
	nt := Clock;
	Put_Line("Took:"&Duration'Image(nt-ct)&" seconds to generate image.");
	TIO.Put_Line("Now writing to ""testing.ppm""");
	Write_Image(img, "testing.ppm");
	TIO.Put_Line("Now exiting raydrive");
end raydrive;

