with Ada.Text_IO;
with Basics;
use Basics;

package body Image is
	package TIO renames Ada.Text_IO;
	package IIO is new TIO.Integer_IO(Natural);
	package RIO is new TIO.Float_IO(Real);
	
	procedure Set_Pixel(img : in out Image_ptr; x,y : in Natural; c : in Basics.Color) is
	begin
		if ((not ((y>Height(img)) or (x > Width(img)))) and
			(not ((y<1) or (x < 1)))) then
			img(x, y) := c;
		end if;
	end Set_Pixel;
	
	function Get_Pixel(img : Image_ptr; x, y : Natural) return Basics.Color is
	begin
		if ((not ((y>Height(img)) or (x > Width(img)))) and
			(not ((y<1) or (x < 1)))) then
			
			return img(x,y);
		else
			return (0.0,0.0,0.0);
		end if;
	end Get_Pixel;

	function Height(img : Image_ptr) return Positive is
	begin
		return img'Last(2);
	end Height;

	function Width(img : Image_ptr) return Positive is
	begin
		return img'Last(1);
	end Width;

	procedure Write_image(img : Image_ptr; fileName : String) is
		outf : TIO.File_Type;
		tc : Color;
		line_len : Natural :=0;
		r, g, b : Real := 0.0;
	begin
		TIO.Create(outf, TIO.Out_File, fileName);

		TIO.Put_Line(outf, "P3 ");
		IIO.Put(outf, Width(img), width=>0);
		TIO.Put(outf, " ");
		IIO.Put(outf, Height(img));
		TIO.Put_Line(outf, " 255");
		
		for j in 1..Height(img) loop
			for i in 1..Width(img) loop
				tc :=Get_Pixel(img, i,j);
				line_len := line_len+12;
				if (tc.red>1.0) then
					r := 1.0;
				elsif (tc.red<0.0) then
					r := 0.0;
				else
					r:= tc.red;
				end if;
				
				if (tc.green>1.0) then
					g := 1.0;
				elsif (tc.green<0.0) then
					g := 0.0;
				else
					g:= tc.green;
				end if;
				
				if (tc.blue>1.0) then
					b := 1.0;
				elsif (tc.blue<0.0) then
					b := 0.0;
				else
					b:= tc.blue;
				end if;
				IIO.Put(outf, Natural(255.0*r), width=>4);
				IIO.Put(outf, Natural(255.0*g), width=>4);
				IIO.Put(outf, Natural(255.0*b), width=>4);
				if (line_len = 60) then
					TIO.New_Line(outf);
					line_len := 0;
				end if;
			end loop;
		end loop;
	end Write_image;
end Image;

