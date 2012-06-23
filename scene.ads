-- scene.ads

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

with Basics;
with Objects;
with grid;

package Scene is
	type Scene is private;
	type Camera is private;
	
	function num_objects(s :scene) return Natural;
	procedure read_Scene(s : in out Scene; fileName : String);
	function object_num(s : Scene; n : Positive) return Objects.Object'Class;

	function get_height(s : Scene) return Positive;
	function get_width(s : Scene) return Positive;
	procedure set_height(s : in out Scene; h : Positive);
	procedure set_width(s : in out Scene; w : Positive);
	function get_camera(s : Scene) return Camera;
	procedure set_camera(s : in out Scene; cam : in Camera);

	function get_ray(s : in Scene; i,j: Natural) return Basics.Ray;

	function current_depth(s : in Scene) return Natural;

	procedure inc_depth(s : in out scene);
	procedure dec_depth(s : in out scene);

	function num_lights(s : scene) return Natural;
	function get_light_ray(s : in scene; light_n : Positive; o : Basics.Point) return Basics.Ray;
	function get_light(s : scene; i : Positive) return Objects.light_ptr;
	
	function get_origin(cam : Camera) return Basics.Point;
	procedure set_origin(cam : in out camera; origin : in Basics.point);

	function get_direction(cam : Camera) return Basics.Vector;
	procedure set_direction(cam : in out camera; direction : in Basics.Vector);

	function Background(s : Scene) return Basics.Color;
	function sgrid(s : Scene) return grid.grid_ptr;
	
	
	
private
	type Camera is record
		ori : Basics.point;
		dir : Basics.Vector;
	end record;
	
	use Basics;
	type obj_Array is array (Positive range <>) of Objects.Object_ptr;
	type light_array is array (Positive range <>) of Objects.Light_ptr;

	type obj_array_ptr is access all obj_Array;
	type light_array_ptr is access all light_array;

	invalid_light : exception;
	invalid_object : exception;
	
	type Scene is record
		num_objects : Natural :=0;
		objs : obj_Array_ptr;
		num_lights : Natural :=0;
		lights : light_array_ptr;
		cam : Camera;

		width : Positive;
		height : Positive;
		xmin : Basics.Real := -1.0;
		ymin : Basics.Real := -1.0;
		xmax : Basics.Real :=  1.0;
		ymax : Basics.Real :=  1.0;

		vup : Vector := (0.0, 1.0,0.0);
		vright : Vector := (1.0,0.0,0.0);

		background_c : Basics.Color := (0.0,0.0,0.0);
		curDepth : Natural := 0;
		gp : grid.grid_ptr;

	end record;
end Scene;
