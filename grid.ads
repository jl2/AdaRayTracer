-- grid.ads

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

with objects;
with genlist;
with Basics;

package grid is
		
	package OList is new GenList(Objects.object_ptr, Objects."=");
	package LList is new GenList(Objects.light_ptr, Objects."=");
	
	type cell is record
		obj_list : OList.List;
		light_list : LList.List;
	end record;
	
	type grid_arr is array( Natural range <>, Natural range <>, natural range <>) of cell;
	
	type grid_arr_ptr is access all grid_arr;
	
	type grid is record
		gp : grid_arr_ptr;
		
		xmin, xmax : Basics.Real;
		ymin, ymax : Basics.Real;
		zmin, zmax : Basics.Real;
		
		xdiff : Basics.Real;
		ydiff : Basics.real;
		zdiff : Basics.Real;
	end record;

	type grid_ptr is access all grid;

	function create_grid(xs, ys, zs : Natural;
				xmin, ymin, zmin, xmax, ymax, zmax: Basics.Real) 
				return grid_ptr;
	
	procedure add( tg : grid_ptr; sph : Objects.sphere_ptr);
	procedure add( tg : grid_ptr; tri : Objects.triangle_ptr);

	procedure cell_of(tg : in grid_ptr; x,y,z : in Basics.Real; 
							xn, yn, zn : out Natural; in_grid : out boolean);
	function cell_at(tg : in grid_ptr; xn,yn,zn : Natural) return cell;
end grid;
