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
